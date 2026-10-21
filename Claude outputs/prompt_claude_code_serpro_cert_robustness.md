# Harden the SERPRO/GlobalSign cert-chain fix: dynamic intermediate resolution, expiry check, and a real end-user exposure check

Branch: new branch from `master` (this is a hardening pass on top of the already-merged comex/baci TLS fix, not a continuation of any open PR) — name it something like `harden-serpro-trust-chain`.

## Context (already established, don't re-derive — verify, don't re-investigate from scratch)

- `.github/actions/trust-serpro-intermediate/action.yml` (merged to master) fixes an incomplete TLS chain on `balanca.mdic.gov.br`: Linux (GitHub Actions runners) doesn't auto-fetch the missing intermediate cert the way Windows does, causing `unable to get local issuer certificate`. The action downloads the intermediate from a **hardcoded** URL (`http://secure.globalsign.com/cacert/acserproar46ovtlsca2025.crt`), checks its subject matches a **hardcoded** expected string (`"AC SERPRO AR46 OV TLS CA 2025"`), verifies it cryptographically against the system's trusted root store (`openssl verify -CAfile /etc/ssl/certs/ca-certificates.crt`), then installs it into the CI runner's OS trust store. Never disables TLS verification — this property must be preserved in everything below.
- It's wired into 3 workflows that call `balanca.mdic.gov.br`: `update-manifest.yaml`, `check-load-br-trade.yaml`, `check-dz-amazonia.yaml` (scoped to the `load_br_trade` job in the 22-job matrix there).
- Two distinct certs in this chain, don't conflate them: the **leaf** cert (the site's own cert for `balanca.mdic.gov.br`) expires 2026-10-22 per a code comment in `resolve_comex.R` — not yet confirmed whether it renews under the same intermediate or a new one. The **intermediate** (`AC SERPRO AR46 OV TLS CA 2025`) is the one hardcoded in `action.yml`; its own validity dates have never been checked. Root is believed to be a GlobalSign root (reasoning: GlobalSign hosts the download at its own domain, and `openssl verify` succeeds using only the system root bundle with no extra chain file — meaning whatever signs this intermediate is itself a root already in that bundle) — not yet confirmed by actually reading the cert's Issuer field (network egress from the planning session is blocked for `globalsign.com`; you should have normal access).

## Three things to do, in order of priority

### 1. Antonio's question — does this fix actually help real end users, or only CI? (highest priority, answer with evidence, not reasoning)

Antonio asked (paraphrased): does the comex/baci Actions fix run every time someone loads the package, or is it internal-only to CI/the resolver — in which case it doesn't solve anything for an actual end user?

My working hypothesis, which needs real verification, not just logical argument: `action.yml` is a **GitHub Actions composite action** — it can only execute inside a GitHub Actions workflow run, on that ephemeral runner, and only patches that runner's OS trust store for the duration of the job. It is not part of the installed R package in any way (not in `R/`, not in any install/`.onLoad` hook, not shipped as a resource). So a real end user who does `install.packages()`/`remotes::install_github()` and then calls `load_br_trade()` or `load_baci()` on their own machine gets **none** of this protection — only our own CI jobs (the resolver, the scheduled checks) are fixed.

**Verify this empirically, don't just confirm my reasoning:**
- Grep the whole package source (`R/`, `DESCRIPTION`, any install scripts, `.onLoad`/`.onAttach`) to confirm there's no other mechanism that installs or references this intermediate cert outside of `.github/actions/`.
- Reproduce the actual failure a real end user would hit: spin up a clean, representative Linux environment (e.g. a plain `rocker/r-ver` Docker container, or equivalent — something that has never run our composite action and has whatever `ca-certificates` package ships by default, unmodified) and attempt a real `load_br_trade()` (or whatever's the lowest-level call that hits `balanca.mdic.gov.br`) against the live host. Confirm whether it actually fails with `unable to get local issuer certificate` there.
- Also check the reverse, don't assume: is it possible a stock Linux distro's `ca-certificates` package already happens to include this GlobalSign-issued SERPRO intermediate (some distros bundle a fairly large set)? Check what's actually in a fresh Ubuntu/Debian `ca-certificates` install before concluding end users are broken.
- Report back clearly, with the actual command output as evidence: are real end users on Linux currently broken for `load_br_trade()`/`load_baci()`, or not?

**If end users are confirmed broken**, this needs an R-level fix (not just the CI-level one), since a GitHub Actions composite action can never run on a user's own machine. Implement one, minimal and targeted — scoped to requests hitting `balanca.mdic.gov.br` only, not a global TLS change. The likely mechanism: pass a CA bundle to the specific `curl`/`httr` call(s) in `R/comex.R`/`R/baci.R` (via `curl::new_handle(cainfo = ...)` or equivalent, following whatever pattern `external_download()`/`dataset_url()` already use) that includes the verified intermediate. Decide and justify, don't silently pick:
  - (a) ship the intermediate as a static PEM resource bundled with the package (simple, no extra network round-trip or dependency on GlobalSign's URL staying up, but needs a maintainer to update it manually if SERPRO ever rotates the intermediate — same maintenance burden as today's hardcoded CI version), vs.
  - (b) fetch+verify it dynamically at runtime the same way the hardened CI action will (see part 2 below), optionally caching it locns, vs.
  - (c) something else you find cleaner once you're actually in the code.
  Keep the same safety invariant as the CI version: subject/signature verification against the trusted root store before use, never `-k`/`ssl_verifypeer=FALSE`/equivalent.

**If end users are confirmed NOT broken** (e.g. the intermediate turns out to already be in standard distro bundles, or there's some other mechanism I'm missing), just report that clearly with the evidence — no R-level change needed, and say so plainly to relay back to Antonio.

### 2. Make `trust-serpro-intermediate/action.yml` robust to a future intermediate rotation

Right now it breaks (silently-ish — the early steps succeed since the *old* intermediate is still itself valid, only the final informational `curl --head` check and, later, real downloads would start failing with `unable to get local issuer certificate` again) if SERPRO ever rotates the intermediate itself (as opposed to just renewing the leaf under the same intermediate, which is harmless).

Implement the dynamic version: instead of a hardcoded download URL, read the issuer's AIA (Authority Information Access) URL directly from the real leaf certificate currently presented by `balanca.mdic.gov.br` (e.g. via `openssl s_client -connect balanca.mdic.gov.br:443 -showcerts` to get the leaf, then `openssl x509 -noout -text` and parse the `CA Issuers - URI:` line from the Authority Information Access extension) — exactly what a browser does automatically. Download the intermediate from *that* URL rather than a hardcoded one.

Keep the cryptographic verification exactly as strict as today (signature against the system trusted root store) — that's the real security boundary and must not weaken. Decide and justify what to do with the hardcoded subject-string check (`"AC SERPRO AR46 OV TLS CA 2025"`): keep it as a hard failure (safe default, but means a legitimate future rotation still requires a manual update to this one string, defeating part of the point), turn it into a non-fatal warning (logs if the subject changed from what was last seen, but proceeds since the cryptographic verification is what actually matters), or something else — your call, with reasoning.

### 3. Check the intermediate's own expiry

Get the real `NotBefore`/`NotAfter` dates of `AC SERPRO AR46 OV TLS CA 2025` (and, while you're at it, confirm the Issuer field — is it really a GlobalSign root, settling the open question above). Report whether a rotation looks imminent or distant, so we know whether part 2 is urgent or just good hygiene. Also report what happens on 2026-10-22 if you can determine it ahead of time (e.g. does the leaf cert, when it renews, show as already reissued under a staging/parallel cert you can fetch now) — don't spend much time on this if it's not easily observable, a live rough answer is enough.

## Rules, as always

- Do NOT commit or push anything — Philippe commits himself.
- Flag every judgment call above (the R-level fix mechanism, the subject-check fallback behavior, commit granularity) with your reasoning rather than deciding silently.
- Flag anything you find outside this scope rather than fixing it inline.
- Full test suite, and a real test of whatever you change (the CI action logic can be tested by simulating a GitHub Actions runner locally if practical, or at minimum dry-run the openssl/curl steps standalone against the live host; the R-level fix, if any, needs a real end-to-end `load_br_trade()`/`load_baci()` call).
- Give a short, clear report: what you verified empirically (with evidence) for Antonio's question, what you changed and why, proposed commit message(s) (your call on whether this is one commit or split, e.g. CI hardening vs R-level end-user fix — justify either way), and anything still open.
