#!/usr/bin/env bash
# Regenerates the TLS chain bundled for R users (inst/extdata/tls/balanca-serpro-chain.pem,
# read by R/balanca_tls.R) from the certificates trust-intermediate.sh has just found and
# verified, so that a rotation of the host's intermediate CA reaches the package as a pull
# request instead of as a warning nobody reads. Run by trust-intermediate.sh, only when the
# calling workflow asked for it (action.yml: refresh-bundled-chain); read that file's header.
#
# Usage: refresh-bundled-chain.sh <bundle.pem> <leaf.pem> <host> <issuer.pem>...
#   <bundle.pem>     the file to rewrite, in place
#   <leaf.pem>       the certificate the server presents today
#   <host>           the host name that certificate is for
#   <issuer.pem>...  the certificates downloaded from the AIA URLs, the leaf's issuer first
#                    (trust-intermediate.sh has already verified leaf <- these <- a system root)
#
# What the new bundle holds: the self-signed root(s) of the old one, kept as they are, then the
# issuer certificate(s) given here, replacing the old intermediate(s). Before anything is written
# it must verify the leaf BY ITSELF -- the bundle as the only trust store, the way R/balanca_tls.R
# uses it -- including host name and serverAuth purpose. If it cannot (a rotation that also
# changed the root needs a person: the new root has to be chosen), nothing is written.
#
# The header comment of the file is regenerated from the certificates themselves (name, validity,
# SHA-256, where each came from), so it cannot disagree with them.
#
# Environment (optional):
#   REFRESH_BODY   path to write the pull request description to  [none]
#   REFRESH_DATE   date written into the header                    [today, UTC]
#
# Exit status: 0 rewritten; 2 nothing to change (already exactly these certificates);
#              3 refused, nothing written (the regenerated bundle does not verify the leaf alone).
set -euo pipefail

[ "$#" -ge 4 ] || {
  echo "usage: $0 <bundle.pem> <leaf.pem> <host> <issuer.pem>..." >&2
  exit 64
}
bundle="$1"
leaf="$2"
host="$3"
shift 3
issuers=("$@")
today="${REFRESH_DATE:-$(date -u +%F)}"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

fingerprint() { openssl x509 -in "$1" -noout -fingerprint -sha256 | cut -d= -f2; }
is_self_signed() { [ "$(openssl x509 -in "$1" -noout -subject_hash)" = "$(openssl x509 -in "$1" -noout -issuer_hash)" ]; }
# A name field ("commonName", "organizationName"...) of the subject or the issuer.
name_of() { openssl x509 -in "$1" -noout "-$2" -nameopt multiline,utf8 | sed -n "s/^ *$3 *= *//p" | head -n 1; }
cn() { name_of "$1" subject commonName; }
issuer_cn() { name_of "$1" issuer commonName; }
dates() { printf 'valid from %s to %s' "$(openssl x509 -in "$1" -noout -startdate | cut -d= -f2)" "$(openssl x509 -in "$1" -noout -enddate | cut -d= -f2)"; }
aia_of() { openssl x509 -in "$1" -noout -text | sed -n 's/^ *CA Issuers - URI://p' | tr -d '\r' | head -n 1; }
basic_constraints() { openssl x509 -in "$1" -noout -ext basicConstraints 2>/dev/null | sed -n '2s/^ *//p'; }

# Splits a PEM file into one file per certificate: <dir>/b1.pem, b2.pem, ...
split_pem() {
  mkdir -p "$2"
  awk -v dir="$2" '/-----BEGIN CERTIFICATE-----/{n++; on=1} on{print > (dir "/b" n ".pem")} /-----END CERTIFICATE-----/{on=0}' "$1"
}

split_pem "$bundle" "$work/old"
old_certs=()
for f in "$work"/old/b*.pem; do
  if [ -s "$f" ]; then old_certs+=("$f"); fi
done
[ "${#old_certs[@]}" -gt 0 ] || {
  echo "no certificate found in $bundle" >&2
  exit 64
}

# The new bundle: the old self-signed root(s) as they are, then the issuers found.
kept_roots=()
for f in "${old_certs[@]}"; do
  if is_self_signed "$f"; then kept_roots+=("$f"); fi
done
new_certs=("${kept_roots[@]+"${kept_roots[@]}"}")
for f in "${issuers[@]}"; do
  dup=0
  for r in "${kept_roots[@]+"${kept_roots[@]}"}"; do
    [ "$(fingerprint "$r")" = "$(fingerprint "$f")" ] && dup=1
  done
  if [ "$dup" = 0 ]; then new_certs+=("$f"); fi
done

fps() { for f in "$@"; do fingerprint "$f"; done | sort; }
if [ "$(fps "${old_certs[@]}")" = "$(fps "${new_certs[@]}")" ]; then
  echo "$bundle already holds exactly these certificates; nothing to change."
  exit 2
fi

# Must verify the leaf with the bundle as the ONLY trust store (what R/balanca_tls.R does).
: >"$work/new_body.pem"
for f in "${new_certs[@]}"; do cat "$f" >>"$work/new_body.pem"; done
if ! out="$(openssl verify -CAfile "$work/new_body.pem" -no-CApath -purpose sslserver -verify_hostname "$host" "$leaf" 2>&1)"; then
  echo "$out"
  echo "the regenerated bundle does not verify $host by itself (most likely the chain leads to a root the old bundle did not carry; or the certificate is not for $host, or has expired) -- not written; this needs a person." >&2
  exit 3
fi
verify_line="the leaf verifies against the new bundle alone (signatures, dates, serverAuth purpose, host name)"

# ---- the header: written from the certificates, so it cannot disagree with them
{
  cat <<'EOF'
# Certificates used ONLY as a fallback when a system cannot verify https://balanca.mdic.gov.br
# (R/balanca_tls.R). That server sends its leaf certificate but not the intermediate CA that
# issued it, and OpenSSL-based systems (Linux) do not fetch missing intermediates themselves.
# Public CA certificates only. They are only ever given to libcurl as the trust anchors for
# requests to that one host, after the system's own trust store has failed -- see R/balanca_tls.R.
EOF
  echo "# Regenerated $today by .github/actions/trust-serpro-intermediate (refresh-bundled-chain.sh) from the"
  echo "# certificate chain the live host presented, after checking it against the runner's own roots."
  echo "#"
  n=0
  prev="$leaf"
  for f in "${new_certs[@]}"; do
    n=$((n + 1))
    if is_self_signed "$f"; then
      echo "# [$n] $(cn "$f") -- self-signed root"
      source_line="kept from the previous bundle"
    else
      echo "# [$n] $(cn "$f") -- issued by $(issuer_cn "$f")"
      source_line="$(aia_of "$prev")"
      [ -n "$source_line" ] || source_line="the AIA 'CA Issuers' URL of the previous certificate in the chain"
      prev="$f"
    fi
    echo "#     $(dates "$f")"
    echo "#     SHA-256 $(fingerprint "$f")"
    bc="$(basic_constraints "$f")"
    [ -z "$bc" ] || echo "#     $bc"
    echo "#     source: $source_line"
  done
  echo "#"
  echo "# When SERPRO (or whoever the host uses) rotates the intermediate, the weekly update-manifest.yaml"
  echo "# run regenerates this file and opens a pull request. Review it: compare the SHA-256 above with"
  echo "# the CA's own publication. tests/testthat/test-balanca-tls.R fails before [2] expires."
  cat "$work/new_body.pem"
} >"$work/new.pem"

cp "$work/new.pem" "$bundle"
echo "rewrote $bundle: $(printf '%s ' "${new_certs[@]}" | wc -w | tr -d ' ') certificate(s); $verify_line"

# ---- the pull request description
if [ -n "${REFRESH_BODY:-}" ]; then
  run_url=""
  if [ -n "${GITHUB_SERVER_URL:-}" ] && [ -n "${GITHUB_REPOSITORY:-}" ] && [ -n "${GITHUB_RUN_ID:-}" ]; then
    run_url="$GITHUB_SERVER_URL/$GITHUB_REPOSITORY/actions/runs/$GITHUB_RUN_ID"
  fi
  {
    echo "The intermediate CA certificate that **$host** needs is no longer the one bundled in \`inst/extdata/tls/balanca-serpro-chain.pem\`."
    echo "That file is the chain \`R/balanca_tls.R\` trusts, for that host only, when a Linux system cannot verify it:"
    echo "until it is updated, \`load_br_trade()\` and \`load_baci()\` stop working there even though CI keeps passing"
    echo "(the runner trusts the live certificate by its own means)."
    echo
    echo "| | Before (bundled) | After (found live) |"
    echo "|---|---|---|"
    old_i=()
    for f in "${old_certs[@]}"; do
      if ! is_self_signed "$f"; then old_i+=("$f"); fi
    done
    new_i=()
    for f in "${new_certs[@]}"; do
      if ! is_self_signed "$f"; then new_i+=("$f"); fi
    done
    join() { local IFS="$1"; shift; echo "$*"; }
    col() { # one column of the table, for the certificates given as arguments
      local what="$1"
      shift
      local out=()
      for f in "$@"; do
        case "$what" in
          name) out+=("$(cn "$f")") ;;
          until) out+=("$(openssl x509 -in "$f" -noout -enddate | cut -d= -f2)") ;;
          sha) out+=("\`$(fingerprint "$f")\`") ;;
        esac
      done
      join '|' "${out[@]+"${out[@]}"}" | sed 's/|/ + /g'
    }
    echo "| intermediate | $(col name "${old_i[@]+"${old_i[@]}"}") | $(col name "${new_i[@]+"${new_i[@]}"}") |"
    echo "| valid until | $(col until "${old_i[@]+"${old_i[@]}"}") | $(col until "${new_i[@]+"${new_i[@]}"}") |"
    echo "| SHA-256 | $(col sha "${old_i[@]+"${old_i[@]}"}") | $(col sha "${new_i[@]+"${new_i[@]}"}") |"
    echo
    echo "What was checked before this file was rewritten:"
    echo "- the chain the server presents plus the downloaded certificate(s) verifies against the runner's own root store only"
    echo "  (signatures, dates, serverAuth purpose, host name), so the certificate is vouched for by a root already trusted;"
    echo "- the **regenerated bundle alone** verifies the live certificate for $host, the way R uses it"
    echo "  (\`openssl verify -CAfile <bundle> -no-CApath -purpose sslserver -verify_hostname $host\`)."
    echo
    echo "The change is this one file: the self-signed root(s) are kept as they were, the intermediate(s) are replaced, and the"
    echo "header comment is regenerated from the certificates."
    echo
    echo "**Before merging**"
    echo "- Compare the SHA-256 above with what the CA itself publishes (the header says which URL each certificate came from)."
    echo "  The download was plain http; it is trusted only because of the check above."
    echo "- \`tests/testthat/test-balanca-tls.R\` should pass with the new file (it expects a self-signed root then one intermediate)."
    echo "- The prose in \`R/balanca_tls.R\` (the FRAGILE note, dated 2026-10-21) still names the previous intermediate and its dates."
    if [ -n "$run_url" ]; then
      echo
      echo "Opened by [this run]($run_url) of \`update-manifest.yaml\`."
    fi
  } >"$REFRESH_BODY"
fi
exit 0
