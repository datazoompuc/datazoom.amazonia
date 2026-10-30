#!/usr/bin/env bash
# Adds to this machine's TLS trust store the intermediate CA certificate(s) that
# balanca.mdic.gov.br fails to send, found the way a browser finds them: from the
# AIA ("CA Issuers") URL inside the certificate the server actually presents.
# Run by action.yml; read that file's header for why this exists.
#
# THE SECURITY BOUNDARY (nothing is installed unless all of it holds):
#   the certificate the server presents, plus the certificate(s) downloaded from
#   its AIA URL(s), must form a chain that OpenSSL verifies against the system's
#   own root store and nothing else -- valid signatures all the way up, dates,
#   serverAuth purpose, and the host name. The download is plain http (as AIA
#   always is); that is safe only because of this check, never because of where
#   the file came from. TLS verification is never turned off anywhere.
#
# The issuer's NAME is not part of the boundary. It used to be a hard failure,
# which meant a legitimate rotation of SERPRO's intermediate stopped CI until
# someone edited a string, defeating the purpose of reading the AIA at all.
# Anything that passes the check above is, by construction, a CA that a root we
# already trust vouched for AS THE ISSUER OF THIS HOST'S CERTIFICATE. What a
# rotation DOES leave behind is the copy of the chain bundled for R users
# (inst/extdata/tls/balanca-serpro-chain.pem), which this runner no longer needs
# but R users on Linux do: step 5 below compares it with what was found and, when
# the calling workflow asks (TRUST_REFRESH), has refresh-bundled-chain.sh rewrite
# it so that the workflow can open a pull request. A different name needs no check
# of its own -- a different name is a different certificate, so it is not in the
# bundle -- and a hard-coded "expected name" would be a second copy of what the
# bundled file already says, going stale together with it.
#
# Environment (all optional; the defaults are what the GitHub runner needs; the
# overrides exist so this script can be exercised locally and against a fake
# server without touching a real trust store):
#   TRUST_HOST      host to inspect                         [balanca.mdic.gov.br]
#   TRUST_PORT      its TLS port                            [443]
#   TRUST_ROOTS     PEM file of trusted roots               [/etc/ssl/certs/ca-certificates.crt]
#   TRUST_DIR       where update-ca-certificates reads from [/usr/local/share/ca-certificates]
#   TRUST_UPDATE    command that rebuilds the store         [update-ca-certificates]
#   TRUST_SUDO      prefix for the two commands above       [sudo]
#   TRUST_SHIPPED   chain bundled with the R package        [$GITHUB_WORKSPACE/inst/extdata/tls/balanca-serpro-chain.pem]
#   TRUST_REFRESH   true: rewrite TRUST_SHIPPED if it lacks the intermediate found, and set the
#                   step outputs chain-stale / pr-body-path   [false: only a warning]
#   TRUST_MAX_HOPS  longest chain of AIA downloads allowed  [4]
#   TRUST_CHECK_URL URL fetched at the end to report success [https://<host>/balanca/manual/Manual.pdf]
set -euo pipefail

host="${TRUST_HOST:-balanca.mdic.gov.br}"
port="${TRUST_PORT:-443}"
roots="${TRUST_ROOTS:-/etc/ssl/certs/ca-certificates.crt}"
install_dir="${TRUST_DIR:-/usr/local/share/ca-certificates}"
update_cmd="${TRUST_UPDATE:-update-ca-certificates}"
sudo_cmd="${TRUST_SUDO-sudo}"
shipped="${TRUST_SHIPPED:-${GITHUB_WORKSPACE:-.}/inst/extdata/tls/balanca-serpro-chain.pem}"
case "${TRUST_REFRESH:-false}" in
  true | 1 | yes) refresh=1 ;;
  *) refresh=0 ;;
esac
max_hops="${TRUST_MAX_HOPS:-4}"
check_url="${TRUST_CHECK_URL:-https://$host/balanca/manual/Manual.pdf}"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

fail() { echo "::error::$*"; exit 1; }
fingerprint() { openssl x509 -in "$1" -noout -fingerprint -sha256 | cut -d= -f2; }

# (Command output is captured into variables before it is searched: with
# `set -o pipefail`, `cmd | grep -q` or `cmd | head -n 1` can fail with SIGPIPE
# when the reader quits early, which would end this script at random.)

# Does the leaf verify for $host against the system roots ONLY, given these
# extra (untrusted) intermediates? Prints OpenSSL's reason on failure.
verifies() {
  local untrusted=()
  [ -s "$1" ] && untrusted=(-untrusted "$1")
  openssl verify -CAfile "$roots" -no-CApath "${untrusted[@]+"${untrusted[@]}"}" -purpose sslserver -verify_hostname "$host" "$work/leaf.pem" 2>&1
}

[ -s "$roots" ] || fail "trusted roots file '$roots' not found"

# 1. The certificate(s) the server presents. s_client does not stop on a
#    verification error, which is the point: we only READ what is presented.
openssl s_client -connect "$host:$port" -servername "$host" -showcerts </dev/null 2>/dev/null >"$work/sclient.txt" || true
awk -v dir="$work" '/-----BEGIN CERTIFICATE-----/{n++; on=1} on{print > (dir "/sent" n ".pem")} /-----END CERTIFICATE-----/{on=0}' "$work/sclient.txt"
[ -s "$work/sent1.pem" ] || fail "no certificate received from $host:$port"
cp "$work/sent1.pem" "$work/leaf.pem"
: >"$work/pool.pem" # certificates the server sent after the leaf, then the ones we download
for f in "$work"/sent[2-9].pem; do
  if [ -s "$f" ]; then cat "$f" >>"$work/pool.pem"; fi
done
echo "leaf: $(openssl x509 -in "$work/leaf.pem" -noout -subject -enddate | tr '\n' ' ')"

# 2. Already fine (the server sends a full chain, or the store already has it)?
if out="$(verifies "$work/pool.pem")"; then
  echo "$host already verifies against the system roots; nothing to install."
  exit 0
fi
echo "$host does not verify as presented: $(echo "$out" | grep -i 'error' | head -n 1)"

# 3. Follow the AIA 'CA Issuers' URLs upward until the chain verifies.
current="$work/leaf.pem"
downloaded=()
for hop in $(seq 1 "$max_hops"); do
  cert_text="$(openssl x509 -in "$current" -noout -text)"
  aia="$(sed -n 's/^ *CA Issuers - URI://p' <<<"$cert_text" | tr -d '\r')"
  aia="${aia%%$'\n'*}" # the first one, if there are several
  [ -n "$aia" ] || fail "the certificate '$(openssl x509 -in "$current" -noout -subject)' has no AIA 'CA Issuers' URL to fetch its issuer from"
  case "$aia" in http://* | https://*) ;; *) fail "refusing a non-http(s) AIA URL: $aia" ;; esac
  echo "hop $hop: fetching the issuer from $aia"
  curl --fail --silent --show-error --location --max-time 30 --max-filesize 65536 \
    --proto '=http,https' --proto-redir '=http,https' --output "$work/issuer$hop.bin" "$aia" ||
    fail "could not download $aia"
  # AIA files are DER as a rule, PEM sometimes.
  if ! openssl x509 -inform DER -in "$work/issuer$hop.bin" -out "$work/issuer$hop.pem" 2>/dev/null &&
    ! openssl x509 -inform PEM -in "$work/issuer$hop.bin" -out "$work/issuer$hop.pem" 2>/dev/null; then
    fail "$aia did not return an X.509 certificate"
  fi
  issuer_text="$(openssl x509 -in "$work/issuer$hop.pem" -noout -text)"
  grep -q 'CA:TRUE' <<<"$issuer_text" || fail "the certificate at $aia is not a CA certificate"
  subject="$(openssl x509 -in "$work/issuer$hop.pem" -noout -subject)"
  echo "  got: $subject | $(openssl x509 -in "$work/issuer$hop.pem" -noout -enddate) | sha256 $(fingerprint "$work/issuer$hop.pem")"
  cat "$work/issuer$hop.pem" >>"$work/pool.pem"
  downloaded+=("$work/issuer$hop.pem")
  current="$work/issuer$hop.pem"
  if out="$(verifies "$work/pool.pem")"; then
    break
  fi
  # A self-signed certificate is the top of its chain: there is nothing above it
  # to fetch, and if the chain does not verify by now, it never will.
  if [ "$(openssl x509 -in "$current" -noout -subject_hash)" = "$(openssl x509 -in "$current" -noout -issuer_hash)" ]; then
    break
  fi
done

# 4. THE CHECK. If the chain still does not verify, install nothing.
if ! out="$(verifies "$work/pool.pem")"; then
  echo "$out"
  fail "the certificate chain for $host still does not verify against the system roots after $((${#downloaded[@]})) AIA download(s) -- nothing was installed. If OpenSSL says the certificate has expired, the server's own certificate is the problem and no trust-store change helps."
fi
echo "verified: leaf <- $(printf '%s\n' "${downloaded[@]}" | wc -l) downloaded issuer(s) <- a root in $roots (signatures, dates, serverAuth, host name)."

# 5. Is the copy of the chain that R users get (R/balanca_tls.R) still current?
#    This runner now heals itself, which would otherwise hide the answer. When it
#    is not, the calling workflow can ask (TRUST_REFRESH) for the file to be
#    rewritten here, in its workspace, and then opens a pull request for it:
#    a change of name is just one more way of not being in the file, and the
#    names, old and new, go into the warning and into the pull request.
chain_stale=0
if [ -s "$shipped" ]; then
  mkdir "$work/shipped"
  awk -v dir="$work/shipped" '/-----BEGIN CERTIFICATE-----/{n++; on=1} on{print > (dir "/s" n ".pem")} /-----END CERTIFICATE-----/{on=0}' "$shipped"
  : >"$work/shipped_fps.txt"
  bundled_names=""
  for f in "$work"/shipped/s*.pem; do
    if [ -s "$f" ]; then
      fingerprint "$f" >>"$work/shipped_fps.txt"
      if [ "$(openssl x509 -in "$f" -noout -subject_hash)" != "$(openssl x509 -in "$f" -noout -issuer_hash)" ]; then
        bundled_names="$bundled_names $(openssl x509 -in "$f" -noout -subject)"
      fi
    fi
  done
  for f in "${downloaded[@]}"; do
    if ! grep -qxF "$(fingerprint "$f")" "$work/shipped_fps.txt"; then
      chain_stale=1
      echo "::warning::the intermediate $host presents today ($(openssl x509 -in "$f" -noout -subject), sha256 $(fingerprint "$f")) is NOT in inst/extdata/tls/balanca-serpro-chain.pem, the chain bundled for R users (its intermediate:${bundled_names:- none}). load_br_trade() and load_baci() on Linux will fail once the system store stops working for them -- replace the bundled intermediate."
    fi
  done
else
  echo "::notice::$shipped not found; skipping the comparison with the chain bundled for R users."
fi

if [ "$chain_stale" = 1 ] && [ "$refresh" = 1 ]; then
  body="${RUNNER_TEMP:-${TMPDIR:-/tmp}}/serpro-chain-pr-body.md"
  rc=0
  REFRESH_BODY="$body" bash "$(dirname "${BASH_SOURCE[0]}")/refresh-bundled-chain.sh" "$shipped" "$work/leaf.pem" "$host" "${downloaded[@]}" || rc=$?
  if [ "$rc" = 0 ]; then
    echo "::notice::inst/extdata/tls/balanca-serpro-chain.pem was rewritten in this workspace; the workflow opens a pull request for it."
    if [ -n "${GITHUB_OUTPUT:-}" ]; then
      {
        echo "chain-stale=true"
        echo "pr-body-path=$body"
      } >>"$GITHUB_OUTPUT"
    fi
  else
    echo "::warning::the bundled chain could not be refreshed automatically (exit $rc, see above); a person has to update inst/extdata/tls/balanca-serpro-chain.pem."
  fi
fi

# 6. Install, then rebuild the store (update-ca-certificates wants *.crt in PEM).
i=0
for f in "${downloaded[@]}"; do
  i=$((i + 1))
  $sudo_cmd cp "$f" "$install_dir/balanca-intermediate-$i.crt"
done
$sudo_cmd $update_cmd

# 7. Informational only: after this the host verifies unless the server's own
#    certificate has a problem, which curl's error then says.
if curl --fail --silent --show-error --head --max-time 30 "$check_url" >/dev/null; then
  echo "$host now verifies."
else
  echo "::warning::$host still does not verify after installing the intermediate -- see the curl error above (e.g. an expired leaf certificate)."
fi
