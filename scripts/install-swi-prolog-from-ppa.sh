#!/usr/bin/env bash
# Install SWI-Prolog from the upstream PPA.
#
# Bypasses `add-apt-repository`, which queries launchpad.net's API. That host
# has had recurring outages (HTTP 504 / connection refused). The actual .deb
# files are hosted on a separate, more reliable CDN (ppa.launchpadcontent.net)
# and the GPG signing key is mirrored on keyserver.ubuntu.com — both of which
# stay healthy through the launchpad.net incidents.
#
# Usage: install-swi-prolog-from-ppa.sh <package> [<package>...]
#   e.g. install-swi-prolog-from-ppa.sh swi-prolog
#        install-swi-prolog-from-ppa.sh swi-prolog-nox
#
# Adds retry-with-backoff for transient unreachability of either the keyserver
# or ppa.launchpadcontent.net, and verifies the installed version is high
# enough for janus-swi (>= 9.1.12). If the PPA source silently failed and apt
# fell back to Ubuntu's older stock package, the version check fails the build
# loudly instead of leaking a broken install into the rest of the pipeline.

set -euo pipefail

SWI_GPG_KEY="${SWI_GPG_KEY:-0xE8B739E3753FF4A12360BA6A4AB3A5F60EA9AEB3}"
KEYRING="${KEYRING:-/usr/share/keyrings/swi-prolog.gpg}"
SOURCES_LIST="${SOURCES_LIST:-/etc/apt/sources.list.d/swi-prolog.list}"
MIN_VERSION="${MIN_VERSION:-9.1.12}"

if [ "$#" -eq 0 ]; then
    echo "Usage: $0 <package> [<package>...]" >&2
    exit 2
fi

# Detect Ubuntu codename without relying on lsb_release (not in minimal images).
CODENAME="${CODENAME:-}"
if [ -z "$CODENAME" ] && [ -r /etc/os-release ]; then
    # shellcheck disable=SC1091
    CODENAME="$(. /etc/os-release && echo "${VERSION_CODENAME:-${UBUNTU_CODENAME:-}}")"
fi
if [ -z "$CODENAME" ]; then
    echo "ERROR: could not detect Ubuntu codename (set CODENAME=...)" >&2
    exit 1
fi

# sudo only when not already root (works in both CI runners and Docker).
SUDO=""
if [ "$(id -u)" -ne 0 ]; then
    SUDO="sudo"
fi

# Ensure curl + gpg are available (no-op if already installed).
if ! command -v curl >/dev/null 2>&1 || ! command -v gpg >/dev/null 2>&1; then
    $SUDO apt-get update
    $SUDO apt-get install -y --no-install-recommends curl gnupg ca-certificates
fi

retry() {
    local label="$1"; shift
    local attempt
    for attempt in 1 2 3 4 5; do
        if "$@"; then
            return 0
        fi
        local sleep_for=$((attempt * 5))
        echo "[$label] attempt $attempt failed; retrying in ${sleep_for}s..." >&2
        sleep "$sleep_for"
    done
    echo "[$label] all 5 attempts failed" >&2
    return 1
}

fetch_key() {
    curl -fsSL --max-time 30 \
        "https://keyserver.ubuntu.com/pks/lookup?op=get&search=${SWI_GPG_KEY}" \
        | $SUDO gpg --dearmor -o "$KEYRING"
}

# Fetch GPG key with retries.
$SUDO install -d -m 0755 "$(dirname "$KEYRING")"
retry "gpg-key-fetch" fetch_key

# Configure apt source pointing at the CDN-backed PPA host.
echo "deb [signed-by=${KEYRING}] https://ppa.launchpadcontent.net/swi-prolog/stable/ubuntu/ ${CODENAME} main" \
    | $SUDO tee "$SOURCES_LIST" >/dev/null

# `apt-get update` returns 0 on partial-source failure (W: warnings) by default,
# which would let apt silently fall back to stock Ubuntu's older swi-prolog.
# `Error-Mode=any` promotes those W: lines to hard errors so the retry actually
# runs when the PPA is unreachable.
update_apt() {
    $SUDO apt-get update -o APT::Update::Error-Mode=any
}
retry "apt-get update" update_apt

# Install requested packages.
$SUDO apt-get install -y --no-install-recommends "$@"

# Sanity-check version.
swipl_ver="$(swipl --version | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1)"
if [ "$(printf '%s\n%s\n' "$MIN_VERSION" "$swipl_ver" | sort -V | head -1)" != "$MIN_VERSION" ]; then
    echo "ERROR: SWI-Prolog $swipl_ver is older than required $MIN_VERSION" >&2
    echo "       This usually means the PPA install silently failed and apt fell back to Ubuntu stock." >&2
    exit 1
fi
echo "SWI-Prolog $swipl_ver installed from PPA."
