#!/usr/bin/env bash
# Hackage's doc builder lacks libsecretspec, so docs are built and uploaded locally.
# Usage: scripts/hackage-upload.sh <package-dir> [--publish]
set -euo pipefail

pkg="${1:?usage: $0 <package-dir> [--publish]}"
candidate=(--candidate)
test "${2:-}" = --publish && candidate=()

cd "$(dirname "$0")/.."
test -d "$pkg" || {
	echo "no such package: $pkg" >&2
	exit 1
}
pkg-config --exists libsecretspec || {
	echo "libsecretspec not found via pkg-config (see .envrc)" >&2
	exit 1
}

stack upload "${candidate[@]}" "$pkg"
stack upload --documentation "${candidate[@]}" "$pkg"
