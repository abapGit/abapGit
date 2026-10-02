#!/usr/bin/env bash
set -eu

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
project_root="$(cd "$script_dir/../.." && pwd)"
cert_file="$script_dir/.work/registry.crt"

if command -v cygpath >/dev/null 2>&1; then
  cert_file="$(cygpath -w "$cert_file")"
fi

export NODE_EXTRA_CA_CERTS="$cert_file"
cd "$project_root"
npm run integration
