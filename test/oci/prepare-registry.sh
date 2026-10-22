#!/usr/bin/env bash
set -eu

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
work_dir="$script_dir/.work"

if [ ! -d "$work_dir" ]; then
  mkdir -p "$work_dir"
fi
openssl req -x509 -nodes -newkey rsa:2048 \
  -keyout "$work_dir/registry.key" \
  -out "$work_dir/registry.crt" \
  -days 30 \
  -config "$script_dir/registry-openssl.cnf"
