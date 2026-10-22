#!/usr/bin/env bash
set -eu

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
project_root="$(cd "$script_dir/../.." && pwd)"
oras_bin="${ORAS_BIN:-oras}"
ready=0

for attempt in $(seq 1 30); do
  if curl --silent --fail --insecure https://127.0.0.1:5443/v2/ >/dev/null 2>&1; then
    ready=1
    break
  fi
  sleep 1
done

if [ "$ready" -ne 1 ]; then
  echo "Local OCI registry did not become ready on https://127.0.0.1:5443" >&2
  exit 1
fi

"$oras_bin" push --insecure \
  --artifact-type application/vnd.abapgit.repository.v1 \
  127.0.0.1:5443/team/library:v1 \
  "$project_root/test/oci/fixtures/v1/snapshot.tar:application/vnd.oci.image.layer.v1.tar"

"$oras_bin" push --insecure \
  --artifact-type application/vnd.abapgit.repository.v1 \
  127.0.0.1:5443/team/library:v2 \
  "$project_root/test/oci/fixtures/v2/snapshot.tar:application/vnd.oci.image.layer.v1.tar"
