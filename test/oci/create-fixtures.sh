#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "$0")/../.." && pwd)"
fixtures="test/oci/fixtures"
layout="test/oci/layout"
work="test/oci/.work"
artifact_type='application/vnd.abapgit.repository.v1'
layer_type='application/vnd.oci.image.layer.v1.tar'

mkdir -p "$work"
cd "$root"

for version in v1 v2; do
  archive="$work/$version.tar"
  tar --format=ustar --sort=name --mtime='@0' \
    --owner=0 --group=0 --numeric-owner \
    -cf "$archive" -C "$fixtures/$version" .abapgit.xml src
  cp "$archive" "$fixtures/$version/snapshot.tar"
  oras push --oci-layout --artifact-type "$artifact_type" \
    --annotation 'org.opencontainers.image.created=2000-01-01T00:00:00Z' \
    --export-manifest "test/oci/fixtures/$version/manifest.json" \
    "test/oci/layout:$version" "$archive:$layer_type"
done

echo "Created ORAS OCI layout at $layout"
