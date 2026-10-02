# OCI registry repositories

An OCI repository stores an abapGit source snapshot in an OCI registry. abapGit downloads and validates the snapshot, then uses the normal repository status, diff, pull checks, and deserialization flow. The registry connection is read-only; a pull can still create, update, or delete SAP objects after the existing checks and user decisions.

## Supported artifact profile

The remote reference must include an explicit tag or a SHA-256 manifest digest:

```text
oci://registry.example.com/team/library:1.2.3
oci://registry.example.com/team/library@sha256:<64 lowercase hex characters>
```

The registry origin is a hostname with an optional port, without a scheme or path. Connections always use HTTPS. Tags are resolved again on refresh; the resolved manifest digest is shown with the repository. Digest references stay pinned. A pull uses the already fetched snapshot, so a moving tag is not resolved again between status and import. The imported digest advances only after the normal deserialize path reaches its existing success point.

The artifact must have:

- An OCI image manifest (`application/vnd.oci.image.manifest.v1+json`) with schema version 2.
- Artifact identity `application/vnd.abapgit.repository.v1`, in `artifactType` or the OCI 1.0 config media type.
- Exactly one uncompressed USTAR layer with media type `application/vnd.oci.image.layer.v1.tar`.
- A root `.abapgit.xml` file and the repository files beneath the paths it describes.

The TAR reader accepts regular files and directory entries. It rejects path traversal, duplicate normalized paths, links, devices, PAX/GNU extensions, sparse files, and other unsupported entry types. It does not strip a wrapper directory or choose among multiple repositories.

Current limits are 2 MiB for the manifest, 50 MiB for a layer, 10 MiB per file, and 10,000 TAR entries. The HTTP API currently buffers each response before these size checks, so the limits bound accepted content and archive decoding, but do not hard-limit peak network buffer memory.

## Produce and push a snapshot

Use GNU tar and ORAS outside abapGit. Start from a working tree that contains `.abapgit.xml` at its root. `--format=ustar` prevents GNU tar from writing PAX or GNU extension records; the command fails if a path cannot fit USTAR's name and prefix fields.

```sh
repo_dir=/path/to/abapgit-repository
archive=/tmp/abapgit-snapshot.tar
registry=registry.example.com
repository=team/library
tag=1.2.3

tar --format=ustar --sort=name --mtime='@0' \
  --owner=0 --group=0 --numeric-owner \
  -cf "$archive" -C "$repo_dir" .

oras push \
  --artifact-type application/vnd.abapgit.repository.v1 \
  "$registry/$repository:$tag" \
  "$archive:application/vnd.oci.image.layer.v1.tar"
```

This uses ORAS's OCI 1.1 image-manifest profile and sets the layer media type explicitly. To keep the manifest for inspection, add `--export-manifest manifest.json` to `oras push`. Confirm the exported manifest has the supported artifact type, manifest media type, and exactly one TAR layer before creating a repository in abapGit. ORAS documents both the `--artifact-type` option and per-file media-type syntax in its [`oras push` reference](https://oras.land/docs/commands/oras_push/).

The repository includes two ORAS 1.2.3 fixtures under `test/oci/fixtures/`. Version two updates the sample class and removes another class. Their exported manifests, USTAR bytes, manifest digests, and layer digests are checked by `npm run test:oci-fixtures`. Regenerate the fixtures with `bash test/oci/create-fixtures.sh` after installing GNU tar and ORAS 1.2.3.

For registries that require an OCI 1.0 manifest, ORAS supports `--image-spec v1.0`; set the artifact identity as the config media type and verify the exported manifest against the profile above. Do not push multiple layers or compressed TAR content.

## Authentication and SAP connectivity

Anonymous registries need no setup. For a private registry, abapGit uses the standard credential prompt when the registry rejects the request. Basic credentials are kept in the in-memory login manager for the current session; repository metadata does not store passwords or bearer tokens. Bearer tokens are kept only in the OCI client memory until expiry. The token request asks only for the repository's `pull` scope.

Registry and token requests use the configured SAP HTTP proxy and SSL identity. The SAP system must trust the registry certificate and reach the registry over HTTPS. Registry credentials are sent only to the registry origin. If a different-origin token realm requires Basic authentication, abapGit prompts separately for that realm; those credentials are used only for that HTTPS origin. A blob redirect to a different origin does not receive registry credentials or bearer tokens.

The client follows up to three HTTPS redirects manually and removes authorization when the origin changes. It never sends registry upload, delete, or tag mutation requests.

## Not supported

This profile does not support gzip or zstd layers, multiple filesystem layers or whiteouts, image indexes, legacy ORAS artifact manifests, arbitrary loose-file artifacts, tag browsing, registry catalogs, signatures/referrers, OCI APACK dependency installation, or background scheduled pulls. Git staging, commits, branches, tags, push, history, merge, and pull request operations are unavailable for OCI repositories.

When an OCI repository targets the package containing the running `ZABAPGIT` program, the self-update guard identifies it by that package binding because OCI references do not identify a Git hosting project. Use the standalone version when changing objects that the existing self-update checks flag as unsafe.
