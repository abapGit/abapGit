# Implementation plan: OCI registry remotes

Implement [abapGit issue #6618](https://github.com/abapGit/abapGit/issues/6618): install and update an abapGit repository from an OCI registry, with pull operations only. This plan is based on the issue body, all three comments, and the local checkout at `cb95b0b8`. Proposed names and artifact conventions below are design decisions, not existing abapGit APIs or requirements stated in the issue.

## Scope and expected behavior

A user selects **OCI Registry** when creating a repository, enters a registry, repository path, and explicit tag or digest, and binds it to an SAP package. abapGit retrieves a packaged snapshot, shows the usual status and diff, and runs the existing pull checks and deserialization. Refreshing a tag can discover a newer snapshot; a digest reference remains pinned.

The remote is read-only. This does not make the SAP package write-protected: pulling must still be able to create, update, and delete local objects through the normal decision dialogs. Registry uploads, registry deletion, staging, commits, branches, Git tags, pull requests, and Git history are unavailable for OCI repositories.

Initial supported scope:

- HTTPS registries, including a hostname with a port and nested repository paths.
- Explicit tags and `sha256:` manifest digests; no implicit `latest`.
- Anonymous access, HTTP Basic authentication where supported, and Bearer challenge/token authentication for private registries.
- One OCI image manifest containing exactly one uncompressed TAR layer with an abapGit repository snapshot.
- A documented producer recipe using an external TAR tool and ORAS. Production and upload of artifacts happen outside abapGit.
- Existing package authorization, language checks, requirements, APACK checks, overwrite/deletion decisions, transports, activation, and checksums.

Defer gzip/zstd compression, arbitrary loose-file ORAS artifacts, multiple filesystem layers and whiteouts, image indexes, legacy ORAS artifact manifests, tag browsing, registry catalogs, signatures/referrers, OCI APACK dependency resolution, and scheduled OCI pulls. Existing Git and offline ZIP behavior must continue to work.

## Findings in the current code

| Area | Current implementation | Consequence |
| --- | --- | --- |
| Repository model | `src/repo/zif_abapgit_repo.intf.abap` provides generic files, checks, and deserialization. `zif_abapgit_repo_online` adds Git branches, commits, and push. | OCI should use the generic repository contract without implementing fake Git operations. |
| Remote fetch | `src/repo/zcl_abapgit_repo_online.clas.abap`, `fetch_remote`, calls Git porcelain and caches files, Git objects, and a commit. The class is `FINAL`. | Separate fetching from the repository container; do not inherit from the Git repository class. |
| Repository construction | `src/repo/zcl_abapgit_repo_srv.clas.abap`, `instantiate_and_add`, selects a class using only `offline`. `new_online` discovers or creates a Git branch. | Add explicit OCI construction that never enters Git branch discovery or creation. |
| Persistence | `src/persist/zif_abapgit_persistence.intf.abap` stores `offline`, URL, and Git-specific fields. XML serialization and metadata masks live in `zcl_abapgit_persistence_repo`. | Add a repository kind and separate OCI settings with backward-compatible defaults. |
| Shared import path | `src/repo/zcl_abapgit_repo.clas.abap` owns remote files, filters, refresh flags, checks, and deserialization. `set_files_remote` marks fetching complete. | Deliver a complete validated file table and reuse the existing import pipeline. |
| Archive precedent | `src/repo/utils/zcl_abapgit_zip.clas.abap` builds file paths and Git blob SHA-1 values, then optionally strips a common wrapper directory. | Follow the file-table convention; keep OCI root handling explicit rather than reusing ZIP's wrapper heuristic. |
| HTTP | `src/http/zif_abapgit_http_agent.intf.abap` and `zif_abapgit_http_response` expose binary bodies, status, JSON, and headers. The implementation uses existing SSL/proxy settings. | Inject this interface into the OCI client; explicitly handle authentication and redirects. |
| UI and services | Repository view, header helpers, remote settings, router, service helpers, and repository lookup methods assume that `is_offline = false` permits a Git-interface cast. | Audit these assumptions before exposing an OCI repository in the UI. |
| Background processing | `src/background/zif_abapgit_background.intf.abap` requires `zif_abapgit_repo_online`. | Keep OCI out of the background scheduler initially and report unsupported requests clearly. |
| Compatibility | `abaplint.json` targets ABAP 7.02. | Verify hashing and archive dependencies on the minimum supported SAP runtime and in the transpiler. |

The linked [issue #2216](https://github.com/abapGit/abapGit/issues/2216) proposes separating repository containers from connection strategies. Apply that idea narrowly to remote fetching. Completing its entire class hierarchy cleanup is outside this feature. Its discussion and `CONTRIBUTING.md` favor small changes with separate reviews.

## Proposed design

### Repository kinds and capabilities

Introduce explicit `git`, `offline`, and `oci` repository kinds. Preserve `is_offline` for compatibility, but replace checks that actually mean “supports Git operations” with a kind or capability check. An OCI repository is online and pullable, while its Git and push capabilities are false.

Expose generic remote metadata and capabilities through the repository contract: remote address, selected reference, resolved revision, network refresh, and Git/push support. Keep revision identifiers as strings: an OCI `sha256:` digest cannot fit into a Git SHA-1 field. Keep Git-only operations on `zif_abapgit_repo_online`.

Add `zcl_abapgit_repo_oci`, inheriting from `zcl_abapgit_repo`. Introduce a narrow `zif_abapgit_repo_connector` with a fetch operation returning a complete file snapshot and an opaque resolved revision. Attach a Git connector to the existing online repository and an OCI connector to the new repository. Git objects and commit details remain in the Git connector/adapter and are available to existing Git operations; they do not become required fields of the generic connector result. Preserve the offline repository's in-memory ZIP source and refresh semantics.

Use constructor injection and the project's factory/injector patterns for connectors and HTTP dependencies. New global classes/interfaces need their `.clas.xml`/`.intf.xml` metadata as well as ABAP source.

### Reference and artifact contract

Collect registry origin, repository name, and reference as separate fields. Display a canonical reference such as `oci://registry.example.com/team/library:1.2.3` or `oci://registry.example.com/team/library@sha256:<64 lowercase hex characters>`. The `oci://` form identifies the remote; HTTP requests use HTTPS. Reject user information, query strings, fragments, missing references, and malformed names. Do not pass these references through Git URL normalization.

Proposed version-one artifact profile:

- Manifest media type: `application/vnd.oci.image.manifest.v1+json`, schema version 2.
- Proposed artifact identity: `application/vnd.abapgit.repository.v1`. For OCI 1.1 use `artifactType`; for an OCI 1.0 producer use the corresponding config media type. Settle this convention with maintainers before implementation.
- Exactly one layer with media type `application/vnd.oci.image.layer.v1.tar`, containing a valid POSIX USTAR archive.
- `.abapgit.xml` at archive root, with serialized objects beneath the paths it declares. Do not strip an arbitrary directory or choose among multiple repository roots.
- Ignore informational annotations. Reject incompatible artifact identities, manifest formats, layer counts, and layer types with an actionable error.

OCI defines the manifest envelope; this application profile deliberately narrows the accepted content. The SAP article introduces generic ORAS storage, so accepting every artifact that ORAS can upload would be a separate compatibility feature. Sources: [OCI manifest specification](https://github.com/opencontainers/image-spec/blob/v1.1.1/manifest.md), [SAP article](https://community.sap.com/t5/application-development-and-automation-blog-posts/storing-abap-build-artifacts-in-oci-registry/ba-p/13529848), and [ORAS producer options](https://oras.land/docs/commands/oras_push/).

### Pull and snapshot lifecycle

1. Validate the reference and fetch `GET /v2/<name>/manifests/<tag-or-digest>` with an explicit supported manifest `Accept` header.
2. Handle authentication and bounded redirects; validate status, response media type, JSON structure, and artifact profile.
3. Calculate the manifest's SHA-256 over the original response bytes. For digest references verify the requested digest; verify `Docker-Content-Digest` when provided. Retain the resolved digest for tag references.
4. Fetch the selected layer with `GET /v2/<name>/blobs/<digest>`. Verify descriptor size and SHA-256 against the downloaded bytes before archive decoding.
5. Decode TAR in memory, validate the repository root, and populate `zif_abapgit_git_definitions=>ty_files_tt` with `path`, `filename`, raw `data`, and `zcl_abapgit_hash=>sha1_blob( data )`.
6. Publish files and resolved revision together only after validation succeeds. Use `set_files_remote` and the shared filtering/status/import path.
7. Run checks, decisions, deletion, deserialization, and activation against this same snapshot. Do not re-resolve a mutable tag halfway through a pull. Record the last successfully imported digest separately from the currently fetched digest, using the existing success/log semantics.

Refresh invalidates fetch state; the next read can resolve a tag again. Reuse verified layer bytes by digest within the session. Fetch errors must not expose partial files as a successful snapshot or advance the imported revision. SAP imports retain the existing partial-failure and checksum behavior; this feature does not promise transactional rollback of all SAP object changes.

Protocol references: [OCI distribution pull endpoints](https://github.com/opencontainers/distribution-spec/blob/v1.1.1/spec.md#pull), [descriptor integrity rules](https://github.com/opencontainers/image-spec/blob/v1.1.1/descriptor.md).

## Implementation sequence

### 1. Confirm the profile and build fixtures

- [ ] Confirm the proposed artifact identity, one-layer profile, root convention, and deferred features in the existing issue discussion before substantial implementation. This plan does not post a message.
- [ ] Follow up on [the offer of existing ABAP TAR code](https://github.com/abapGit/abapGit/issues/6618#issuecomment-1792386036); assess source availability, license, supported TAR variants, ABAP 7.02 compatibility, and standalone inclusion before choosing reuse or a new decoder. Do not assume this code is already available.
- [ ] Verify a plain binary SHA-256 implementation on supported SAP releases and in transpiled tests. Prefer an existing compatible SAP API behind a small wrapper; do not truncate digests or reuse the Git blob-hashing format for OCI verification.
- [ ] Produce a minimal repository fixture with `.abapgit.xml`, a package, and a simple ABAP object. Store its TAR bytes, manifest, and expected digests, plus a second version with an update and a deletion.
- [ ] Validate an external producer command that explicitly selects USTAR and the layer media type. Capture an ORAS-produced manifest rather than guessing its structure.

Exit: a documented profile and independently generated fixtures that both the producer and future consumer can reproduce.

### 2. Introduce the narrow repository abstraction

- [ ] Add kind/capability/remote-metadata accessors and the connector contract in `src/repo/`.
- [ ] Extract Git fetching into a connector without changing branch selection, Git object caches, push, refresh, or filtered-file behavior. Keep existing Git public interfaces available.
- [ ] Update existing repository test doubles for the new contract.
- [ ] Start replacing unsafe online-to-Git assumptions; search all `src/` for `is_offline`, `zif_abapgit_repo_online`, `zcl_abapgit_repo_online`, and casts before declaring the audit complete.
- [ ] Preserve offline ZIP imports and the offline subclass's `reset_remote` behavior.

Exit: existing Git/offline tests pass with no OCI repository yet exposed. Submit this mechanical refactor separately.

### 3. Add a bounded TAR reader

- [ ] Add or adapt `zcl_abapgit_tar` under `src/repo/utils/`, with a binary-input API independent of HTTP and SAP object creation.
- [ ] Implement USTAR's 512-byte headers, header checksum, octal sizes, prefix/name combination, payload padding, and end-of-archive handling. Support regular files and directory entries; preserve file bytes exactly.
- [ ] Normalize harmless leading `./`; produce absolute abapGit directory paths with trailing `/`. Reject absolute archive paths, `..` segments, drive/UNC paths, invalid names, and duplicate normalized file paths.
- [ ] Reject symlinks, hard links, devices, sparse entries, GNU/PAX extensions, and unsupported numeric encodings with descriptive errors. The producer recipe must generate the supported subset.
- [ ] Validate lengths and arithmetic before slicing binary data. Set named limits for archive bytes, individual file bytes, and entry count; reject oversized declarations and malformed/truncated archives without a runtime dump.
- [ ] Return the standard file table with Git blob hashes and require exactly one root `.abapgit.xml` when validating the OCI snapshot.

Exit: ordinary, nested, empty, binary, and prefix-based filenames decode correctly; malformed and unsupported inputs fail deterministically. ZIP handling remains unchanged.

### 4. Implement the OCI registry client and connector

- [ ] Add a reference parser, OCI client, and connector, with injected `zif_abapgit_http_agent` and test doubles for responses.
- [ ] Implement manifest/layer retrieval, artifact-profile validation, raw SHA-256 verification, and TAR-to-snapshot conversion. Parse JSON through the existing AJSON interfaces and translate failures into `zcx_abapgit_exception`.
- [ ] Implement anonymous and Basic access, then the `401` Bearer challenge flow: parse `realm`/`service`/`scope`, request a token with repository pull scope, accept `token` or `access_token`, retry with a Bearer header, and bound retries. Cache tokens only in memory with origin/service/scope and expiry boundaries.
- [ ] Reuse existing credential prompting/session handling where suitable, but inspect the Git login manager's keying before reuse. Do not persist passwords/tokens in repository XML or log authentication headers. Restrict credential-bearing token requests to a trusted configured realm; allow explicitly configured external authentication origins.
- [ ] Handle common blob redirects with a hop limit and HTTPS validation. Do not copy registry credentials to a different origin. Close HTTP responses on success and exception paths.
- [ ] Report missing manifests/blobs, denied access, rate limits, unsupported formats, integrity failures, and transport errors with the affected operation/reference. Keep the client read-only: manifest, blob, and token retrieval use GET; no registry mutation endpoints.
- [ ] Enforce manifest/blob size limits. The current HTTP response API materializes whole bodies; document that limitation and add bounded receiving if needed to enforce a hard network-memory limit. Post-download checks alone cannot provide that guarantee.

Exit: mocked anonymous/private registry flows yield a verified snapshot; authentication, redirect, format, and digest failures yield no partial snapshot. Authentication source: [Distribution token authentication](https://distribution.github.io/distribution/spec/auth/token/).

### 5. Persist and construct OCI repositories

- [ ] Extend `ty_repo_xml`, `ty_repo_meta_mask`, and remote-settings types in `src/persist/zif_abapgit_persistence.intf.abap`. Store kind, registry origin, repository name, selected tag/digest, and last imported digest in OCI-specific fields.
- [ ] Update `zif_abapgit_persist_repo` creation parameters, `zcl_abapgit_persistence_repo` serialization/defaults/masks, and the shared repository `set`/listener path. Keep masks and persisted fields aligned.
- [ ] Default absent kind to `offline` when the old flag is true, otherwise `git`. Write explicit kinds for new records. Reject unknown kinds and inconsistent OCI/offline settings. No destructive database migration is needed.
- [ ] Add `new_oci` to `zif_abapgit_repo_srv`/`zcl_abapgit_repo_srv`; reuse package and authorization checks, but never call Git URL validation, branch discovery, or initial-branch creation.
- [ ] Instantiate `zcl_abapgit_repo_oci` by kind and attach the OCI connector. Reinstantiate when kind changes, not just when `offline` changes.
- [ ] Update duplicate-source detection and installed-repository lookup to use kind-aware remote metadata. Preserve repository key, package binding, and local settings across reloads.
- [ ] Keep existing Git/offline switching behavior. Reject OCI type conversion initially rather than silently discarding its reference or interpreting it as Git.

Exit: legacy records load unchanged; OCI settings survive a new session and select the right runtime class.

### 6. Integrate creation, settings, pull, and capability checks

- [ ] Extend `src/ui/pages/dlg/zcl_abapgit_gui_page_cr_repo.clas.abap` and creation services with an OCI option and registry/repository/reference fields. Preserve existing package and local-setting inputs.
- [ ] Update `src/ui/pages/sett/zcl_abapgit_gui_page_sett_remo.clas.abap` for OCI settings and cache invalidation after a reference change. Show tag/digest semantics without branch or Git commit controls.
- [ ] Update `src/ui/pages/zcl_abapgit_gui_page_repo_view.clas.abap`, overview/list rendering, and `src/ui/lib/zcl_abapgit_gui_chunk_lib.clas.abap`. Display OCI identity and resolved/imported digest; enable refresh, status, diff, and pull when appropriate.
- [ ] Gate stage, patch-for-commit, push, branch/tag mutation, history, pull requests, and flow features using capabilities, including hotkeys and action palettes.
- [ ] Enforce the same checks in router/service entry points so direct event URLs cannot bypass hidden controls. Generic pull continues through `gui_deserialize`/`real_deserialize`.
- [ ] Fix the Git casts in `check_self_update` and `check_for_restart`. Preserve self-update protection where applicable; define and document how an OCI artifact identifies abapGit itself instead of relying on a Git-hosting URL.
- [ ] Audit background setup/dispatch and APACK installation paths. Exclude OCI from Git-only scheduling and flow operations; APACK requirements checking stays active while OCI dependency installation remains unsupported.
- [ ] Adapt remaining generic displays and services, including connection checks, installed-repository lookup, and checksum-rebuild explanations. Reuse existing authorization and write-protection rules for SAP changes.

Exit: an OCI repository can be created, reloaded, refreshed, diffed, and pulled without any Git-interface cast or registry write. Unsupported actions fail clearly even when invoked directly.

### 7. Validate and document the complete workflow

- [ ] Add ABAP Unit tests for reference parsing, TAR, SHA-256 vectors, OCI response handling, snapshot caching, capability guards, and persistence compatibility. Reuse the existing injection/test-double conventions.
- [ ] Test tag movement from fixture version one to version two; verify updates and deletions use the same fetched digest throughout checks and import. Verify pinned digests remain pinned and failed pulls do not advance the imported digest.
- [ ] Test anonymous, Basic, and Bearer flows, expired/denied tokens, redirects without credential leakage, `404`/`429` errors, invalid JSON, index/multilayer rejection, size mismatch, digest mismatch, and malformed TAR.
- [ ] Test root metadata, object filters, ignore/excluded paths, language/requirements/APACK checks, overwrite/deletion decisions, local changes, transports, activation errors, and partial import retry.
- [ ] Add a local OCI registry fixture to the integration harness following `test/gitea/`; produce artifacts with ORAS in the test setup. Keep credentials out of fixtures and public-network access out of unit tests.
- [ ] Run `npm test`, `npm run unit`, `npm run merge`, and `npm run merge.ci` in the project's supported CI environment; run `npm run integration` with the existing Gitea and new registry fixtures. The build scripts assume Unix shell tools, so use CI/WSL as appropriate on Windows.
- [ ] Validate on an SAP system at the minimum supported release, especially SHA-256, HTTP authentication/redirect behavior, certificate/proxy settings, and real object activation. Supplement with a public and private hosted registry smoke test where access is available.
- [ ] Document accepted artifact/reference formats, a reproducible USTAR + ORAS producer recipe, private-registry authentication, tag versus digest behavior, SAP trust/proxy setup, limits, and deferred formats/features.

## Completion criteria

- [ ] A documented ORAS-produced artifact installs into a package and a later tagged snapshot updates it through the standard pull workflow.
- [ ] Digest references are verified and remain immutable; status and pull report the snapshot actually used.
- [ ] Authenticated retrieval works without persisting secrets or requesting registry write scope.
- [ ] OCI exposes no Git/write operations, and direct attempts are rejected before network mutation or unsafe casts.
- [ ] Corrupt/unsupported artifacts fail before SAP deserialization and leave source identity/checksums consistent with the existing error semantics.
- [ ] Existing Git repositories, offline ZIP imports, persistence records, and standalone builds pass their regression checks.
- [ ] The code and documentation state the supported profile and resource/runtime limits; gzip, multi-layer artifacts, indexes, and scheduled OCI pulls are not implied to work.

Deliver the implementation in separate reviews: repository abstraction; TAR/hash utilities; registry client; persistence/repository construction; UI integration and end-to-end documentation. Merge prerequisites before enabling OCI creation. Recheck the relevant source paths and issue discussion against the implementation branch before starting each stage.
