# Splitting `hvam/oci2609` into mergeable PRs

The branch adds OCI registry repositories (issue #6618) in one change: 91 files, about +5,900/−230 lines, of which roughly 3,000 are tests and fixtures. This plan cuts it into PRs that each pass CI on their own, can be reviewed in one sitting, and keep `main` releasable.

The previous implementation plan (design, artifact profile, open validation items) is in commit `8b744379` if it is needed again.

## Principles

- **Dark first, visible last.** Utilities, the refactor and the backend merge without any user-visible OCI option. The UI that creates OCI repositories comes last, after SAP-system validation.
- **Separate refactors from features.** The `is_offline` → `supports_git` rewrite is mechanical and behaviour-neutral while only Git and offline kinds exist. Reviewing it next to new OCI branches hides that.
- **Branch every slice from `main`**, not from this branch. Cherry-pick or re-apply file hunks. The branch contains a merge commit from `main`, so a straight rebase is messier than rebuilding the slices.
- Leave out `PLAN.md` and the `.tmp` line in `.gitignore` unless something still needs it.

## Proposed PR sequence

| # | PR | Main files | Approx. size | Depends on |
|---|----|------------|--------------|------------|
| 1 | SHA-256 helper | `zcl_abapgit_hash` + tests | ~60 | – |
| 2 | HTTP agent: optional redirect following | `zif/zcl_abapgit_http_agent` | ~20 | – |
| 3 | USTAR reader | `zcl_abapgit_tar` + tests + xml | ~950 (half tests) | – |
| 4 | Repository kind and capability accessors | `zif_abapgit_repo`, `zcl_abapgit_repo`, test doubles | ~150 | – |
| 5 | Replace "online ⇒ Git" assumptions | router, services, flow, background, repo view, chunk lib, overview, JS + UI test | ~400 | 4 |
| 6 | Extract the Git fetch connector | `zif/zcl_abapgit_repo_git_connector`, `zcl_abapgit_repo_online` + test | ~150 | – |
| 7 | OCI reference parser | `zcl_abapgit_oci_reference` + tests | ~370 | – |
| 8 | OCI registry client | `zif_abapgit_repo_connector`, `zcl_abapgit_oci_client` + tests, `abap_transpile.json` | ~1,700 (≈1,050 tests) | 1, 2, 3, 7 |
| 9 | Persist and construct OCI repositories (no UI) | persistence types/class + tests, `zcl_abapgit_repo` set/imported digest, `zcl_abapgit_repo_oci(_connector)` + tests, `zcl_abapgit_repo_srv` | ~900 | 4, 8 |
| 10 | OCI integration harness and fixtures | `test/oci/**`, `test/gitea/*`, `test/src/zcl_abapgit_oci_integration`, CI workflow, `package.json`, `test/README.md` | ~600 + binaries | 8 |
| 11 | OCI UI: view, pull, self-update guard | OCI branches in repo view/chunk lib/overview, `check_self_update`/`check_for_restart` | ~200 | 5, 9 |
| 12 | OCI UI: create and remote settings + docs | `zcl_abapgit_gui_page_addonline`, `zcl_abapgit_gui_page_sett_remo` + tests, `services_repo=>new_oci`, `docs/oci-registry-repositories.md` | ~450 | 11 |

PRs 1, 2, 3, 4, 6 and 7 have no dependencies on each other and can be opened in parallel.

```text
1 ─┐
2 ─┼─► 8 ─┬─► 9 ─► 11 ─► 12
3 ─┤      └─► 10
7 ─┘
4 ─┬─► 5 ──────────► 11
   └─► 9
6   (independent)
```

## Notes per PR

### 1. SHA-256 helper

`zcl_abapgit_hash=>sha256_raw` and its two test vectors. Useful on its own, small, and reviewers can focus on one question: does `cl_abap_message_digest` with `SHA256` exist on 7.02? Answer that in the PR description; it is still an open item from the original plan.

### 2. HTTP agent redirect flag

Adds `iv_follow_redirect` (default `abap_true`) to `zif_abapgit_http_agent~request`. No caller changes, so no behaviour change. Very quick review, and it unblocks 8.

### 3. USTAR reader

`zcl_abapgit_tar` is a generic binary → `ty_files_tt` decoder with no OCI or HTTP dependencies. It is the biggest piece of security-sensitive parsing (path traversal, size limits, malformed headers), so it deserves a dedicated review. Already covered by the `zcl_abapgit_t` transpile prefix.

### 4. Repository kind and capability accessors

Add `get_repo_kind`, `supports_git`, `supports_push`, `get_remote_address`, `get_selected_reference`, `get_resolved_revision` and `get_imported_revision` to `zif_abapgit_repo`, plus `c_repo_kind` containing **only `git` and `offline`**. Derive the kind from the `offline` flag; do not add a persisted field yet. Update all `zif_abapgit_repo` test doubles (flow logic, repo checksums, any others).

Changes to make while extracting:

- **Move `set_oci_reference` off `zif_abapgit_repo`.** It is the only OCI-specific method on the generic interface. It forces every test double to stub it and makes this PR depend on `zcl_abapgit_oci_reference`. Put it on `zcl_abapgit_repo_oci`, or on a small `zif_abapgit_repo_oci` mirroring `zif_abapgit_repo_online`. Settings code would then cast, the same way it already does for Git.
- **Deduplicate the kind-defaulting logic.** "Empty kind → offline if flagged, else git" exists three times on the branch (`zcl_abapgit_repo~get_repo_kind`, `zcl_abapgit_repo_srv->instantiate_and_add`, `zcl_abapgit_persistence_repo->get_repo_from_content`/`add`). Put it in one place, for example a static helper next to `c_repo_kind`, and introduce that helper here.

### 5. Replace "online ⇒ Git" assumptions

Mechanical change: `is_offline( ) = abap_false` used as "can cast to `zif_abapgit_repo_online`" becomes `supports_git( ) = abap_true`, and guarded casts are added in the router, `zcl_abapgit_services_git` (`get_git_repo`), flow logic/pages, background, `zcl_abapgit_repo_news`, code inspector, patch, repo view hotkeys and the overview `action_git_repo` JS class with its UI test.

With only two kinds, every edit should be a no-op. State that in the PR and leave out the new `ELSE` OCI branches (OCI toolbar, OCI header display), which belong in 11. If the PR feels large, split it into **5a** backend/routing/flow/background and **5b** pages, chunk lib and JS.

Keep the `check_self_update`/`check_for_restart` rewrite **out** of this PR. It changes behaviour for offline repositories: the early `is_offline` return is replaced by a `sy-cprog` check plus a package lookup. It belongs in 11, where that change can be discussed explicitly.

### 6. Extract the Git fetch connector

`zcl_abapgit_repo_online` gets an injectable `zif_abapgit_repo_git_connector`, and fetching moves into `zcl_abapgit_repo_git_connector`. Includes the new `ltcl_repo_online=>fetch_snapshot_cache` test. It is self-contained, matches the direction of issue #2216, and has no OCI code.

Decide in this PR whether two connector interfaces are wanted. `zif_abapgit_repo_git_connector` and `zif_abapgit_repo_connector` differ only by the `objects` field. Possible options:

- Keep both, as on the branch.
- Introduce one `zif_abapgit_repo_connector` here, with Git-specific extras in a sub-interface or a separate method.

Reviewers will ask, so it is better settled before OCI arrives.

### 7. OCI reference parser

`zcl_abapgit_oci_reference` (`parse`/`build`/canonical form) with tests. Pure string logic, no dependencies, easy to review in isolation. It needs the `zcl_abapgit_oci_` entry in `test/abap_transpile.json` (or a narrower one).

### 8. OCI registry client

Includes `zif_abapgit_repo_connector` (or the unified interface from 6), because the client returns its `ty_snapshot`. Nothing instantiates the client from product code yet.

This is the largest review: manifest/blob fetch, digest verification, Basic and Bearer auth, token caching, redirects that drop credentials across origins, and use of the login manager. If reviewers push back on size, split it as follows:

- **8a**: anonymous fetch, manifest profile validation, digest/size verification, TAR to snapshot. Tests: `fetch_anonymous`, `fetch_pinned_digest`, `reject_*`.
- **8b**: authentication and redirects. Tests: `fetch_basic_credentials`, `fetch_bearer`, `refresh_rejected_bearer`, `reject_denied_bearer`, `external_token_auth`, `redirect_drops_*`.

The test class is already grouped this way, so the split is mostly cutting methods.

### 9. Persist and construct OCI repositories (no UI)

- Add `oci` to `c_repo_kind` and the `repo_kind`/`oci_*` fields to `ty_repo_xml`, `ty_repo_meta_mask` and `ty_remote_settings`.
- Add `zif_abapgit_persist_repo~add` parameters and the load-time validation in `get_repo_from_content`.
- Add the persistence tests (`legacy_repo_kind_defaults`, `oci_metadata_roundtrip`, `unknown_kind_is_rejected`). The protected-section move in `zcl_abapgit_persistence_repo` exists only for those tests.
- In `zcl_abapgit_repo`: the new `set` parameters, recording the imported digest in `deserialized`, and the guard in `switch_repo_type`.
- Add `zcl_abapgit_repo_oci`, `zcl_abapgit_repo_oci_connector` and their tests.
- In `zcl_abapgit_repo_srv`: `new_oci`, kind-based `instantiate_and_add`, `reinstantiate` on kind change, and kind-aware `is_repo_installed`/duplicate detection.

Points to flag for review:

- After this PR, every **new** repository record carries `repo_kind`. Confirm that an older abapGit reading such a record ignores the unknown XML element. Downgrade safety is the question maintainers will ask.
- `get_repo_from_content` now **raises** on inconsistent metadata. A single corrupt record could therefore break the repository list. Decide whether that should be a hard error or a logged skip.
- `new_oci` is reachable only through the API, not the UI, so this PR stays dark.

### 10. OCI integration harness and fixtures

Contents: `test/oci/**` (fixtures, scripts, local registry, Node fixture test), the `oci-registry` service in `test/gitea/docker-compose.yml`, `prepare-registry.sh`/`push-fixtures.sh` in the Gitea npm script, `setup-oras` in `test-pr.yml`, the `test:oci-*` scripts and their addition to `npm test`, `test/README.md`, and `test/src/zcl_abapgit_oci_integration`.

Keeping this separate means CI and infrastructure changes get their own review. It can merge once 8 is in. Reviewers may question whether adding the fixture test to the default `npm test` is wanted. An alternative is to run it only in the integration workflow.

### 11. OCI UI: view, pull, self-update guard

- Add the OCI branches in `zcl_abapgit_gui_chunk_lib` (header: reference, resolved and imported digest; icon) with its test, and the OCI toolbar (Pull, Diff, Log) in `zcl_abapgit_gui_page_repo_view`.
- In `zcl_abapgit_services_repo`, add `is_abapgit_package_repo` and the `check_self_update`/`check_for_restart` rewrite.
- Add the router exception that lets `git_pull` through for OCI.

An OCI repository created through the API (PR 9) becomes usable in the UI, but users still cannot create one there.

### 12. OCI UI: create and remote settings, plus docs

- Add the OCI option in `zcl_abapgit_gui_page_addonline`.
- Add OCI fields and cache invalidation in `zcl_abapgit_gui_page_sett_remo`, with its tests.
- Add `zif/zcl_abapgit_services_repo=>new_oci`.
- Add `docs/oci-registry-repositories.md`.

This PR is the one that turns the feature on. Hold it until the open validation items from the original plan are done:

- SHA-256 and HTTP auth/redirects on a minimum-release SAP system.
- A full pull and deserialization of the v1 → v2 fixture.
- The Docker-based `npm run integration`.

If maintainers want it merged earlier, gate it behind the experimental-features setting.

## Suggested order of work

1. Open 1, 2, 3, 6 and 7 now. They are small, independent and uncontroversial.
2. Open 4. Once it merges, open 5 (or 5a and 5b).
3. Open 8 after 1, 2, 3 and 7 are merged, then 10 alongside it.
4. Open 9 after 4 and 8.
5. Open 11, then 12, after SAP validation.

Before starting each slice, re-diff that slice's files against current `main`. Several of them (repo view, router, services) change often upstream.
