# Splitting `hvam/oci2609` into mergeable PRs

The branch adds OCI registry repositories (issue #6618) in one change: 91 files, about +5,900/−230 lines, of which roughly 3,000 are tests and fixtures. This plan cuts it into PRs that each pass CI on their own, can be reviewed in one sitting, and keep `main` releasable.

The previous implementation plan (design, artifact profile, open validation items) is in commit `8b744379` if it is needed again.

## Target model: OCI is a third repository type

abapGit has two repository types today:

- **online**: `zcl_abapgit_repo_online`, which implements `zif_abapgit_repo_online`.
- **offline**: `zcl_abapgit_repo_offline`.

Both inherit from `zcl_abapgit_repo`, and the `offline` flag in the persisted XML picks the class. OCI is added as a third sibling in the same style, instead of the generic layer the branch introduces.

| | online | offline | OCI (new) |
|---|---|---|---|
| Class | `zcl_abapgit_repo_online` | `zcl_abapgit_repo_offline` | `zcl_abapgit_repo_oci` |
| Type-specific interface | `zif_abapgit_repo_online` | – | `zif_abapgit_repo_oci` |
| `get_type( )` returns | `c_type-online` | `c_type-offline` | `c_type-oci` |
| Persisted flag | – | `offline` | `oci` |
| `url` holds | Git URL | display name | canonical `oci://registry/repo:tag` or `@sha256:…` |
| Created by | `repo_srv->new_online` | `repo_srv->new_offline` | `repo_srv->new_oci` |
| Remote source | Git porcelain | in-memory ZIP | `zcl_abapgit_oci_client` |

Concretely:

- `zif_abapgit_repo` gains `get_type`, returning one of the constants `zif_abapgit_repo=>c_type-online`, `-offline` or `-oci` (type `ty_type`, for example `c LENGTH 7`).
  - `zcl_abapgit_repo` is already `ABSTRACT`. Declare `zif_abapgit_repo~get_type` as abstract there, and have each subclass return its own constant. No flag logic in the base class.
  - Test doubles return whichever type the test needs.
- **`is_offline` is deleted** from `zif_abapgit_repo` and `zcl_abapgit_repo`. Its about 30 call sites on `main` move to `get_type( )` comparisons (PR 6).
- **`switch_repo_type`** takes `iv_type` instead of `iv_offline`. Only online ↔ offline is allowed, and OCI is rejected on either side.
- **Comparing against `c_type-online` is what keeps OCI out of Git code.** Code that today checks `is_offline( ) = abap_false` before casting to `zif_abapgit_repo_online` becomes `get_type( ) = c_type-online`, which excludes OCI automatically. No separate OCI guard is needed at those places.
- OCI-specific accessors go on `zif_abapgit_repo_oci`, implemented by `zcl_abapgit_repo_oci`:
  - `get_reference`
  - `set_reference`
  - `get_resolved_digest`
  - `get_imported_digest`

  Callers cast to it the way they cast to `zif_abapgit_repo_online` today.
- Persistence gets three fields:
  - `oci` (flag)
  - `oci_resolved_digest`
  - `oci_imported_digest`

  Registry, repository and tag/digest are parsed from `url` with `zcl_abapgit_oci_reference`, so there are no separate `oci_registry`/`oci_repository`/`oci_reference` fields.
- `zcl_abapgit_repo_oci` calls the OCI client directly, with an injectable client for tests, the same way `zcl_abapgit_repo_online` calls the Git porcelain.

### What this removes from the branch

- **The generic layer on `zif_abapgit_repo`:**
  - `c_repo_kind`/`repo_kind`
  - `get_repo_kind`
  - `supports_git`
  - `supports_push`
  - `get_remote_address`
  - `get_selected_reference`
  - `get_resolved_revision`
  - `get_imported_revision`
  - `set_oci_reference`

  `get_type` covers what `get_repo_kind`/`supports_git`/`supports_push` were for. The remaining getters move to `zif_abapgit_repo_oci`.
- **The connector abstraction:**
  - `zif_abapgit_repo_connector`
  - `zif_abapgit_repo_git_connector`
  - `zcl_abapgit_repo_git_connector`
  - `zcl_abapgit_repo_oci_connector`
  - the connector injection in `zcl_abapgit_repo_online`

  The OCI client defines its own result type (files plus manifest digest).
- **The `is_offline` → `supports_git` rewrite.** It is replaced by the `is_offline` → `get_type` rewrite (PR 6). The call sites are the same, but the method ends up deleted rather than kept next to a new capability API.
- **The repeated kind-defaulting logic** in `zcl_abapgit_repo`, `zcl_abapgit_repo_srv` and `zcl_abapgit_persistence_repo`. Each subclass knows its type, and persistence keeps flags that default to empty, so legacy records need no defaulting. Only `instantiate_and_add` reads the flags to choose the class.

The Git connector extraction (issue #2216 direction) can still be proposed later as an independent refactor. It is no longer on the OCI critical path.

## Principles

- **Dark first, visible last.** Utilities, the client and the backend type merge without any user-visible OCI option. The UI that creates OCI repositories comes last, after SAP-system validation.
- **Branch every slice from `main`**, not from this branch. Re-apply hunks per slice. The branch contains a merge commit from `main` and the abstractions above, so most slices are rewrites rather than cherry-picks.
- Leave out `PLAN.md` and the `.tmp` line in `.gitignore` unless something still needs it.

## Proposed PR sequence

| # | PR | Main files | Approx. size | Depends on |
|---|----|------------|--------------|------------|
| 1 | SHA-256 helper | `zcl_abapgit_hash` + tests | ~60 | – |
| 2 | HTTP agent: optional redirect following | `zif/zcl_abapgit_http_agent` | ~20 | – |
| 3 | USTAR reader | `zcl_abapgit_tar` + tests + xml | ~950 (half tests) | – |
| 4 | OCI reference parser | `zcl_abapgit_oci_reference` + tests, `abap_transpile.json` | ~370 | – |
| 5 | OCI registry client | `zcl_abapgit_oci_client` + tests | ~1,700 (≈1,050 tests) | 1, 2, 3, 4 |
| 6 | Replace `is_offline` with `get_type` | `zif_abapgit_repo~get_type` + `c_type`, about 30 call sites, `switch_repo_type( iv_type )`, test doubles, overview JS + UI test | ~400 | – |
| 7 | OCI repository type, backend only | persistence + tests, `zif/zcl_abapgit_repo_oci` + tests, `zcl_abapgit_repo`, `zif/zcl_abapgit_repo_srv` | ~700 | 4, 5, 6 |
| 8 | OCI integration harness and fixtures | `test/oci/**`, `test/gitea/*`, `test/src/zcl_abapgit_oci_integration`, CI workflow, `package.json`, `test/README.md` | ~600 + binaries | 5 |
| 9 | OCI UI: view, pull, self-update guard | OCI branches in repo view, chunk lib, overview; `check_self_update`/`check_for_restart` | ~200 | 7 |
| 10 | OCI UI: create and remote settings + docs | `zcl_abapgit_gui_page_addonline`, `zcl_abapgit_gui_page_sett_remo` + tests, `services_repo=>new_oci`, `docs/oci-registry-repositories.md` | ~450 | 9 |

PRs 1, 2, 3, 4 and 6 have no dependencies on each other and can be opened in parallel.

```text
1 ─┐
2 ─┼─► 5 ─┬─► 7 ─► 9 ─► 10
3 ─┤      └─► 8
4 ─┘
6 ────────────► 7
```

## Notes per PR

### 1. SHA-256 helper

`zcl_abapgit_hash=>sha256_raw` and its two test vectors. Reviewers can focus on one question: does `cl_abap_message_digest` with `SHA256` exist on 7.02? Answer that in the PR description; it is still an open validation item.

### 2. HTTP agent redirect flag

Adds `iv_follow_redirect` (default `abap_true`) to `zif_abapgit_http_agent~request`. No caller changes, so no behaviour change.

### 3. USTAR reader

`zcl_abapgit_tar` is a generic binary → `ty_files_tt` decoder with no OCI or HTTP dependencies. It is the most security-sensitive parsing in the feature (path traversal, size limits, malformed headers), so it deserves a dedicated review. Already covered by the `zcl_abapgit_t` transpile prefix.

### 4. OCI reference parser

`zcl_abapgit_oci_reference` (`parse`/`build`/canonical form) with tests. Pure string logic. Add the `zcl_abapgit_oci_` prefix to `test/abap_transpile.json` here.

In this model the parser is also how persisted OCI repositories get their registry, repository and tag/digest out of `url`. Make sure `parse( build( … ) )` round-trips the canonical form exactly.

### 5. OCI registry client

Change from the branch: `zcl_abapgit_oci_client=>fetch` returns its own type (for example `ty_snapshot` with `files` and `digest`) instead of `zif_abapgit_repo_connector=>ty_snapshot`. Nothing instantiates the client from product code yet.

This is the largest review: manifest/blob fetch, digest verification, Basic and Bearer auth, token caching, redirects that drop credentials across origins, and use of the login manager. If it is too big, split it as follows:

- **5a**: anonymous fetch, manifest profile validation, digest/size verification, TAR to snapshot. Tests: `fetch_anonymous`, `fetch_pinned_digest`, `reject_*`.
- **5b**: authentication and redirects. Tests: `fetch_basic_credentials`, `fetch_bearer`, `refresh_rejected_bearer`, `reject_denied_bearer`, `external_token_auth`, `redirect_drops_*`.

### 6. Replace `is_offline` with `get_type`

This is a refactor with no behaviour change, done before OCI exists:

- Add `zif_abapgit_repo=>ty_type`, the constants `c_type-online`, `-offline` and `-oci`, and the method `get_type`.
- Make `get_type` abstract in `zcl_abapgit_repo` and implement it in `zcl_abapgit_repo_online` and `zcl_abapgit_repo_offline`.
- Replace every `is_offline( )` call, then **delete `is_offline`** from the interface and the class alias.
- Change `switch_repo_type` to take `iv_type`. The remote settings page passes the type instead of the flag.
- Update the `zif_abapgit_repo` test doubles: flow logic, repo checksums, repo pull, chunk lib, remote settings and repo service tests.

The `oci` constant already exists, but nothing returns it until PR 7.

For each call site, translate by what the code means, not literally:

- `is_offline( ) = abap_false`, then casting to `zif_abapgit_repo_online` or doing Git work, becomes `get_type( ) = c_type-online`. This is most sites, and once PR 7 lands, OCI falls out of them automatically.
- `is_offline( ) = abap_true` meaning "show the ZIP import/export controls" becomes `get_type( ) = c_type-offline`.
- `is_offline( ) = abap_true` meaning "no Git actions" becomes `get_type( ) <> c_type-online`.
- Two-way `IF … ELSE` blocks over online/offline become `CASE get_type( )`. That leaves an obvious place to add the `WHEN c_type-oci` branch in PR 9.

The overview list currently exposes `data-offline` to the JS. Replace it with `data-type`, so the JS can enable Git-only actions only for `online`. Update the UI test.

Some code reads the `offline` flag directly instead of calling `is_offline`, so searching for `is_offline` misses it. An OCI record has `offline` empty, so each of these would treat OCI as online. Also search for `-offline` and `isOffline`:

- `zcl_abapgit_gui_page_repo_over` fills `ls_overview-offline` from `ms_data-offline`, which feeds `data-offline`. Replace it with a `type` field filled from `get_type( )`, which feeds `data-type`.
- `zcl_abapgit_gui_chunk_lib` writes `isOffline` into the `repoCatalog` JS array, and `zabapgit_js_common.w3mi.data.js` uses it to choose the icon in the repository jump list. Replace it with `type`. PR 9 adds the OCI icon there.
- `zcl_abapgit_repo_srv=>instantiate_and_add` reads `is_repo_meta-offline`. Leave it as it is; PR 7 changes it.

Also check the places that cast to `zif_abapgit_repo_online` **without** testing `is_offline` first. On `main` about 30 classes reference that interface. Once OCI exists, these are the ones that could still see an OCI repository:

- **Entry points from the repository list or view:**
  - `zcl_abapgit_gui_router` (stage, patch, branch/tag actions; `git_pull` must stay allowed)
  - `zcl_abapgit_services_git`
  - `zcl_abapgit_gui_page_repo_view` (toolbar, hotkeys)
  - `zcl_abapgit_gui_chunk_lib` (header)
  - `zcl_abapgit_gui_page_repo_over` (Git-only list actions; the `data-type` attribute and JS toggle with the UI test)
  - `zcl_abapgit_gui_page_flow` (pull, stage and branch selection event handlers, and the feature toolbar) and `zcl_abapgit_flow_page_utils`. These take the repository key from the event query, so they cast without any check of their own.
  - code inspector page
  - patch page
  - `zcl_abapgit_gui_page_sett_remo`
- **Loops over all repositories:**
  - `zcl_abapgit_repo_srv` (`is_repo_installed`, URL duplicate check)
  - `zcl_abapgit_repo_news`
  - `zcl_abapgit_flow_logic` (feature listing, `update_all_branches`)
  - `zcl_abapgit_background`

Give these an explicit `get_type( ) = c_type-online` check (skip or raise) in this PR. Today that check is a no-op, and it saves a later OCI PR from touching them.

Pages that are only reachable from an already-checked Git action, such as commit, merge, tags, and branch/ref selection, need no change.

The branch's `supports_git` edits are a good checklist of call sites. Translate each `supports_git( ) = abap_true` to `get_type( ) = c_type-online`.

If the PR is large, split it as follows:

- **6a**: add `get_type`, and migrate backend, routing, flow and background.
- **6b**: migrate pages, chunk lib and JS, then delete `is_offline`.

`is_offline` can only be deleted once both parts are merged.

Deleting `is_offline` is a deliberate breaking change. Code outside this repository that calls `zif_abapgit_repo~is_offline`, such as CI runners, user exits and API consumers, stops compiling. Do not keep a deprecated wrapper. State the break in the PR description and give the replacement: `is_offline( ) = abap_true` becomes `get_type( ) = zif_abapgit_repo=>c_type-offline`, or `get_type( ) <> c_type-online` where the code means "no Git".

### 7. OCI repository type, backend only

- **Persistence:**
  - Add `oci`, `oci_resolved_digest` and `oci_imported_digest` to `ty_repo_xml`, `ty_repo_meta_mask` and `ty_remote_settings`.
  - Add an `iv_oci` parameter on `zif_abapgit_persist_repo~add`.
  - Validate on load that `offline` and `oci` are not both set and that an OCI `url` parses.
  - Add tests for a legacy record without `oci`, an OCI round trip, and an inconsistent record.
- **`zcl_abapgit_repo`:**
  - Add the `set` parameters for the new fields.
  - Record `oci_imported_digest` in `deserialized`.
  - Make `switch_repo_type` reject `c_type-oci` as both the source and the target type.
- **New types:** add `zif_abapgit_repo_oci` and `zcl_abapgit_repo_oci` with their tests. `zcl_abapgit_repo_oci` implements `get_type` (returns `c_type-oci`), fetches through the client, falls back to a name in `get_name`, and implements `has_remote_source`.
- **`zif/zcl_abapgit_repo_srv`:**
  - Declare `new_oci` in `zif_abapgit_repo_srv` and implement it. The branch signature takes `iv_registry`, `iv_repository` and `iv_reference` separately. `new_oci` builds the canonical `url` from them with `zcl_abapgit_oci_reference=>build`, and `url` is the only place they are stored.
  - Make `instantiate_and_add` choose between three classes (oci → `zcl_abapgit_repo_oci`, offline → `_offline`, else `_online`).
  - Reinstantiate when `oci` changes.
  - Compare OCI repositories by registry and repository in `is_repo_installed`, and skip them in the Git URL duplicate check.

`new_oci` is reachable only through the API, so this PR stays dark.

Points to flag for review:

- **Records written after this PR carry OCI fields.** An older abapGit would load an OCI record as an online repository with an `oci://` URL and fail on fetch. Decide whether that is acceptable or whether the old reader needs to skip it. Git and offline records are unchanged apart from empty elements.
- **The load-time check now raises on inconsistent metadata.** A single corrupt record could therefore break the repository list. Decide whether that should be a hard error or a logged skip.

### 8. OCI integration harness and fixtures

Contents: `test/oci/**` (fixtures, scripts, local registry, Node fixture test), the `oci-registry` service in `test/gitea/docker-compose.yml`, `prepare-registry.sh`/`push-fixtures.sh` in the Gitea npm script, `setup-oras` in `test-pr.yml`, the `test:oci-*` scripts in `package.json`, `test/README.md`, and `test/src/zcl_abapgit_oci_integration` (adapted to the client's own result type).

It can merge once 5 is in. Reviewers may question adding the fixture test to the default `npm test`. An alternative is to run it only in the integration workflow.

### 9. OCI UI: view, pull, self-update guard

- Add the `WHEN c_type-oci` branch in `zcl_abapgit_gui_chunk_lib` (icon, `oci://` reference, resolved and imported digest via `zif_abapgit_repo_oci`) with its test.
- Add the OCI icon for `type = "oci"` in the `repoCatalog` jump list in `zabapgit_js_common.w3mi.data.js`.
- Add the OCI toolbar (Pull, Diff, Log) in `zcl_abapgit_gui_page_repo_view`. Fill the `CASE get_type( )` blocks that PR 6 prepared.
- Let `git_pull` through the router for OCI repositories.
- Add the OCI case in `zcl_abapgit_services_repo=>check_self_update`/`check_for_restart`. The branch identifies abapGit's own package through the `ZABAPGIT` program. On `main`, `check_self_update` starts with `IF ii_repo->is_offline( ) = abap_true. RETURN.`, which PR 6 turns into a `get_type( )` check. The branch replaced that return with `IF sy-cprog <> 'ZABAPGIT'. RETURN.`, which changes behaviour for offline repositories. Do not carry that over. Keep the offline return and add OCI as its own case.

An OCI repository created through the API (PR 7) becomes usable in the UI, but users still cannot create one there.

### 10. OCI UI: create and remote settings, plus docs

- Add the OCI option in `zcl_abapgit_gui_page_addonline`.
- Show OCI fields in `zcl_abapgit_gui_page_sett_remo` and call `zif_abapgit_repo_oci~set_reference`, which resets the cached remote. Include its tests.
- Add `zif/zcl_abapgit_services_repo=>new_oci`.
- Add `docs/oci-registry-repositories.md`.

This PR is the one that turns the feature on. Hold it until the open validation items are done:

- SHA-256 and HTTP auth/redirects on a minimum-release SAP system.
- A full pull and deserialization of the v1 → v2 fixture.
- The Docker-based `npm run integration`.

If maintainers want it merged earlier, gate it behind the experimental-features setting.

## Suggested order of work

1. Open 1, 2, 3, 4 and 6 now. They are independent and change no behaviour. PR 6 is the largest of them, mostly mechanical and touching about 30 classes.
2. Open 5 once 1, 2, 3 and 4 are merged, then 8 alongside it.
3. Open 7 after 5 and 6.
4. Open 9, then 10, after SAP validation.

Before starting each slice, re-diff that slice's files against current `main`. Repo view, router and services change often upstream.
