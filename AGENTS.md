# Agent guide

abapGit is a Git client for ABAP. These instructions apply to any coding agent
working in this repository.

## Local setup

- Read `AGENTS-LOCAL.md` if present. It supplements this guide with local tools,
  SAP connections, preferences, and verified workflows. Its absence is normal.
- Read `CONTRIBUTING.md` before contributing. Use `package.json`,
  `abaplint.json`, and `.github/workflows/` as the source of truth for checks.
- Use Node.js 22 or newer and run `npm install` from the repository root.
  Local lint, build, and transpiled unit tests do not require a SAP connection.
- Keep machine-specific instructions in `AGENTS-LOCAL.md`. Keep credentials
  and local connection files out of Git and command output.

## Project layout

- `src/`: ABAP source, object metadata, ABAP Unit tests, and embedded UI assets.
- `src/objects/`: SAP object serializers and deserializers.
- `src/ui/`: ABAP UI code and shipped JavaScript/CSS assets.
- `test/`: transpiler setup, test support, JavaScript UI tests, and integration tests.
- `ci/`: checks for the generated standalone program.
- `deps/`: supporting ABAP definitions.

## Development rules

- Inspect the working tree before editing; preserve unrelated and uncommitted work.
- Keep changes focused on the requested behavior and follow nearby code conventions.
- Preserve ABAP 7.02 compatibility as configured in `abaplint.json`. Access to
  newer SAP APIs must follow the project's existing compatibility patterns.
- Edit source and metadata together where needed. Do not hand-edit generated
  `zabapgit.abap`, `output/`, or downloaded `lint_deps/` to implement a source change.
- For serialization changes, consider deserialization, round trips, stable diffs,
  and compatibility with existing repositories and supported SAP releases.
- For UI changes, account for SAP GUI for Windows, SAP GUI for Java, and WebGUI;
  passing mocked JavaScript tests does not establish browser-control compatibility.
- Add or update focused regression tests for behavior changes. Do not weaken
  checks or rewrite unrelated code to make a test pass.

## Validation

Run the checks appropriate to the change; CI runs the same checks:

| Command | Purpose |
| --- | --- |
| `npm test` | ESLint, JavaScript UI tests, and ABAP lint |
| `npm run unit` | Build/transpile and run local ABAP unit tests |
| `npm run merge && npm run merge.ci` | Generate and lint the standalone program |
| `npm run integration` | Git integration tests; requires the Gitea setup in `test/README.md` |

For focused checks, use `npm run eslint`, `npm run test:ui`, or
`npm run abaplint`. Documentation-only changes normally need only a diff review.
Report what was checked and any failures or unavailable checks; do not describe
transpiled tests as tests run on SAP.

## Working with SAP

- Use the connection and tooling instructions in `AGENTS-LOCAL.md`; do not
  assume a particular system, account, MCP server, or CLI is available.
- Before changing SAP, verify the system, client, package, and repository.
  The local Git branch and the branch selected in SAP are separate state.
- Distinguish branch selection from pulling/importing and activation. Preserve
  local SAP changes unless their overwrite is authorized.
- Verify the resulting SAP state after writes. Report remaining differences,
  inactive objects, and test results separately from a successful HTTP response.

## Handoff

Summarize the behavior changed, relevant validation, and remaining limitations.
Do not commit, push, or open a pull request unless the task calls for it.
