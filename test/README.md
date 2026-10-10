# abapGit Testing

## Unit testing
Part of `/src/`

* Harmless, no changes to the system
* No network connectivity required

Run manually on ABAP system, or run locally on Node.js 22+ via `npm install && npm run unit`

Runs automatically for every push, not a required status check

## Integration Testing - Git Protocol

`ZCL_ABAPGIT_INTEGRATION_GIT`

Note that the integration tests are not installed on systems, edit in vscode or copy pasta

`cd test/gitea && npm install && npm run gitea && cd ../.. && bash test/oci/run-integration.sh`

The Gitea setup also starts a local HTTPS OCI registry on port 5443 and pushes
the checked-in v1/v2 ORAS fixtures to the `team/library` repository. Install
Docker, OpenSSL, ORAS 1.2.3, and Playwright before running it. The integration
command trusts the temporary self-signed certificate only for this Node test
process; it runs the critical Gitea and OCI client tests against the local
services.

To run only the OCI client smoke test without Docker, build first with
`npm run build`, then run `npm run test:oci-integration`. It starts a temporary
in-process HTTPS registry on `127.0.0.1:5443`, serves the checked-in ORAS
fixtures, and runs `ZCL_ABAPGIT_OCI_INTEGRATION`. OpenSSL is needed only if the
temporary test certificate has not already been generated. Stop any other
registry using port 5443 first. This does not replace the full Gitea integration
test or SAP pull/deserialization validation.

## Integration Testing - UI

Run `npm run test:ui` for JavaScript action-dispatch regression tests using
mocked DOM and browser-history objects. These run in PR CI and cover hotkeys,
link hints, command palettes, and the submit navigation guard. They do not
replace testing in SAP GUI for Windows, SAP GUI for Java, and WebGUI.

Playwright, https://playwright.dev

todo, webpack and mocked git/repos?

## Integration Testing - Object Serialization

Ad-hoc via [https://github.com/abapGit/CI](abapGit/CI) tooling
