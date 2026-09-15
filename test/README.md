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

`cd test/gitea && npm install && npm run gitea && cd ../../ && npm run integration`

## Integration Testing - UI

Run `npm run test:ui` for JavaScript action-dispatch regression tests using
mocked DOM and browser-history objects. These run in PR CI and cover hotkeys,
link hints, command palettes, and the submit navigation guard. They do not
replace testing in SAP GUI for Windows, SAP GUI for Java, and WebGUI.

Playwright, https://playwright.dev

todo, webpack and mocked git/repos?

## Integration Testing - Object Serialization

Ad-hoc via [https://github.com/abapGit/CI](abapGit/CI) tooling
