# abapGit Testing

## Unit testing
Part of `/src/`

* Harmless, no changes to the system
* No network connectivity required

Run manually on ABAP system

Run locally on Node.js 14+ via `npm run unit`

Runs automatically for every push, not a required status check

## Integration Testing - Git Protocol

`ZCL_ABAPGIT_INTEGRATION_GIT`

Not installed on systems, edit in vscode or copy pasta

* Gitea docker image, https://hub.docker.com/r/gitea/gitea

`npm run integration`

Option to leave Gitea running?

## Integration Testing - UI
Playwright, https://playwright.dev

webpack and mocked git/repos?

## Integration Testing - Object Serialization
https://github.com/abapGit/CI

## GitHub Actions Setup
todo
