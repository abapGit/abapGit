# JavaScript UI tests

These tests use Node's built-in test runner and require no SAP system or browser.
Run them from the repository root:

```sh
npm run test:ui
```

To show individual test results on Node versions that support disabling test
isolation:

```sh
node --test --test-isolation=none --test-reporter=spec test/ui/*.test.cjs
```

`npm test` runs ESLint, these UI tests, and ABAP lint. ABAP lint may need network
access to fetch its configured dependency.

## Test setup

[`load-ui.cjs`](load-ui.cjs) evaluates the shipped
[`zabapgit_js_common.w3mi.data.js`](../../src/ui/zabapgit_js_common.w3mi.data.js)
in a fresh VM context for each call. The context supplies a browser-like `window`
global; each test provides the DOM methods and other globals it needs.

Add tests in `*.test.cjs` files using `node:test` and `node:assert/strict`. Keep DOM
fixtures small and specific to the behavior under test. Assert observable results
such as dispatched actions, submitted fields, visible commands, and selection.
Exercise the shipped functions rather than copying their implementation into tests.
Avoid expanding the fixtures into a general-purpose browser emulator.

## Coverage

| File | Behavior covered |
| --- | --- |
| [`sapevents.test.cjs`](sapevents.test.cjs) | Action discovery, keyboard and link-hint activation, and navigation guards |
| [`forms.test.cjs`](forms.test.cjs) | Desktop and WebGUI form routing, parameter encoding, repeated submissions, and preservation of existing fields |
| [`stage.test.cjs`](stage.test.cjs) | Commit and patch actions, selection/filter precedence, visible-file staging, and selection counts |
| [`palette.test.cjs`](palette.test.cjs) | Filtering, keyboard navigation, command execution, reopening, and fuzzy matching |

Important regression expectations:

- Explicit staging selections take precedence over filters, even when selected
  files are hidden by the filter.
- A staging filter with no matches must not fall back to committing everything.
- A command palette with no matches must clear its selection so Enter does nothing.
- Reopening the palette must reset both the search field and the active filter.

## Limits

The fixtures model only the DOM operations needed by each test. They do not verify
browser layout, scrolling geometry, native event ordering, or actual SAP GUI/WebGUI
integration. Changes that depend on those behaviors still need testing in the
relevant browser control.
