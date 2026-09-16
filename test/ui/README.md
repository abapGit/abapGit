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
| [`checklist.test.cjs`](checklist.test.cjs) | Filter clicks and cancellation of fragment navigation |
| [`diff.test.cjs`](diff.test.cjs) | Combined filters, staging visible files, and exact-path jumps |
| [`fragments.test.cjs`](fragments.test.cjs) | Local fragment handling and browser Back interactions |
| [`patch.test.cjs`](patch.test.cjs) | File and section selection boundaries and patch submission |
| [`repo-overview.test.cjs`](repo-overview.test.cjs) | Repository selection and persisted state |
| [`scroll.test.cjs`](scroll.test.cjs) | Scroll restoration and unavailable storage |
| [`source-viewer.test.cjs`](source-viewer.test.cjs) | Asset loading, response ordering, caching, failures/retries, line numbers, and IE fallbacks |

To measure the shipped JavaScript with Node's V8 coverage:

```sh
node --test --experimental-test-coverage test/ui/*.test.cjs
```

The VM loader supplies the source filename so coverage includes `common.js`,
instead of omitting the anonymous evaluated script. Check the script's row in
the report, not an aggregate that might also include test fixtures.

Priority gaps for further regression tests:

- Global hotkeys: Ctrl/Alt/Meta combinations, selects, and editable content.
- Link hints: ordinary typing must not change yank mode; partial hints and
  cancelling/reopening must leave predictable state.
- Diff text selection: table-background clicks, empty selections, copying from
  unrelated tables, and left/right/unified columns.
- Keyboard menu navigation and modal focus: disabled/hidden controls, dropdown
  boundaries, and focus restoration.
- Source viewer DOM lifecycle: opening/closing, keyboard-handler removal, and
  repeated openings. Asset-loading tests do not cover these interactions.

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
