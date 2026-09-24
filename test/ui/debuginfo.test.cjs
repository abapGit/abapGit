const assert = require("node:assert/strict");
const test = require("node:test");
const loadUi = require("./load-ui.cjs");

function memoryStorage(initial = {}) {
  const items = { ...initial };
  return {
    get length() { return Object.keys(items).length; },
    getItem(key) { return key in items ? items[key] : null; },
    setItem(key, value) { items[key] = String(value); }
  };
}

test("storage check reports working storage and the previous check time", () => {
  const context = loadUi({ location: { href: "file:///tmp/<page>.html" } });
  context.localStorage = memoryStorage({ abapGitStorageCheck: "yesterday" });
  context.sessionStorage = memoryStorage();
  const html = context.describeBrowserStorage();
  assert.match(html, /Page URL:<\/td><td>file:\/\/\/tmp\/&lt;page&gt;\.html/);
  assert.match(html, /localStorage:<\/td><td>read\/write OK, 1 entries, previous check: yesterday/);
  assert.match(html, /sessionStorage:<\/td><td>read\/write OK, 1 entries, previous check: none/);
});

test("storage check reports missing and throwing storage", () => {
  const context = loadUi();
  context.localStorage = { getItem() { throw Object.assign(Error("denied"), { name: "SecurityError" }); } };
  const html = context.describeBrowserStorage();
  assert.match(html, /localStorage:<\/td><td>error: SecurityError/);
  assert.match(html, /sessionStorage:<\/td><td>not available/);
});
