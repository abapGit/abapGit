const assert = require("node:assert/strict");
const test = require("node:test");
const loadUi = require("./load-ui.cjs");

function memoryStorage(initial = {}) {
  const items = { ...initial };
  return {
    get length() { return Object.keys(items).length; },
    key(index) { return Object.keys(items)[index]; },
    getItem(key) { return key in items ? items[key] : null; },
    setItem(key, value) { items[key] = String(value); },
    removeItem(key) { delete items[key]; },
    items
  };
}

test("storage check reports working storage without leaving data behind", () => {
  const context = loadUi({ location: { href: "file:///tmp/<page>.html" } });
  context.localStorage = memoryStorage({ state: "1" });
  context.sessionStorage = memoryStorage();
  const html = context.describeBrowserStorage();
  assert.match(html, /Page URL:<\/td><td>file:\/\/\/tmp\/&lt;page&gt;\.html/);
  assert.match(html, /localStorage:<\/td><td>read\/write OK, 1 entries</);
  assert.match(html, /sessionStorage:<\/td><td>read\/write OK, 0 entries</);
  assert.deepEqual({ ...context.localStorage.items }, { state: "1" });
  assert.deepEqual({ ...context.sessionStorage.items }, {});
});

test("storage check reports missing and throwing storage", () => {
  const context = loadUi();
  assert.doesNotMatch(context.describeBrowserStorage(), /<th>/); // no entry table without entries
  // e.g. storage exists but writing is refused
  context.localStorage = { length: 0, setItem() { throw Object.assign(Error("denied"), { name: "SecurityError" }); } };
  const html = context.describeBrowserStorage();
  assert.match(html, /localStorage:<\/td><td>error: SecurityError/);
  assert.match(html, /sessionStorage:<\/td><td>not available/);
});

test("storage check lists entries sorted with escaped, truncated values", () => {
  const context = loadUi();
  context.localStorage = memoryStorage({ zState: "x".repeat(250), aState: '{"key":"<1>"}' });
  const html = context.describeBrowserStorage();
  const keys = [...html.matchAll(/<tr><td>localStorage<\/td><td>([^<]+)<\/td>/g)].map(match => match[1]);
  assert.deepEqual(keys, ["aState", "zState"]);
  assert.match(html, /<td>aState<\/td><td>13<\/td><td><code>\{"key":"&lt;1&gt;"\}<\/code>/);
  assert.match(html, new RegExp(`<td>zState</td><td>250</td><td><code>${"x".repeat(200)}\u2026</code>`));
});
