const assert = require("node:assert/strict");
const test = require("node:test");
const loadUi = require("./load-ui.cjs");

function page(sessionStorage) {
  const html = { scrollTop: 0 };
  const context = loadUi({
    document: { addEventListener() {}, querySelector() { return html } },
    sessionStorage: sessionStorage
  });
  return { context, html };
}

test("scroll position survives a round trip and is cleared after restoring", () => {
  const store = {};
  const { context, html } = page({
    setItem(key, value) { store[key] = String(value); },
    getItem(key) { return key in store ? store[key] : null; }
  });

  html.scrollTop = 128;
  context.saveScrollPosition();
  // Stored unquoted, so a build that predates the helpers reads it back unchanged.
  assert.equal(store.scrollTop, "128");

  html.scrollTop = 0;
  context.restoreScrollPosition();
  assert.equal(html.scrollTop, "128");
  assert.equal(store.scrollTop, "0");
});

for (const failure of ["access", "read", "write"]) {
  test(`refresh still runs when session storage ${failure} fails`, () => {
    const throwing = {
      getItem() { if (failure === "read") throw Error("denied"); return null },
      setItem() { if (failure === "write") throw Error("full") }
    };
    const { context } = page(failure === "access" ? undefined : throwing);
    if (failure === "access") {
      Object.defineProperty(context, "sessionStorage", { get() { throw Error("denied") } });
    }

    let ran = 0;
    const refresh = context.memorizeScrollPosition(function() { ran++; return "submitted" });
    assert.equal(refresh(), "submitted");
    assert.equal(ran, 1);
    context.restoreScrollPosition();
  });
}
