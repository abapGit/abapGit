const assert = require("node:assert/strict");
const test = require("node:test");
const loadUi = require("./load-ui.cjs");

function page() {
  const documentListeners = {};
  const windowListeners = {};
  const ids = {};
  const names = {};
  const scrolled = [];
  let backs = 0;
  let pushes = 0;
  const context = loadUi({ document: {
    addEventListener(type, fn) { (documentListeners[type] ||= []).push(fn); },
    getElementById(id) { return ids[id] || null; },
    getElementsByName(name) { return names[name] || []; }
  }, addEventListener(type, fn) { windowListeners[type] = fn; },
  history: { pushState() { pushes++; } } });
  context.redirectBrowserBackToSapEvent();
  context.triggerSapEventBack = () => backs++;
  function link(href, attrs = {}) {
    attrs = { href, ...attrs };
    return { nodeName: "A", getAttribute(name) { return attrs[name] ?? null; },
      hasAttribute(name) { return name in attrs; } };
  }
  function click(anchor, { nested = false, legacy = false, ownHandler, cancelled = false } = {}) {
    const e = { defaultPrevented: cancelled, preventDefault() { this.defaultPrevented = true; } };
    const target = nested ? { nodeName: "SPAN", parentNode: { nodeName: "I", parentNode: anchor } } : anchor;
    e[legacy ? "srcElement" : "target"] = target;
    if (ownHandler) ownHandler(e);
    for (const handler of documentListeners.click || []) handler(e);
    return e;
  }
  function followFragment(anchor, options) {
    const e = click(anchor, options);
    if (!e.defaultPrevented) windowListeners.popstate();
    return e;
  }
  function destination(id, named = false) {
    const element = { scrollIntoView() { scrolled.push(id); } };
    if (named) names[id] = [element]; else ids[id] = element;
  }
  return { context, link, click, followFragment, destination, scrolled,
    back() { windowListeners.popstate(); }, get backs() { return backs; }, get pushes() { return pushes; } };
}

for (const nested of [false, true]) {
  for (const legacy of [false, true]) {
    test(`dummy clicks run their handler without navigating (nested=${nested}, legacy=${legacy})`, () => {
      const p = page();
      let toggles = 0;
      const opts = { nested, legacy, ownHandler() { toggles++; } };
      p.followFragment(p.link("#"), opts);
      p.followFragment(p.link("#"), opts);
      assert.equal(toggles, 2);
      assert.equal(p.backs, 0);
      assert.equal(p.pushes, 1);
      assert.deepEqual(p.scrolled, []);
      p.back();
      assert.equal(p.backs, 1);
    });
  }
}

test("submenu headings need no special markup or keyboard initialization", () => {
  const p = page();
  assert.equal(p.followFragment(p.link("#")).defaultPrevented, true);
  assert.equal(p.backs, 0);
});

for (const [href, id, named] of [
  ["#top", "top"], ["#stage-details", "stage-details"],
  ["#a%20%26%20b", "a & b"], ["#%C3%A4", "ä"], ["#100%", "100%"],
  ["#legacy", "legacy", true]
]) {
  test(`local fragment ${href} scrolls without changing history`, () => {
    const p = page();
    p.destination(id, named);
    assert.equal(p.followFragment(p.link(href)).defaultPrevented, true);
    assert.deepEqual(p.scrolled, [id]);
    assert.equal(p.backs, 0);
    assert.equal(p.pushes, 1);
    p.back();
    assert.equal(p.backs, 1);
  });
}

test("missing fragment targets do not navigate back or throw", () => {
  const p = page();
  p.followFragment(p.link("#missing"));
  assert.equal(p.backs, 0);
  assert.deepEqual(p.scrolled, []);
});

for (const href of ["SAPEVENT:go_back", "file:///SAPEVENT:save", "sap-cust://sap-place-holder/SAPEVENT:save",
  "https://example.org/#top", "other.html#top", "#sapevent25"]) {
  test(`routing remains untouched for ${href}`, () => {
    const p = page();
    assert.equal(p.click(p.link(href)).defaultPrevented, false);
  });
}

test("ITS markers and original href protect rewritten event links", () => {
  const p = page();
  assert.equal(p.click(p.link("#rewritten", { "data-sapevent": "save" })).defaultPrevented, false);
  const anchor = p.link("#rewritten");
  anchor.hrefsav = "SAPEVENT:save";
  assert.equal(p.click(anchor).defaultPrevented, false);
});

test("dummy form links submit once, consume submit popstate, and suppress the extra fragment event", () => {
  const p = page();
  let submits = 0;
  const form = { submit() { submits++; p.back(); } };
  const anchor = p.link("#", { "data-sapevent": "save" });
  p.followFragment(anchor, { ownHandler() { p.context.submitForm(form); } });
  assert.equal(submits, 1);
  assert.equal(p.backs, 0);
  p.back();
  assert.equal(p.backs, 1);
});

test("a click already cancelled by its handler is left alone", () => {
  const p = page();
  p.destination("top");
  p.click(p.link("#top"), { ownHandler(e) { e.preventDefault(); } });
  assert.deepEqual(p.scrolled, []);
});

for (const attrs of [{ target: "_blank" }, { target: "other-frame" }, { download: "" }]) {
  test(`explicit alternate navigation is preserved: ${JSON.stringify(attrs)}`, () => {
    const p = page();
    assert.equal(p.click(p.link("#top", attrs)).defaultPrevented, false);
  });
}

test("non-links and anchors without href are ignored", () => {
  const p = page();
  assert.equal(p.click({ nodeName: "BUTTON" }).defaultPrevented, false);
  assert.equal(p.click(p.link(null)).defaultPrevented, false);
});

test("message toggle runs before dummy navigation is suppressed", () => {
  const p = page();
  const panel = { style: { display: "" } };
  p.context.document.getElementById = () => panel;
  p.followFragment(p.link("#"), { ownHandler() { p.context.toggleDisplay("message"); } });
  assert.equal(panel.style.display, "none");
  assert.equal(p.backs, 0);
});

test("diff Jump keeps its own scroll behavior without a subsequent Back", () => {
  const p = page();
  let jumps = 0;
  p.context.document.querySelector = () => ({ scrollIntoView() { jumps++; } });
  p.context.setTimeout = fn => fn();
  const anchor = p.link("#");
  anchor.text = "/src/file.abap";
  const helper = Object.create(p.context.DiffHelper.prototype);
  p.followFragment(anchor, { ownHandler(e) { helper.onJump(e); } });
  assert.equal(jumps, 1);
  assert.equal(p.backs, 0);
});
