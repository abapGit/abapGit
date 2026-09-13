const assert = require("node:assert/strict");
const test = require("node:test");
const loadUi = require("./load-ui.cjs");

function page({ href = "#", submenu = true, parentName = "LI" } = {}) {
  const listeners = {};
  const windowListeners = {};
  const classes = new Set();
  const li = { nodeName: parentName, classList: {
    toggle(name) { if (classes.has(name)) classes.delete(name); else classes.add(name); }
  } };
  const anchor = { nodeName: "A", parentNode: li, parentElement: li,
    href: "https://host/page" + href, getAttribute(name) { return name === "href" ? href : null; },
    nextElementSibling: submenu ? { nodeName: "UL" } : null };
  const icon = { nodeName: "I", parentNode: anchor };
  const span = { nodeName: "SPAN", parentNode: icon };
  const context = loadUi({ document: {
    activeElement: anchor, location: { href: "https://host/page" },
    addEventListener(name, fn) { (listeners[name] ||= []).push(fn); }
  }, history: { pushState() {} },
  addEventListener(name, fn) { windowListeners[name] = fn; } });
  let backs = 0;
  context.redirectBrowserBackToSapEvent();
  context.triggerSapEventBack = () => backs++;
  function click(target, legacy = false) {
    const event = { defaultPrevented: false, preventDefault() { this.defaultPrevented = true; } };
    event[legacy ? "srcElement" : "target"] = target;
    for (const handler of listeners.click || []) handler(event);
    // Simulate the default fragment navigation reaching the Back trap.
    if (!event.defaultPrevented) windowListeners.popstate();
    return event.defaultPrevented;
  }
  return { context, anchor, icon, span, classes, click,
    back() { windowListeners.popstate(); }, get backs() { return backs; } };
}

for (const target of ["anchor", "icon", "span"]) {
  for (const legacy of [false, true]) {
    test(`submenu ${target} click cancels fragment navigation (${legacy ? "srcElement" : "target"})`, () => {
      const p = page();
      assert.equal(p.click(p[target], legacy), true);
      assert.equal(p.click(p[target], legacy), true);
      assert.equal(p.backs, 0);
      p.back();
      assert.equal(p.backs, 1);
    });
  }
}

for (const options of [
  { submenu: false }, { href: "#section" }, { href: "SAPEVENT:go_back" },
  { href: "https://example.org/" }, { parentName: "DIV" }
]) {
  test(`links outside the dummy submenu-heading pattern retain navigation: ${JSON.stringify(options)}`, () => {
    const p = page(options);
    assert.equal(p.click(p.anchor), false);
  });
}

test("submenu heading remains compatible with Enter/Space keyboard toggling", () => {
  const p = page();
  p.anchor.click = () => assert.fail("Keyboard submenu navigation must not follow the link");
  assert.equal(p.anchor.onclick, undefined);
  const navigation = new p.context.KeyNavigation();
  assert.equal(navigation.onEnterOrSpace(), true);
  assert.equal(p.classes.has("force-nav-hover"), true);
  assert.equal(navigation.onEnterOrSpace(), true);
  assert.equal(p.classes.has("force-nav-hover"), false);
});
