const assert = require("node:assert/strict");
const test = require("node:test");
const loadUi = require("./load-ui.cjs");

function page({ option = "abap", checked = "X", aux = "extension" } = {}) {
  const listeners = {};
  const menu = {};
  const classes = new Set(["icon", "icon-check", checked === "X" ? "blue" : "grey"]);
  const icon = { tagName: "I", classList: {
    contains(name) { return classes.has(name); },
    add(name) { classes.add(name); }, remove(name) { classes.delete(name); }
  } };
  const attrs = { "data-check": checked, "data-aux": aux };
  const li = { tagName: "LI", getAttribute(name) { return attrs[name] ?? null; },
    setAttribute(name, value) { attrs[name] = value; } };
  const anchor = { tagName: "A", parentNode: li, children: [icon], innerText: option };
  icon.parentNode = anchor;
  const context = loadUi({ document: { addEventListener() {}, getElementById() { return menu; } },
    history: { pushState() {} }, addEventListener(name, fn) { listeners[name] = fn; } });
  const calls = [];
  let backs = 0;
  context.redirectBrowserBackToSapEvent();
  context.triggerSapEventBack = () => backs++;
  new context.CheckListWrapper("diff-filter",
    (...args) => calls.push(["filter", ...args]), (...args) => calls.push(["mine", ...args]));
  function click(target, legacy = false) {
    const event = { defaultPrevented: false, preventDefault() { this.defaultPrevented = true; } };
    event[legacy ? "srcElement" : "target"] = target;
    menu.onclick(event);
    // Model the default fragment navigation after bubbling. A cancelled click
    // must never reach the same popstate listener used for genuine Back.
    if (!event.defaultPrevented) listeners.popstate();
    return event.defaultPrevented;
  }
  return { anchor, icon, li, calls, classes, click, back() { listeners.popstate(); }, get backs() { return backs; } };
}

for (const target of ["anchor", "icon"]) {
  for (const legacy of [false, true]) {
    test(`filter ${target} click cancels navigation and toggles both ways (${legacy ? "srcElement" : "target"})`, () => {
      const p = page();
      assert.equal(p.click(p[target], legacy), true);
      assert.equal(p.li.getAttribute("data-check"), "");
      assert.equal(p.classes.has("grey"), true);
      assert.equal(p.click(p[target], legacy), true);
      assert.equal(p.li.getAttribute("data-check"), "X");
      assert.equal(p.classes.has("blue"), true);
      assert.deepEqual(p.calls, [["filter", "extension", "abap", false], ["filter", "extension", "abap", true]]);
      assert.equal(p.backs, 0);
      p.back();
      assert.equal(p.backs, 1);
    });
  }
}

test("Only my changes cancels navigation and dispatches its own callback", () => {
  const p = page({ option: "Only my changes", checked: "", aux: "DEVELOPER" });
  assert.equal(p.click(p.icon), true);
  assert.deepEqual(p.calls, [["mine", "DEVELOPER", true]]);
  assert.equal(p.backs, 0);
});

test("ordinary links without a checkbox retain their default navigation", () => {
  const p = page({ checked: null });
  assert.equal(p.click(p.anchor), false);
  assert.deepEqual(p.calls, []);
});
