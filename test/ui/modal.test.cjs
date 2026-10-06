const assert = require("node:assert/strict");
const test = require("node:test");

const loadUi = require("./load-ui.cjs");

// An in-page popup (zcl_abapgit_gui_in_page_modal) over a page. Focus starts on
// the page behind it, as after rendering: nothing in the popup has it yet.
function popup(items = ["first", "middle", "last"]) {
  const listeners = {};
  const document = {
    activeElement: null,
    addEventListener(name, fn) { listeners[name] = fn; },
    getElementById(id) { return id === "modal" ? modal : null; }
  };
  const element = (name, attrs = {}) => ({
    name,
    disabled: Boolean(attrs.disabled),
    offsetWidth: attrs.hidden ? 0 : 10,
    offsetHeight: attrs.hidden ? 0 : 10,
    getClientRects() { return attrs.hidden ? [] : [{}]; },
    getAttribute(attr) { return attrs[attr] === undefined ? null : attrs[attr]; },
    focus() { document.activeElement = this; }
  });
  const elements = items.map(item => typeof item === "string" ? element(item) : element(item.name, item));
  const modal = { querySelectorAll() { return elements; } };
  const pageLink = element("page link");
  document.activeElement = pageLink;
  const context = loadUi({ document });
  for (const name in listeners) delete listeners[name]; // only what trapFocus registers
  context.trapFocus();
  return {
    tab(shiftKey = false) {
      let prevented = false;
      listeners.keydown({ key: "Tab", keyCode: 9, shiftKey, preventDefault() { prevented = true; } });
      return prevented;
    },
    focus(name) { elements.find(item => item.name === name).focus(); },
    get focused() { return document.activeElement.name; },
    listeners
  };
}

test("Tab from the page behind the popup goes to the popup's first control", () => {
  const page = popup();
  assert.equal(page.tab(), true);
  assert.equal(page.focused, "first");
});

test("Shift+Tab from the page behind the popup goes to the popup's last control", () => {
  const page = popup();
  assert.equal(page.tab(true), true);
  assert.equal(page.focused, "last");
});

test("Tab wraps around at both ends of the popup", () => {
  const page = popup();
  page.focus("last");
  assert.equal(page.tab(), true);
  assert.equal(page.focused, "first");
  assert.equal(page.tab(true), true);
  assert.equal(page.focused, "last");
});

test("Tab inside the popup is left to the browser", () => {
  const page = popup();
  page.focus("middle");
  assert.equal(page.tab(), false);
  assert.equal(page.tab(true), false);
});

test("hidden, disabled and tabindex=-1 controls are not tab stops", () => {
  const page = popup([{ name: "hidden", hidden: true }, "first", { name: "submit", tabindex: "-1" },
    "last", { name: "disabled", disabled: true }]);
  assert.equal(page.tab(), true);
  assert.equal(page.focused, "first");
  page.focus("last");
  assert.equal(page.tab(), true);
  assert.equal(page.focused, "first");
});

test("a page without a popup gets no focus trap", () => {
  const listeners = {};
  const context = loadUi({ document: { addEventListener(name, fn) { listeners[name] = fn; },
    getElementById() { return null; } } });
  for (const name in listeners) delete listeners[name];
  context.trapFocus();
  assert.equal(listeners.keydown, undefined);
});
