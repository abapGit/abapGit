const assert = require("node:assert/strict");
const test = require("node:test");

const loadUi = require("./load-ui.cjs");

const EDGE = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36 Edg/120.0.0.0";
const IE = "Mozilla/5.0 (Windows NT 10.0; WOW64; Trident/7.0; rv:11.0) like Gecko";
const CHROME = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36";

// An in-page popup (zcl_abapgit_gui_in_page_modal) over a page. Focus starts on
// the page behind it, as after rendering: nothing in the popup has it yet.
// listeners[name] calls every listener of that event type the popup registered.
function popup(items = ["first", "middle", "last"], userAgent = CHROME, env = {}) {
  const registered = {};
  const listeners = {};
  const document = {
    activeElement: null,
    addEventListener(name, fn) {
      if (!registered[name]) {
        registered[name] = [];
        listeners[name] = event => registered[name].forEach(handler => handler(event));
      }
      registered[name].push(fn);
    },
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
  let modal = { querySelectorAll() { return elements; } };
  const pageLink = element("page link");
  document.activeElement = pageLink;
  const context = loadUi({ document, navigator: { userAgent } });
  context.setEnvironment(env);
  for (const name in registered) { delete registered[name]; delete listeners[name]; } // only what trapFocus registers
  const backActions = [];
  context.triggerSapEventBack = action => backActions.push(action);
  context.trapFocus();
  return {
    tab(shiftKey = false) {
      let prevented = false;
      listeners.keydown({ key: "Tab", keyCode: 9, shiftKey, preventDefault() { prevented = true; } });
      return prevented;
    },
    focus(name) { elements.find(item => item.name === name).focus(); },
    // keydown, keypress and keyup of one key press; returns which were prevented
    press(key, keyCode) {
      return ["keydown", "keypress", "keyup"].map(type => {
        let prevented = false;
        if (listeners[type]) {
          listeners[type]({ type, key, keyCode, preventDefault() { prevented = true; }, stopPropagation() {} });
        }
        return prevented;
      });
    },
    close() { modal = null; },
    get focused() { return document.activeElement.name; },
    backActions,
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

for (const [name, userAgent, env] of [
  ["the Edge control", EDGE, { isSapGuiForWindows: true }],
  ["WebGUI", CHROME, { isWebGui: true }]
]) {
  test(`on ${name} Escape cancels the popup with go_back and keeps the key from SAP GUI`, () => {
    const page = popup(undefined, userAgent, env);
    assert.deepEqual(page.press("Escape", 27), [true, true, true]);
    assert.deepEqual(page.backActions, ["go_back"]);
  });
}

test("a held Escape sends go_back only once", () => {
  const page = popup(undefined, EDGE, { isSapGuiForWindows: true });
  page.press("Escape", 27);
  page.press("Escape", 27);
  page.press("Esc", 27); // the name older controls use
  assert.deepEqual(page.backActions, ["go_back"]);
});

for (const [name, userAgent, env] of [
  ["SAP GUI for Java, which closes the popup itself,", CHROME, {}],
  ["the IE control", IE, { isSapGuiForWindows: true }]
]) {
  test(`on ${name} Escape is left alone`, () => {
    const page = popup(undefined, userAgent, env);
    assert.deepEqual(page.press("Escape", 27), [false, false, false]);
    assert.deepEqual(page.backActions, []);
  });
}

test("other keys than Escape are left alone", () => {
  const page = popup(undefined, EDGE, { isSapGuiForWindows: true });
  assert.deepEqual(page.press("Enter", 13), [false, false, false]);
  assert.deepEqual(page.backActions, []);
});

test("Escape is left alone once the popup is gone", () => {
  const page = popup(undefined, CHROME, { isWebGui: true });
  page.close();
  assert.deepEqual(page.press("Escape", 27), [false, false, false]);
  assert.deepEqual(page.backActions, []);
});
