const assert = require("node:assert/strict");
const test = require("node:test");

const loadUi = require("./load-ui.cjs");

const EDGE = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36 Edg/120.0.0.0";
const IE = "Mozilla/5.0 (Windows NT 10.0; WOW64; Trident/7.0; rv:11.0) like Gecko";
const CHROME = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36";

const EDGE_CONTROL = ["the Edge control", EDGE, { isSapGuiForWindows: true }];
const WEBGUI = ["WebGUI", CHROME, { isWebGui: true }];

// A page with the Back handling every abapGit page sets up. What is open in it
// (an in-page popup, the hotkey overview, a menu opened by keyboard) is set
// through `dom`.
function page(userAgent, env) {
  const dom = { modal: null, hotkeys: null, openMenu: null };
  const context = loadUi({
    navigator: { userAgent },
    document: {
      addEventListener() {},
      getElementById(id) { return { modal: dom.modal, hotkeys: dom.hotkeys }[id] || null; },
      querySelector(selector) { return selector === ".force-nav-hover" ? dom.openMenu : null; },
      querySelectorAll() { return []; }
    },
    history: { pushState() {} }
  });
  context.setEnvironment(env);
  context.redirectBrowserBackToSapEvent();
  const backActions = [];
  context.triggerSapEventBack = action => backActions.push(action);
  return {
    context,
    dom,
    backActions,
    // keydown, keypress and keyup of one key press; returns which were prevented
    press(key, keyCode) {
      return ["keydown", "keypress", "keyup"].map(type => {
        let prevented = false;
        if (context.gKeyboard.handlers[type]) {
          context.gKeyboard.dispatch(type, { type, key, keyCode,
            get defaultPrevented() { return prevented; },
            preventDefault() { prevented = true; }, stopPropagation() {} });
        }
        return prevented;
      });
    }
  };
}

for (const [name, userAgent, env] of [EDGE_CONTROL, WEBGUI]) {
  test(`on ${name} Escape goes back and keeps the key from SAP GUI`, () => {
    const { press, backActions } = page(userAgent, env);
    assert.deepEqual(press("Escape", 27), [true, true, true]);
    assert.deepEqual(backActions, ["go_back"]);
  });

  test(`on ${name} Escape cancels an in-page popup with the same go_back`, () => {
    const { press, dom, backActions } = page(userAgent, env);
    dom.modal = {};
    assert.deepEqual(press("Escape", 27), [true, true, true]);
    assert.deepEqual(backActions, ["go_back"]);
  });
}

test("a held Escape goes back only once", () => {
  const { press, backActions } = page(...EDGE_CONTROL.slice(1));
  press("Escape", 27);
  press("Escape", 27);
  press("Esc", 27); // the name older controls use
  assert.deepEqual(backActions, ["go_back"]);
});

test("Escape goes back while typing in a field, as in SAP GUI for Java", () => {
  const { press, context, backActions } = page(...WEBGUI.slice(1));
  context.document.activeElement = { nodeName: "INPUT", readOnly: false };
  assert.deepEqual(press("Escape", 27), [true, true, true]);
  assert.deepEqual(backActions, ["go_back"]);
});

for (const [name, userAgent, env] of [
  ["SAP GUI for Java, which goes back itself,", CHROME, {}],
  ["the IE control", IE, { isSapGuiForWindows: true }]
]) {
  test(`on ${name} Escape is left alone`, () => {
    const { press, backActions } = page(userAgent, env);
    assert.deepEqual(press("Escape", 27), [false, false, false]);
    assert.deepEqual(backActions, []);
  });
}

for (const [what, open] of [
  ["a command palette", ({ context }) => context.CommandPalette.instances.push(
    { elements: { palette: { style: { display: "" } } } })],
  ["link hints", ({ context }) => { context.LinkHints.areHintsDisplayed = true; }],
  ["the hotkey overview", ({ dom }) => { dom.hotkeys = { style: { display: "" } }; }],
  ["a menu opened by keyboard", ({ dom }) => { dom.openMenu = {}; }]
]) {
  test(`Escape does not leave the page while ${what} is open`, () => {
    const current = page(...EDGE_CONTROL.slice(1));
    open(current);
    assert.deepEqual(current.press("Escape", 27), [false, false, false]);
    assert.deepEqual(current.backActions, []);
  });
}

test("a hidden hotkey overview does not keep Escape from going back", () => {
  const { press, dom, backActions } = page(...WEBGUI.slice(1));
  dom.hotkeys = { style: { display: "none" } };
  press("Escape", 27);
  assert.deepEqual(backActions, ["go_back"]);
});

test("other keys than Escape are left alone", () => {
  const { press, backActions } = page(...EDGE_CONTROL.slice(1));
  assert.deepEqual(press("Enter", 13), [false, false, false]);
  assert.deepEqual(backActions, []);
});
