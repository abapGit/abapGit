const assert = require("node:assert/strict");
const test = require("node:test");

const loadUi = require("./load-ui.cjs");

const EDGE = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36 Edg/120.0.0.0";
const IE = "Mozilla/5.0 (Windows NT 10.0; WOW64; Trident/7.0; rv:11.0) like Gecko";
const CHROME = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36";

const EDGE_CONTROL = ["the Edge control", EDGE, { isSapGuiForWindows: true }];
const WEBGUI = ["WebGUI", CHROME, { isWebGui: true }];

function element(tag) {
  const classes = new Set();
  return {
    nodeName: tag.toUpperCase(), style: {}, children: [], listeners: {}, dataset: {},
    value: "", scrollTop: 0,
    classList: {
      add(name) { classes.add(name); },
      remove(name) { classes.delete(name); },
      contains(name) { return classes.has(name); }
    },
    appendChild(child) { child.parentNode = this; this.children.push(child); },
    removeChild(child) { this.children.splice(this.children.indexOf(child), 1); child.parentNode = null; },
    insertAdjacentElement(position, child) { this.appendChild(child); },
    addEventListener(name, fn) { this.listeners[name] = fn; },
    matches() { return false; },
    focus() {},
    blur() {},
    contains(node) { for (; node; node = node.parentNode) if (node === this) return true; return false; },
    getBoundingClientRect() { return { top: 0, bottom: 100, height: 100 }; }
  };
}

// A page with a command palette and link hints, as zcl_abapgit_gui_page sets
// them up: the environment first, then the link hints, then the palettes.
function page(userAgent, env, { withBack = false } = {}) {
  const targets = Array.from({ length: 12 }, () => element("A")); // two-digit hint codes
  const context = loadUi({
    navigator: { userAgent },
    document: {
      addEventListener() {},
      activeElement: { nodeName: "BODY" },
      getElementById() { return null; },
      querySelector() { return null; },
      querySelectorAll() { return targets; },
      createEvent() { return { initEvent() {} }; },
      createElement: element,
      body: element("body")
    },
    history: { pushState() {} }
  });
  context.setEnvironment(env);
  // Escape as Back, as every abapGit page sets it up (escape.test.cjs)
  const backActions = [];
  if (withBack) {
    context.redirectBrowserBackToSapEvent();
    context.triggerSapEventBack = action => backActions.push(action);
  }
  context.activateLinkHints("f");
  const palette = new context.CommandPalette(() => [{ title: "Save", action() {} }],
    { toggleKey: "F1", hotkeyDescription: "Commands" });

  // keydown, keypress and keyup of one key press, through the page-wide
  // dispatcher; returns which were prevented
  function press(key, keyCode, repeat = false) {
    return ["keydown", "keypress", "keyup"].map(type => {
      let prevented = false;
      if (context.gKeyboard.handlers[type]) {
        context.gKeyboard.dispatch(type, { type, key, keyCode, repeat: type === "keydown" && repeat,
          get defaultPrevented() { return prevented; },
          preventDefault() { prevented = true; } });
      }
      return prevented;
    });
  }
  return {
    context, palette, press, backActions,
    get paletteOpen() { return context.CommandPalette.isVisible(); },
    get hintsShown() { return context.LinkHints.areHintsDisplayed; }
  };
}

for (const [name, userAgent, env] of [EDGE_CONTROL, WEBGUI]) {
  test(`on ${name} Escape closes the command palette and keeps the key from SAP GUI`, () => {
    const current = page(userAgent, env);
    current.palette.toggleDisplay(true);
    assert.deepEqual(current.press("Escape", 27), [true, true, true]);
    assert.equal(current.paletteOpen, false);
  });

  test(`on ${name} Escape hides the link hints and keeps the key from SAP GUI`, () => {
    const current = page(userAgent, env);
    current.press("f", 70);
    assert.equal(current.hintsShown, true);
    assert.deepEqual(current.press("Escape", 27), [true, true, true]);
    assert.equal(current.hintsShown, false);
  });
}

test("Escape also drops a half-typed hint code and the yank mode", () => {
  const current = page(...EDGE_CONTROL.slice(1));
  const hints = current.context.LinkHints.instance;
  current.press("y", 89); // yank mode, then the hints
  current.press("f", 70);
  current.press("1", 49); // half of a two-digit code
  assert.equal(hints.pendingPath, "1");
  assert.equal(hints.yankModeActive, true);
  current.press("Escape", 27);
  assert.equal(current.hintsShown, false);
  assert.equal(hints.pendingPath, "");
  assert.equal(hints.yankModeActive, false);
  current.press("f", 70); // the next activation starts afresh
  assert.equal(current.hintsShown, true);
  assert.equal(hints.pendingPath, "");
});

test("Escape with nothing open is left to the Back handling", () => {
  const current = page(...WEBGUI.slice(1));
  assert.deepEqual(current.press("Escape", 27), [false, false, false]);
});

for (const [name, userAgent, env] of [EDGE_CONTROL, WEBGUI]) {
  test(`on ${name} Escape closing the palette does not also go back, the next one does`, () => {
    const current = page(userAgent, env, { withBack: true });
    current.palette.toggleDisplay(true);
    assert.deepEqual(current.press("Escape", 27), [true, true, true]);
    assert.equal(current.paletteOpen, false);
    assert.deepEqual(current.backActions, []);
    current.press("Escape", 27);
    assert.deepEqual(current.backActions, ["go_back"]);
  });

  test(`on ${name} Escape hiding the link hints does not also go back`, () => {
    const current = page(userAgent, env, { withBack: true });
    current.press("f", 70);
    current.press("Escape", 27);
    assert.equal(current.hintsShown, false);
    assert.deepEqual(current.backActions, []);
  });
}

test("an Escape held after closing the palette does not go back", () => {
  const current = page(...EDGE_CONTROL.slice(1), { withBack: true });
  current.palette.toggleDisplay(true);
  current.press("Escape", 27);
  assert.deepEqual(current.press("Escape", 27, true), [true, true, true]);
  assert.deepEqual(current.backActions, []);
});

for (const [name, userAgent, env] of [
  ["SAP GUI for Java", CHROME, {}],
  ["the IE control", IE, { isSapGuiForWindows: true }]
]) {
  test(`on ${name} Escape is left alone, also with the palette open`, () => {
    const current = page(userAgent, env);
    current.palette.toggleDisplay(true);
    assert.deepEqual(current.press("Escape", 27), [false, false, false]);
    assert.equal(current.paletteOpen, true);
  });
}

test("other keys than Escape do not close the palette", () => {
  const current = page(...EDGE_CONTROL.slice(1));
  current.palette.toggleDisplay(true);
  current.press("Enter", 13);
  assert.equal(current.paletteOpen, true);
});
