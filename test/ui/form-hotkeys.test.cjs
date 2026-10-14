const assert = require("node:assert/strict");
const test = require("node:test");
const loadUi = require("./load-ui.cjs");

function page({ webgui = false, field = "TEXTAREA", main = true, disabled = false } = {}) {
  let clicks = 0;
  let prevented = 0;
  const form = {
    nodeName: "FORM",
    getAttribute(name) { return name === "action" ? "SAPEVENT:save" : null; },
    querySelector() { return main ? command : null; }
  };
  const command = {
    nodeName: webgui ? "A" : "INPUT",
    type: webgui ? undefined : "submit",
    form: webgui ? undefined : form,
    parentNode: form,
    disabled,
    title: "Save",
    getAttribute(name) { return webgui && name === "data-sapevent" ? "save" : null; },
    click() { clicks++; assert.equal(context.gSapeventNavPending, true); }
  };
  const context = loadUi({ document: {
    addEventListener() {},
    activeElement: field === "A" ? command : { nodeName: field, form: field === "BODY" ? undefined : form },
    querySelector() { return command; },
    querySelectorAll() { return webgui ? [command] : []; }
  } });
  context.setTimeout = () => {};
  context.setKeyBindings({ Enter: "save", "^Enter": "save" });
  return {
    context, form, command,
    key(type = "keydown", extra = {}) {
      context.gKeyboard.dispatch(type, {
        type, key: "Enter", ctrlKey: true,
        preventDefault() { prevented++; }, ...extra
      });
    },
    clicks: () => clicks,
    prevented: () => prevented
  };
}

for (const webgui of [false, true]) {
  for (const field of ["INPUT", "TEXTAREA", "SELECT", "BODY", "A"]) {
    test(`Ctrl+Enter clicks the primary ${webgui ? "WebGUI link" : "submit input"} from ${field} once`, () => {
      const p = page({ webgui, field });
      p.key();
      p.key("keydown", { repeat: true });
      p.key("keypress");
      assert.equal(p.clicks(), 1);
      assert.equal(p.prevented(), 1);
      if (webgui) assert.match(p.command.title, /\[Ctrl\+Enter\]/);
    });
  }
}

test("Ctrl+Enter uses the focused form even when another form appears first", () => {
  const p = page();
  p.context.document.querySelector = () => assert.fail("Must use the focused form");
  p.key();
  assert.equal(p.clicks(), 1);
});

for (const active of [
  { nodeName: "INPUT" },
  { nodeName: "INPUT", readOnly: true },
  { nodeName: "SPAN", isContentEditable: true },
  { nodeName: "INPUT", form: { querySelector() { return null; } } }
]) {
  test(`Ctrl+Enter ignores unrelated ${active.nodeName} fields`, () => {
    const p = page();
    p.context.document.activeElement = active;
    p.key();
    assert.equal(p.clicks(), 0);
    assert.equal(p.prevented(), 0);
  });
}

for (const options of [{ main: false }, { disabled: true }]) {
  test(`Ctrl+Enter ignores ${options.disabled ? "disabled" : "missing"} primary commands`, () => {
    const p = page(options);
    p.key();
    assert.equal(p.clicks(), 0);
  });
}

for (const modifier of ["shiftKey", "altKey", "metaKey", "defaultPrevented"]) {
  test(`Ctrl+Enter ignores ${modifier}`, () => {
    const p = page();
    p.key("keydown", { [modifier]: true });
    assert.equal(p.clicks(), 0);
  });
}

test("a visible command palette blocks Ctrl+Enter", () => {
  const p = page();
  p.context.CommandPalette.instances = [{ elements: { palette: { style: { display: "" } } } }];
  p.key();
  assert.equal(p.clicks(), 0);
});

test("Enter retains its field behavior and Ctrl+Enter does not alias repository navigation", () => {
  const p = page();
  p.key("keypress", { ctrlKey: false });
  assert.equal(p.clicks(), 0);
  const hotkeys = new p.context.Hotkeys({ Enter: "save" });
  p.context.gKeyboard.handlers = {};
  p.context.gKeyboard.on("keydown", 0, hotkeys.onkeydown.bind(hotkeys));
  p.key();
  assert.equal(p.clicks(), 0);
});
