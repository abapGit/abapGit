const assert = require("node:assert/strict");
const test = require("node:test");

const loadUi = require("./load-ui.cjs");

// Model only the DOM surface used by action discovery and dispatch. The event
// ordering is deliberate: embedded controls can emit popstate during submit.
function page(elements = []) {
  const listeners = {};
  const context = loadUi({
    document: {
      addEventListener() {},
      querySelectorAll(selector) {
        return elements.filter(element => selector.split(", ").some(part => {
          const simple = part.match(/^([a-z]+)(\[[^\]]+\])*$/);
          if (!simple) return false; // No toolbars or nested links in these fixtures
          const tag = simple[1];
          if (element.nodeName !== tag.toUpperCase()) return false;
          const attrs = [...part.matchAll(/\[([^=\]]+)(?:='([^']*)')?\]/g)];
          return attrs.every(([, name, value]) => value === undefined
            ? element.getAttribute(name) !== null : element.getAttribute(name) === value);
        }));
      },
      querySelector() { return null; },
      getElementById(id) { return elements.find(element => element.id === id); }
    },
    history: { pushState() {} },
    addEventListener(name, fn) { listeners[name] = fn; }
  });
  context.popstate = () => listeners.popstate();
  return context;
}

function element(nodeName, attrs = {}) {
  return {
    nodeName,
    type: attrs.type,
    title: attrs.title || "",
    value: attrs.value || "",
    href: attrs.href,
    getAttribute(name) { return attrs[name] === undefined ? null : attrs[name]; },
    classList: { contains(name) { return (attrs.class || "").split(" ").includes(name); } }
  };
}

test("Enter finds and clicks a WebGUI dialog command with a rewritten href", () => {
  const command = element("A", { "data-sapevent": "save", href: "https://host/webgui#" });
  const context = page([command]);
  let clicks = 0;
  let prevented = false;
  command.click = () => { clicks++; assert.equal(context.gSapeventNavPending, true); };
  const hotkeys = new context.Hotkeys({ Enter: "save" });
  hotkeys.onkeydown({ key: "Enter", preventDefault() { prevented = true; } });
  assert.equal(clicks, 1);
  assert.equal(prevented, true);
  assert.match(command.title, /\[Enter\]/);
});

test("WebGUI picker is discoverable by action and by command palette", () => {
  const picker = element("INPUT", { type: "button", "data-sapevent": "choose_package", title: "Package" });
  const unrelated = element("INPUT", { type: "button", title: "Unrelated" });
  const context = page([picker, unrelated]);
  let clicks = 0;
  picker.click = () => { clicks++; assert.equal(context.gSapeventNavPending, true); };
  assert.equal(context.findSapEventElement("choose_package"), picker);
  const commands = context.enumerateUiActions();
  assert.equal(commands.length, 1);
  assert.match(commands[0].title, /Package/);
  commands[0].action();
  assert.equal(clicks, 1);
});

for (const type of ["button", "submit"]) {
  test(`link hints activate ${type} actions with the navigation guard`, () => {
    const input = element("INPUT", { type, "data-sapevent": "choose_package" });
    const context = page();
    let clicks = 0;
    input.focus = () => assert.fail("Action must be clicked, not merely focused");
    input.click = () => { clicks++; assert.equal(context.gSapeventNavPending, true); };
    new context.LinkHints("f").hintActivate({ parent: input });
    assert.equal(clicks, 1);
  });
}

test("link hints guard main submits inheriting the form action", () => {
  const input = element("INPUT", { type: "submit" });
  input.form = element("FORM", { action: "SAPEVENT:save" });
  const context = page();
  input.click = () => assert.equal(context.gSapeventNavPending, true);
  new context.LinkHints("f").hintActivate({ parent: input });
});

test("link hints guard sapevent links but leave ordinary links unarmed", () => {
  for (const href of ["SAPEVENT:go_back", "https://example.org/"]) {
    const anchor = element("A", { href });
    const context = page();
    context.document.location = { href: "https://host/webgui" };
    anchor.click = () => assert.equal(context.gSapeventNavPending, href.startsWith("SAPEVENT:"));
    new context.LinkHints("f").hintActivate({ parent: anchor });
  }
});

test("palette main submit preserves the form and ignores submit-induced popstate", () => {
  const input = element("INPUT", { type: "submit", class: "main", value: "Save" });
  input.formAction = "https://host/current-page";
  const context = page([input]);
  let submits = 0;
  let backs = 0;
  const form = { action: "SAPEVENT:save", elements: [{ name: "message", value: "keep me" }] };
  form.submit = function() {
    assert.equal(this, form);
    assert.equal(this.elements[0].value, "keep me");
    assert.equal(context.gSapeventNavPending, true);
    submits++;
    context.popstate();
  };
  input.form = form;
  context.redirectBrowserBackToSapEvent();
  context.triggerSapEventBack = () => backs++;
  context.enumerateUiActions()[0].action();
  assert.equal(submits, 1);
  assert.equal(backs, 0);
  context.popstate();
  assert.equal(backs, 1);
});

test("submitFormById keeps guarding server-rendered forms", () => {
  const form = { id: "edit_form" };
  const context = page([form]);
  let submits = 0;
  form.submit = () => { submits++; assert.equal(context.gSapeventNavPending, true); };
  context.submitFormById(form.id);
  assert.equal(submits, 1);
});
