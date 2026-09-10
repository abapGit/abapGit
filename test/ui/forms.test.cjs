const assert = require("node:assert/strict");
const test = require("node:test");
const loadUi = require("./load-ui.cjs");

function field(attrs = {}) {
  return {
    attrs: { ...attrs },
    value: attrs.value,
    setAttribute(name, value) {
      this.attrs[name] = String(value);
      if (name === "value") this.value = String(value);
    }
  };
}

// Only the form/field operations used by submitSapeventForm are modeled here.
function page({ action = "SAPEVENT:old", fields = [], webgui = false, global = true, prefix = "" } = {}) {
  const submissions = [];
  const form = {
    id: "global_sapevent_form",
    attrs: { action },
    elements: fields,
    getAttribute(name) { return this.attrs[name] ?? null; },
    setAttribute(name, value) { this.attrs[name] = value; },
    querySelectorAll(selector) {
      if (selector === "input[name='PARAMS']") return this.elements.filter(f => f.attrs.name === "PARAMS");
      assert.equal(selector, "input[data-sapevent-field]");
      return this.elements.filter(f => "data-sapevent-field" in f.attrs);
    },
    appendChild(input) { input.parentNode = this; this.elements.push(input); },
    removeChild(input) { this.elements.splice(this.elements.indexOf(input), 1); },
    submit() {
      // Every submit has to arm the browser-back guard, not just the first one:
      // consume the flag so a later submit that forgot to set it is caught too.
      assert.equal(context.gSapeventNavPending, true);
      context.gSapeventNavPending = false;
      submissions.push({
        action: this.attrs.action,
        method: this.attrs.method,
        fields: this.elements.map(f => ({ name: f.attrs.name, value: f.value }))
      });
    }
  };
  fields.forEach(f => { f.parentNode = form; });
  const context = loadUi({ document: {
    addEventListener() {},
    getElementById(id) { return global && id === form.id ? form : null; },
    querySelector() { return null; },
    createElement(tag) {
      assert.equal(tag, "input");
      return field();
    },
    body: { appendChild(node) { assert.equal(node, form); } }
  } });
  context.setEnvironment({ isWebGui: webgui });
  context.gSapeventPrefix = prefix;
  return { context, form, submissions };
}

for (const prefix of ["", "file:///", "sap-cust://sap-place-holder/"]) {
  test(`desktop submission uses the configured prefix ${JSON.stringify(prefix)}`, () => {
    const { context, submissions } = page({ prefix });
    context.submitSapeventForm({ message: "a & b" }, "save");
    assert.deepEqual(submissions, [{
      action: prefix + "SAPEVENT:save", method: "post",
      fields: [{ name: "message", value: "a & b" }]
    }]);
  });
}

test("an explicit desktop sapevent URL is preserved", () => {
  const { context, submissions } = page({ prefix: "file:///" });
  context.submitSapeventForm({}, "sap-cust://sap-place-holder/SAPEVENT:save?key=1");
  assert.equal(submissions[0].action, "sap-cust://sap-place-holder/SAPEVENT:save?key=1");
});

test("WebGUI updates every PARAMS field and preserves ITS routing fields and action", () => {
  const fields = [field({ name: "PARAMS", value: "old" }), field({ name: "PARAMS", value: "other" }),
    field({ name: "~control", value: "116" }), field({ name: "~event", value: "OnSAPEvent" })];
  const { context, submissions } = page({ webgui: true, action: "https://host/webgui", fields });
  context.submitSapeventForm({ message: "keep me" }, "save?key=a&other=b");
  assert.deepEqual(submissions[0], {
    action: "https://host/webgui", method: "post",
    fields: [
      { name: "PARAMS", value: "save?key=a&other=b" },
      { name: "PARAMS", value: "save?key=a&other=b" },
      { name: "~control", value: "116" }, { name: "~event", value: "OnSAPEvent" },
      { name: "message", value: "keep me" }
    ]
  });
});

test("WebGUI action routing encodes the event without changing the routing prefix", () => {
  const routing = "https://host/webgui?~control=116&~event=OnSAPEvent&ALINK=1&frameName=&PARAMS=";
  const { context, submissions } = page({ webgui: true, action: routing + "old" });
  context.submitSapeventForm({}, "save?key=100%&value=a+b#c");
  assert.equal(submissions[0].action, routing + "save?key=100%25%26value=a%2Bb%23c");
});

for (const action of ["search", "search?repo=1"]) {
  test(`GET parameters survive WebGUI POST conversion for ${action}`, () => {
    const { context, submissions } = page({ webgui: true, fields: [field({ name: "PARAMS", value: "old" })] });
    context.submitSapeventForm({ "search term": "a &+#%ä" }, action, "GET");
    assert.deepEqual(submissions[0].fields, [{
      name: "PARAMS", value: action + (action.includes("?") ? "&" : "?") + "search%20term=a%20%26%2B%23%25%C3%A4"
    }]);
    assert.equal(submissions[0].method, "post");
  });
}

test("reusing the global form removes only previously generated fields", () => {
  const { context, submissions } = page({ webgui: true, fields: [
    field({ name: "PARAMS", value: "old" }), field({ name: "token", value: "keep" })
  ] });
  context.submitSapeventForm({ filterValue: "first" }, "stage_filter");
  context.submitSapeventForm({ filterValue: "second" }, "stage_filter");
  context.submitSapeventForm({}, "go_back");
  assert.deepEqual(submissions.map(s => s.fields), [
    [{ name: "PARAMS", value: "stage_filter" }, { name: "token", value: "keep" }, { name: "filterValue", value: "first" }],
    [{ name: "PARAMS", value: "stage_filter" }, { name: "token", value: "keep" }, { name: "filterValue", value: "second" }],
    [{ name: "PARAMS", value: "go_back" }, { name: "token", value: "keep" }]
  ]);
});

test("an explicitly supplied ITS form keeps user fields and normalizes valueless elements", () => {
  const { context, form, submissions } = page({ webgui: true, global: false, fields: [
    field({ name: "PARAMS", value: "old" }), field({ name: "message", value: "keep me" }), field()
  ] });
  context.submitSapeventForm({}, "save", "post", form);
  assert.deepEqual(submissions[0].fields, [
    { name: "PARAMS", value: "save" }, { name: "message", value: "keep me" }, { name: undefined, value: "" }
  ]);
});
