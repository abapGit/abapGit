const assert = require("node:assert/strict");
const test = require("node:test");
const loadUi = require("./load-ui.cjs");

function page(keys = []) {
  const rows = keys.map(key => {
    const classes = new Set();
    return { dataset: { key }, classList: {
      add(name) { classes.add(name); },
      remove(name) { classes.delete(name); },
      contains(name) { return classes.has(name); }
    } };
  });
  const listeners = {};
  const opened = [];
  const saved = [];
  const context = loadUi({ document: {
    activeElement: { id: "", tagName: "BODY" },
    addEventListener(name, handler) { listeners[name] = handler; },
    querySelectorAll() { return rows; },
    querySelector(selector) {
      const selected = rows.find(row => row.classList.contains("selected"));
      if (selector === ".repo-overview tr.selected") return selected || null;
      if (selector === ".repo-overview tr.selected td.ro-go a") {
        return selected ? { click() { opened.push(selected.dataset.key); } } : null;
      }
      const key = selector.match(/\[data-key='([^']+)'\]/);
      assert.ok(key, selector);
      return rows.find(row => row.dataset.key === key[1]) || null;
    }
  } });
  // Exercise selection and keyboard handling without constructing toolbar DOM.
  const helper = Object.create(context.RepoOverViewHelper.prototype);
  helper.updateActionLinks = () => {};
  helper.saveLocalStorage = () => saved.push(helper.selectedRepoKey);
  helper.registerKeyboardShortcuts();
  return { context, helper, rows, opened, saved, enter() { listeners.keypress({ keyCode: 13 }); } };
}

test("empty repository lists tolerate initial selection and Enter", () => {
  const { helper, enter, opened, saved } = page();
  helper.selectRowByIndex(0);
  enter();
  assert.deepEqual(opened, []);
  assert.deepEqual(saved, []);
});

test("out-of-range indexes preserve the selected repository", () => {
  const { helper, rows, saved } = page(["1", "2"]);
  helper.selectRowByIndex(0);
  for (const index of [-1, 2, 3]) helper.selectRowByIndex(index);
  assert.equal(rows[0].classList.contains("selected"), true);
  assert.equal(helper.selectedRepoKey, "1");
  assert.deepEqual(saved, ["1"]);
});

test("Enter without selection does nothing; selecting the last row opens it", () => {
  const { helper, enter, opened } = page(["1", "2"]);
  enter();
  assert.deepEqual(opened, []);
  helper.selectRowByIndex(0);
  helper.selectRowByIndex(1);
  enter();
  assert.deepEqual(opened, ["2"]);
});

// Registers the keyboard shortcuts once more to get at the keypress handler
function keypress(p) {
  let handler;
  p.context.document.addEventListener = (name, fn) => { if (name === "keypress") handler = fn; };
  p.helper.registerKeyboardShortcuts();
  return (keyCode, event = {}) => handler({ keyCode, key: String.fromCharCode(keyCode), ...event });
}

test("digit 2 selects the next repository", () => {
  const p = page(["1", "2"]);
  const { helper } = p, press = keypress(p);
  helper.selectRowByIndex(0);
  press(50);
  assert.equal(helper.selectedRepoKey, "2");
});

// Typing hint "26" must not move the selection first: the "2" would retarget
// the action links before the hint activates one of them.
for (const [name, arrange] of [
  ["a link hint code is typed", context => { context.LinkHints.areHintsDisplayed = true; }],
  ["the command palette input has focus", context => { context.document.activeElement = { nodeName: "INPUT", tagName: "INPUT" }; }]
]) {
  test(`digits leave the repository selection alone while ${name}`, () => {
    const p = page(["1", "2", "3"]);
    const { context, helper, saved } = p, press = keypress(p);
    helper.selectRowByIndex(0);
    arrange(context);
    press(50);
    press(56);
    assert.equal(helper.selectedRepoKey, "1");
    assert.deepEqual(saved, ["1"]);
  });
}

test("keys consumed by an earlier handler leave the repository selection alone", () => {
  const p = page(["1", "2"]);
  const { helper, opened } = p, press = keypress(p);
  helper.selectRowByIndex(0);
  press(50, { defaultPrevented: true });
  press(13, { defaultPrevented: true });
  assert.equal(helper.selectedRepoKey, "1");
  assert.deepEqual(opened, []);
});

test("restoring a removed repository key leaves the current selection intact", () => {
  const { helper, saved } = page(["1"]);
  helper.selectRowByRepoKey("missing");
  assert.deepEqual(saved, []);
  helper.selectRowByRepoKey("1");
  helper.selectRowByRepoKey("missing");
  assert.equal(helper.selectedRepoKey, "1");
  assert.deepEqual(saved, ["1"]);
});

for (const raw of [null, "{broken", "[]", "42", '{"selectedRepoKey":"missing"}', '{"selectedRepoKey":"a\'b"}']) {
  test(`invalid or stale saved repository state falls back to the first row: ${raw}`, () => {
    const { context, helper, rows } = page(["1", "2"]);
    context.localStorage = { getItem() { return raw; } };
    helper.onPageLoad();
    assert.equal(rows[0].classList.contains("selected"), true);
  });
}

test("saved repository selection and detail preference are restored", () => {
  const { context, helper, rows } = page(["1", "2"]);
  context.localStorage = { getItem() { return '{"selectedRepoKey":"2","isDetailsDisplayed":true}'; } };
  let details;
  helper.toggleItemsDetail = value => { details = value; };
  helper.onPageLoad();
  assert.equal(rows[1].classList.contains("selected"), true);
  assert.equal(rows[0].classList.contains("selected"), false);
  assert.equal(details, true);
});

for (const failure of ["access", "read", "write"]) {
  test(`repository selection and opening survive storage ${failure} failure`, () => {
    const { context, helper, rows, opened } = page(["1", "2"]);
    Object.defineProperty(context, "localStorage", { get() {
      if (failure === "access") throw Error("denied");
      return {
        getItem() { if (failure === "read") throw Error("denied"); return null; },
        setItem() { if (failure === "write") throw Error("full"); }
      };
    } });
    delete helper.saveLocalStorage;
    helper.onPageLoad();
    helper.selectRowByIndex(1);
    helper.openSelectedRepo();
    assert.equal(rows[1].classList.contains("selected"), true);
    assert.deepEqual(opened, ["2"]);
  });
}

test("action links keep their raw sapevent href when the repository key is swapped", () => {
  const attrs = { href: "sapevent:go_stage?key=#" };
  const classes = new Set(["action_link", "action_online_repo"]);
  const link = {
    // A normalizing href property, as in the SAP GUI for Java browser control
    get href() { return "sapevent://" + attrs.href.slice("sapevent:".length).replace("?", "/?"); },
    set href(value) { attrs.href = value; },
    getAttribute(name) { return attrs[name] === undefined ? null : attrs[name]; },
    setAttribute(name, value) { attrs[name] = value; },
    classList: { contains(name) { return classes.has(name); } },
    parentElement: { classList: { add() {}, remove() {} } }
  };
  const context = loadUi({ document: {
    addEventListener() {},
    querySelectorAll(selector) { assert.equal(selector, "a.action_link"); return [link]; }
  } });
  context.RepoOverViewHelper.prototype.updateActionLinks({ dataset: { key: "42", offline: "" } });
  assert.equal(attrs.href, "sapevent:go_stage?key=42");
});
