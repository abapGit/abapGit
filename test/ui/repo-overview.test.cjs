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
    activeElement: { id: "", tagName: "BODY", nodeName: "BODY" },
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
  return {
    context, helper, rows, opened, saved,
    enter() { listeners.keypress({ keyCode: 13 }); },
    keypress(keyCode) { listeners.keypress({ keyCode }); },
    // Returns whether the key was handled (default prevented)
    keydown(key, extra = {}) {
      let prevented = false;
      listeners.keydown({ key, preventDefault() { prevented = true; }, ...extra });
      return prevented;
    },
    focus(element) { context.document.activeElement = element; }
  };
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

test("arrow keys move the selection within the visible rows", () => {
  const { helper, rows, keydown, saved } = page(["1", "2", "3"]);
  helper.selectRowByIndex(0);
  assert.equal(keydown("ArrowDown"), true);
  assert.equal(keydown("Down"), true); // IE key name
  assert.equal(rows[2].classList.contains("selected"), true);
  keydown("ArrowDown"); // stays on the last row
  assert.equal(helper.selectedRepoKey, "3");
  keydown("ArrowUp");
  assert.equal(helper.selectedRepoKey, "2");
  assert.deepEqual(saved, ["1", "2", "3", "2"]);
});

test("arrow keys scroll the newly selected row into view only when it is off screen", () => {
  const { context, helper, rows, keydown } = page(["1", "2", "3"]);
  context.innerHeight = 100;
  const scrolled = [];
  const tops = [0, 50, 150];
  rows.forEach((row, i) => {
    row.getBoundingClientRect = () => ({ top: tops[i], bottom: tops[i] + 20 });
    row.scrollIntoView = alignToTop => scrolled.push([row.dataset.key, alignToTop]);
  });
  helper.selectRowByIndex(0);
  keydown("ArrowDown");
  keydown("ArrowDown");
  tops[0] = -30;
  keydown("ArrowUp");
  keydown("ArrowUp");
  assert.deepEqual(scrolled, [["3", false], ["1", true]]);
});

test("arrow keys are left alone in fields, menus, open palettes and with modifiers", () => {
  const { context, helper, keydown, focus } = page(["1", "2"]);
  helper.selectRowByIndex(0);
  focus({ nodeName: "INPUT" });
  assert.equal(keydown("ArrowDown"), false);
  focus({ nodeName: "A", parentElement: { nodeName: "LI", parentElement: null } });
  assert.equal(keydown("ArrowDown"), false);
  focus({ nodeName: "BODY" });
  assert.equal(keydown("ArrowDown", { altKey: true }), false);
  assert.equal(keydown("ArrowDown", { defaultPrevented: true }), false);
  context.CommandPalette.isVisible = () => true;
  assert.equal(keydown("ArrowDown"), false);
  assert.equal(helper.selectedRepoKey, "1");
});

test("number keys still move the selection", () => {
  const { helper, keypress } = page(["1", "2"]);
  helper.selectRowByIndex(0);
  keypress(50); // "2"
  assert.equal(helper.selectedRepoKey, "2");
  keypress(56); // "8"
  assert.equal(helper.selectedRepoKey, "1");
});
