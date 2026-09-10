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
  return { helper, rows, opened, saved, enter() { listeners.keypress({ keyCode: 13 }); } };
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
