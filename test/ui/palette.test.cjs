const assert = require("node:assert/strict");
const test = require("node:test");
const loadUi = require("./load-ui.cjs");

function page(titles = ["Open Repo", "Save", "Open Settings"]) {
  // Model construction and event dispatch, with fixed geometry: these tests
  // exercise selection and actions, not browser layout or scrolling.
  function element(tag) {
    const classes = new Set();
    return {
      nodeName: tag.toUpperCase(), style: {}, children: [], listeners: {},
      value: "", scrollTop: 0,
      classList: {
        add(name) { classes.add(name); },
        remove(name) { classes.delete(name); },
        contains(name) { return classes.has(name); }
      },
      appendChild(child) { child.parentNode = this; this.children.push(child); },
      addEventListener(name, fn) { this.listeners[name] = fn; },
      focus() {},
      getBoundingClientRect() { return { top: 0, bottom: 100, height: 100 }; }
    };
  }
  const context = loadUi({ document: {
    addEventListener() {}, querySelector() { return null; },
    createElement: element, body: element("body")
  } });
  const actions = [];
  const palette = new context.CommandPalette(() => titles.map(title => ({
    title, action() { actions.push(title); }
  })), { toggleKey: "F1", hotkeyDescription: "Commands" });
  palette.toggleDisplay(true);
  // Invoke the registered keyup handler; this fixture does not simulate
  // browser default actions or the preceding keydown/keypress events.
  function key(key) {
    palette.elements.input.listeners.keyup({ key, preventDefault() {} });
  }
  function filter(value) {
    palette.elements.input.value = value;
    key("x");
  }
  function selected() {
    return palette.commands.filter(cmd => cmd.element.classList.contains("selected")).map(cmd => cmd.title);
  }
  return { palette, actions, key, filter, selected };
}

test("Enter after a filter with no matches does not execute the previous selection", () => {
  const { palette, actions, key, filter, selected } = page();
  key("ArrowDown");
  filter("no such command");
  key("Enter");
  assert.deepEqual(actions, []);
  assert.deepEqual(selected(), []);
  assert.equal(palette.getSelected(), undefined);
  assert.equal(palette.elements.palette.style.display, "");
});

for (const [up, down] of [["ArrowUp", "ArrowDown"], ["Up", "Down"]]) {
  test(`${up}/${down} skip hidden commands and stop at list boundaries`, () => {
    const { filter, key, selected } = page();
    filter("open");
    key(up);
    assert.deepEqual(selected(), ["Open Repo"]);
    key(down);
    assert.deepEqual(selected(), ["Open Settings"]);
    key(down);
    assert.deepEqual(selected(), ["Open Settings"]);
    key(up);
    assert.deepEqual(selected(), ["Open Repo"]);
  });
}

test("changing the filter selects the first matching command and Enter executes it once", () => {
  const { palette, actions, key, filter, selected } = page();
  key("ArrowDown");
  key("ArrowDown");
  filter("save");
  assert.deepEqual(selected(), ["Save"]);
  key("Enter");
  assert.deepEqual(actions, ["Save"]);
  assert.equal(palette.elements.palette.style.display, "none");
});

test("clearing a filter restores all titles and selects the first command", () => {
  const { palette, filter, selected } = page();
  filter("save");
  filter("");
  assert.deepEqual(selected(), ["Open Repo"]);
  for (const cmd of palette.commands) {
    assert.equal(cmd.element.style.display, "");
    assert.equal(cmd.titleSpan.innerText, cmd.title);
  }
});

test("selection recovers when a no-match filter is replaced with a match", () => {
  const { filter, selected, key, actions } = page();
  filter("zzzz");
  filter("settings");
  assert.deepEqual(selected(), ["Open Settings"]);
  key("Enter");
  assert.deepEqual(actions, ["Open Settings"]);
});

test("an empty command list tolerates navigation and Enter", () => {
  const { key, actions, selected } = page([]);
  key("ArrowDown");
  key("ArrowUp");
  key("Enter");
  assert.deepEqual(actions, []);
  assert.deepEqual(selected(), []);
});

test("reopening a filtered palette resets both the search field and visible results", () => {
  const { palette, filter, selected } = page();
  filter("save");
  palette.toggleDisplay(false);
  palette.toggleDisplay(true);
  assert.equal(palette.elements.input.value, "");
  assert.deepEqual(palette.commands.map(cmd => cmd.element.style.display), ["", "", ""]);
  assert.deepEqual(selected(), ["Open Repo"]);
});

for (const [title, filter, expected] of [
  ["Open Repo", "or", "<mark>O</mark>pen <mark>R</mark>epo"],
  ["Open Repo", "oR", "<mark>O</mark>pen <mark>R</mark>epo"],
  ["Save", "save", "<mark>S</mark><mark>a</mark><mark>v</mark><mark>e</mark>"],
  ["Open Repo", "", "Open Repo"],
  ["", "", ""],
  ["", "a", null],
  ["Save", "saved", null],
  ["abc", "ca", null],
  ["aba", "aa", "<mark>a</mark>b<mark>a</mark>"]
]) {
  test(`fuzzy matching ${JSON.stringify(title)} with ${JSON.stringify(filter)}`, () => {
    assert.equal(loadUi().fuzzyMatchAndMark(title, filter), expected);
  });
}

test("filtering escapes title markup while preserving generated highlights", () => {
  const title = "Repo <img src=x onerror=alert(1)> & Docs";
  const { palette, filter, key, actions } = page([title]);
  filter("repo");
  assert.equal(palette.commands[0].titleSpan.innerHTML,
    "<mark>R</mark><mark>e</mark><mark>p</mark><mark>o</mark> &lt;img src=x onerror=alert(1)&gt; &amp; Docs");
  key("Enter");
  assert.deepEqual(actions, [title]);
});

test("matching special characters uses the original title, escaping every output segment", () => {
  const { palette, filter } = page(["<A & B>"]);
  filter("&");
  assert.equal(palette.commands[0].titleSpan.innerHTML, "&lt;A <mark>&amp;</mark> B&gt;");
  filter("<>");
  assert.equal(palette.commands[0].titleSpan.innerHTML, "<mark>&lt;</mark>A &amp; B<mark>&gt;</mark>");
  filter("");
  assert.equal(palette.commands[0].titleSpan.innerText, "<A & B>");
});
