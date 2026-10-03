const assert = require("node:assert/strict");
const test = require("node:test");
const loadUi = require("./load-ui.cjs");

// A page whose document records its listeners; press() sends a key through them
function page(activeElement = { nodeName: "BODY" }) {
  const listeners = {};
  const context = loadUi({ document: {
    activeElement,
    addEventListener(type, fn) { (listeners[type] = listeners[type] || []).push(fn); },
    getElementById() { return null; }
  } });
  function press(type, key, extra = {}) {
    const event = { type, key, preventDefault() { event.defaultPrevented = true; }, ...extra };
    (listeners[type] || []).forEach(fn => fn(event));
    return event;
  }
  return { context, listeners, press };
}

test("handlers run by their order, not by when the page registered them", () => {
  const { context, listeners, press } = page();
  const calls = [];
  const { order } = context.gKeyboard;
  context.gKeyboard.on("keypress", order.hotkeys, () => calls.push("hotkeys"));
  context.gKeyboard.on("keypress", order.page, () => calls.push("page 1"));
  context.gKeyboard.on("keypress", order.linkHints, () => calls.push("link hints"));
  context.gKeyboard.on("keypress", order.page, () => calls.push("page 2"));
  press("keypress", "a");
  assert.deepEqual(calls, ["link hints", "page 1", "page 2", "hotkeys"]);
  assert.equal(listeners.keypress.length, 1);
});

test("keypress and keydown handlers stay apart", () => {
  const { context, press } = page();
  const calls = [];
  context.gKeyboard.on("keydown", context.gKeyboard.order.menus, event => calls.push(event.type));
  press("keypress", "a");
  assert.deepEqual(calls, []);
  press("keydown", "ArrowDown");
  assert.deepEqual(calls, ["keydown"]);
});

// The order that stops the repository overview from moving its selection on
// the digits of a link hint code - now also when its script comes first
test("a link hint code is consumed before the page shortcuts see its digits", () => {
  const { context, press } = page();
  const pageKeys = [];
  context.gKeyboard.on("keypress", context.gKeyboard.order.page, event => {
    if (!event.defaultPrevented) pageKeys.push(event.key);
  });
  const hints = new context.LinkHints("t");
  hints.deployHintContainers = () => ({});
  hints.displayHints = state => { hints.areHintsDisplayed = state; };
  context.gKeyboard.on("keypress", context.gKeyboard.order.linkHints, hints.getHandler());
  press("keypress", "t");
  press("keypress", "2");
  assert.deepEqual(pageKeys, ["t"]);
});

for (const [name, element, typing, arrows] of [
  ["the page", { nodeName: "BODY" }, false, false],
  ["a link", { nodeName: "A" }, false, false],
  ["an input", { nodeName: "INPUT" }, true, true],
  ["a text area", { nodeName: "TEXTAREA" }, true, true],
  ["a dropdown", { nodeName: "SELECT" }, true, true],
  ["editable content", { nodeName: "DIV", isContentEditable: true }, true, true],
  ["a read-only input", { nodeName: "INPUT", readOnly: true }, false, true]
]) {
  test(`with the focus on ${name}, shortcuts are ${typing ? "off" : "on"} and arrows ${arrows ? "stay with it" : "are free"}`, () => {
    const { context } = page(element);
    assert.equal(context.gKeyboard.isTyping(), typing);
    assert.equal(context.gKeyboard.isTakenByField(element, true), arrows);
  });
}

for (const [event, arrow] of [
  [{ key: "ArrowUp" }, -1], [{ key: "Up" }, -1], [{ keyCode: 38 }, -1],
  [{ key: "ArrowDown" }, 1], [{ key: "Down" }, 1], [{ keyCode: 40 }, 1],
  [{ key: "PageDown", keyCode: 34 }, 0], [{ key: "PageUp", keyCode: 33 }, 0], [{ key: "d", keyCode: 68 }, 0]
]) {
  test(`${JSON.stringify(event)} reads as vertical arrow ${arrow}`, () => {
    const { context } = page();
    assert.equal(context.gKeyboard.getVerticalArrow(event), arrow);
  });
}

// KeyNavigation read any key ending in "Down" as the arrow before
test("Page Down leaves the focus in a dropdown menu where it is", () => {
  const { context } = page();
  const navigation = new context.KeyNavigation();
  let moved = false;
  navigation.onArrowDown = () => { moved = true; return true; };
  navigation.onkeydown({ key: "PageDown", keyCode: 34, preventDefault() {} });
  assert.equal(moved, false);
  navigation.onkeydown({ key: "ArrowDown", keyCode: 40, preventDefault() {} });
  assert.equal(moved, true);
});

// The stage page's filter key, "f" by default
function stagePage(activeElement) {
  const p = page(activeElement);
  const focused = [];
  const helper = Object.create(p.context.StageHelper.prototype);
  Object.assign(helper, {
    focusFilterKey: "f",
    ids: { objectSearch: "objectSearch" },
    dom: {
      stageTab: {}, commitBtn: {}, patchBtn: {},
      objectSearch: { id: "objectSearch", focus() { focused.push("filter"); } }
    }
  });
  p.context.addEventListener = () => {};
  helper.setHooks();
  return { ...p, focused };
}

test("the stage filter key focuses the filter", () => {
  const { press, focused } = stagePage({ nodeName: "BODY" });
  const event = press("keypress", "f");
  assert.deepEqual(focused, ["filter"]);
  assert.equal(event.defaultPrevented, true);
});

for (const [name, activeElement, extra] of [
  ["while typing in another field", { nodeName: "INPUT", id: "other" }, {}],
  ["when an earlier handler consumed the key", { nodeName: "BODY" }, { defaultPrevented: true }]
]) {
  test(`the stage filter key is typed as text ${name}`, () => {
    const { press, focused } = stagePage(activeElement);
    press("keypress", "f", extra);
    assert.deepEqual(focused, []);
  });
}

test("a failing handler leaves the later ones running and its error reported", () => {
  const { context, press } = page();
  const calls = [];
  context.gKeyboard.on("keypress", context.gKeyboard.order.linkHints, () => { throw new Error("broken hints"); });
  context.gKeyboard.on("keypress", context.gKeyboard.order.hotkeys, event => calls.push(event.key));
  assert.throws(() => press("keypress", "s"), /broken hints/);
  assert.deepEqual(calls, ["s"]);
});
