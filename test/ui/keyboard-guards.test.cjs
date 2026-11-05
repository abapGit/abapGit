const assert = require('node:assert/strict');
const test = require('node:test');
const loadUi = require('./load-ui.cjs');

function page(activeElement = { nodeName: 'BODY' }) {
  const context = loadUi({ document: { addEventListener() {}, activeElement } });
  let actions = 0;
  const hotkeys = Object.create(context.Hotkeys.prototype);
  hotkeys.oKeyMap = { s() { actions++; } };
  const hints = Object.create(context.LinkHints.prototype);
  Object.assign(hints, { linkHintHotKey: 'f', areHintsDisplayed: false, yankModeActive: false });
  return { context, hotkeys, hints, actions: () => actions };
}

for (const modifier of ['ctrlKey', 'altKey', 'metaKey', 'defaultPrevented']) {
  test(`${modifier} preserves browser shortcuts and link-hint state`, () => {
    const p = page();
    p.hotkeys.onkeydown({ key: 's', [modifier]: true });
    p.hints.handleKey({ key: 'y', [modifier]: true });
    assert.equal(p.actions(), 0);
    assert.equal(p.hints.yankModeActive, false);
  });
}

for (const active of [{ nodeName: 'INPUT' }, { nodeName: 'TEXTAREA' },
  { nodeName: 'SELECT' }, { nodeName: 'SPAN', isContentEditable: true }]) {
  test(`typing in ${active.nodeName} leaves hotkeys and yank mode alone`, () => {
    const p = page(active);
    p.hotkeys.onkeydown({ key: 's' });
    p.hints.handleKey({ key: 'y' });
    assert.equal(p.actions(), 0);
    assert.equal(p.hints.yankModeActive, false);
  });
}

for (const active of [{ nodeName: 'BODY' }, { nodeName: 'INPUT', readOnly: true }]) {
  test(`plain shortcuts still work from ${active.nodeName}`, () => {
    const p = page(active);
    p.hotkeys.onkeydown({ key: 's' });
    p.hints.handleKey({ key: 'y' });
    assert.equal(p.actions(), 1);
    assert.equal(p.hints.yankModeActive, true);
  });
}

test('hint selection still yanks only when deliberately enabled', () => {
  const p = page();
  let clicked = 0, copied;
  p.context.submitSapeventForm = (params, action) => { copied = { ...params, action }; };
  const hint = { parent: { nodeName: 'A', cloneNode() { return { textContent: 'link text', querySelectorAll: () => [] }; } } };
  p.hints.hintsMap = { '1': hint };
  p.hints.hintActivate = () => clicked++;
  p.hints.displayHints = state => { p.hints.areHintsDisplayed = state; };
  p.hints.handleKey({ key: 'y' });
  p.hints.pendingPath = '';
  p.hints.areHintsDisplayed = true;
  p.hints.handleKey({ key: '1', preventDefault() {} });
  assert.deepEqual(copied, { clipboard: 'link text', action: 'clipboard' });
  assert.equal(clicked, 0);
  assert.equal(p.hints.yankModeActive, false);
  p.hints.pendingPath = '';
  p.hints.areHintsDisplayed = true;
  p.hints.handleKey({ key: '1', preventDefault() {} });
  assert.equal(clicked, 1);
});
