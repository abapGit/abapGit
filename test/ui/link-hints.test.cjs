const assert = require('node:assert/strict');
const test = require('node:test');
const loadUi = require('./load-ui.cjs');

function node(nodeName, attrs = {}) {
  const classes = new Set();
  return Object.assign({
    nodeName, children: [], dataset: {},
    classList: {
      add(value) { classes.add(value); },
      remove(value) { classes.delete(value); },
      contains(value) { return classes.has(value); }
    },
    appendChild(child) { this.children.push(child); child.parentNode = this; },
    removeChild(child) { this.children.splice(this.children.indexOf(child), 1); child.parentNode = null; },
    insertAdjacentElement(position, child) { this.appendChild(child); },
    matches(selector) { return selector === ':disabled' && (this.disabled || this.disabledByFieldset); }
  }, attrs);
}

function page(targets) {
  const context = loadUi({ document: {
    addEventListener() {}, activeElement: { nodeName: 'BODY' },
    querySelectorAll() { return targets; },
    createEvent() { return { initEvent() {} }; }, createElement(name) { return node(name.toUpperCase()); }
  } });
  const hints = new context.LinkHints('f');
  const actions = [];
  hints.hintActivate = hint => actions.push(hint.parent);
  context.submitSapeventForm = () => actions.push('clipboard');
  function key(key) { hints.handleKey({ key, preventDefault() {} }); }
  return { context, hints, key, actions };
}

test('partial hint filters labels; reopening removes old containers and includes new targets', () => {
  const targets = Array.from({ length: 12 }, () => node('A'));
  const p = page(targets);
  p.key('f');
  p.key('2');
  assert.equal(p.hints.hintsMap[10].container.classList.contains('nodisplay'), true);
  assert.equal(p.hints.hintsMap[20].pendingSpan.innerText, '2');
  assert.equal(p.hints.hintsMap[20].remainingSpan.innerText, '0');
  p.key('f');
  assert.equal(p.hints.areHintsDisplayed, false);
  targets.push(node('A'));
  p.key('f');
  assert.equal(p.hints.pendingPath, '');
  assert.equal(p.hints.hintsMap.last, 22);
  for (const target of targets) assert.equal(target.children.length, 1);
  p.key('1'); p.key('0');
  assert.deepEqual(p.actions, [targets[0]]);
});

for (const cancel of ['x', 'f']) {
  test(`cancelling copy hints with ${cancel} does not copy on the next activation`, () => {
    const target = node('A', { firstChild: { textContent: 'text' } });
    const p = page([target]);
    p.key('y'); p.key('f'); p.key(cancel);
    assert.equal(p.hints.areHintsDisplayed, false);
    p.key('f'); p.key('1');
    assert.deepEqual(p.actions, [target]);
    assert.equal(p.hints.yankModeActive, false);
  });
}

test('disabled controls, including fieldset descendants, have no hints', () => {
  const targets = [node('INPUT', { disabled: true }), node('INPUT', { disabledByFieldset: true }),
    node('INPUT', { type: 'hidden' }), node('I'), node('A')];
  const p = page(targets);
  p.key('f');
  assert.equal(p.hints.hintsMap.last - p.hints.hintsMap.first + 1, 1);
  assert.equal(p.hints.hintsMap[p.hints.hintsMap.first].parent, targets[4]);
});

test('a checkbox disabled after hint deployment cannot be toggled', () => {
  const p = page([]);
  const target = node('INPUT', { type: 'checkbox', disabled: true, checked: false, parentElement: { dispatchEvent() {} } });
  p.context.LinkHints.prototype.hintActivate.call(p.hints, { parent: target });
  assert.equal(target.checked, false);
});

test('empty pages can open and close hints without stale copy mode', () => {
  const p = page([]);
  p.key('y'); p.key('f'); p.key('1');
  assert.equal(p.hints.areHintsDisplayed, false);
  assert.equal(p.hints.yankModeActive, false);
});

for (const method of ['matches', 'msMatchesSelector']) {
  test(`disabled fieldset controls are guarded using ${method}`, () => {
    const p = page([]);
    const target = node('INPUT', { type: 'checkbox', disabledByFieldset: true, checked: false,
      parentElement: { dispatchEvent() { assert.fail('Disabled control must not dispatch a click'); } } });
    if (method === 'msMatchesSelector') {
      target.msMatchesSelector = target.matches;
      delete target.matches;
    }
    p.context.LinkHints.prototype.hintActivate.call(p.hints, { parent: target });
    assert.equal(target.checked, false);
  });
}

test('enabled checkboxes still toggle with and without a parent click handler', () => {
  const p = page([]);
  for (const handlesClick of [false, true]) {
    let clicks = 0;
    const target = node('INPUT', { type: 'checkbox', checked: false });
    target.parentElement = { dispatchEvent() {
      clicks++;
      if (handlesClick) target.checked = !target.checked;
    } };
    p.context.LinkHints.prototype.hintActivate.call(p.hints, { parent: target });
    assert.equal(target.checked, true);
    assert.equal(clicks, 1);
  }
});

test('cancelling a partial hint closes the dropdown opened by hints', () => {
  const p = page([node('A')]);
  const dropdown = node('LI');
  dropdown.classList.add('force-nav-hover');
  p.hints.activatedDropdown = dropdown;
  p.key('f'); p.key('x');
  assert.equal(dropdown.classList.contains('force-nav-hover'), false);
  assert.equal(p.hints.activatedDropdown, null);
});

test('hint code keys are consumed and the displayed state is visible to page shortcuts', () => {
  const targets = Array.from({ length: 12 }, () => node('A'));
  const p = page(targets);
  const prevented = [];
  const press = key => p.hints.handleKey({ key, preventDefault() { prevented.push(key); } });
  press('f');
  assert.equal(p.context.LinkHints.areHintsDisplayed, true);
  press('2');
  press('9'); // no hint 29: cancels, but the key was still meant for the hints
  assert.deepEqual(prevented, ['2', '9']);
  assert.equal(p.context.LinkHints.areHintsDisplayed, false);
  press('f'); press('1'); press('0');
  assert.deepEqual(prevented, ['2', '9', '1', '0']);
  assert.equal(p.context.LinkHints.areHintsDisplayed, false);
  assert.deepEqual(p.actions, [targets[0]]);
});
