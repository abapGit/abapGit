const assert = require('node:assert/strict');
const test = require('node:test');
const loadUi = require('./load-ui.cjs');

function menu() {
  const document = { addEventListener() {}, location: { href: 'https://host/page' } };
  const context = loadUi({ document });
  let clicks = 0;
  function item(href) {
    const classes = new Set();
    const li = { nodeName: 'LI', classList: {
      contains(value) { return classes.has(value); },
      remove(value) { classes.delete(value); },
      toggle(value) { if (classes.has(value)) classes.delete(value); else classes.add(value); }
    } };
    li.firstElementChild = { nodeName: 'A', href, parentElement: li,
      focus() { document.activeElement = this; }, click() { clicks++; } };
    return li;
  }
  const dropdown = item('https://host/page#');
  const first = item('SAPEVENT:first'), last = item('SAPEVENT:last');
  const ul = { nodeName: 'UL', firstElementChild: first, parentElement: dropdown };
  first.parentElement = last.parentElement = ul;
  first.nextElementSibling = last; last.previousElementSibling = first;
  dropdown.firstElementChild.nextElementSibling = ul;
  document.activeElement = dropdown.firstElementChild;
  const navigation = new context.KeyNavigation();
  function key(key, modifiers = {}) {
    let prevented = false;
    navigation.onkeydown({ key, ...modifiers, preventDefault() { prevented = true; } });
    return prevented;
  }
  return { document, dropdown, first, last, key, clicks: () => clicks };
}

for (const modifier of ['ctrlKey', 'altKey', 'metaKey', 'defaultPrevented']) {
  test(`menu navigation preserves ${modifier} events`, () => {
    const p = menu();
    for (const key of ['Enter', ' ', 'ArrowDown', 'ArrowUp', 'Backspace']) {
      assert.equal(p.key(key, { [modifier]: true }), false);
    }
    assert.equal(p.dropdown.classList.contains('force-nav-hover'), false);
    assert.equal(p.document.activeElement, p.dropdown.firstElementChild);
    p.first.firstElementChild.focus();
    assert.equal(p.key('Enter', { [modifier]: true }), false);
    assert.equal(p.clicks(), 0);
  });
}

test('keyboard opens dropdown, navigates its items, activates links and returns to parent', () => {
  const p = menu();
  assert.equal(p.key('Enter'), true);
  assert.equal(p.dropdown.classList.contains('force-nav-hover'), true);
  assert.equal(p.key('ArrowDown'), true);
  assert.equal(p.document.activeElement, p.first.firstElementChild);
  assert.equal(p.key('ArrowUp'), false);
  assert.equal(p.key('ArrowDown'), true);
  assert.equal(p.document.activeElement, p.last.firstElementChild);
  assert.equal(p.key('ArrowDown'), false);
  assert.equal(p.key('ArrowUp'), true);
  assert.equal(p.key(' '), true);
  assert.equal(p.clicks(), 1);
  assert.equal(p.key('Backspace'), true);
  assert.equal(p.document.activeElement, p.dropdown.firstElementChild);
  assert.equal(p.dropdown.classList.contains('force-nav-hover'), false);
});

test('editing controls retain their keyboard behavior', () => {
  const p = menu();
  p.document.activeElement = { nodeName: 'TEXTAREA' };
  for (const key of ['Enter', ' ', 'ArrowDown', 'ArrowUp', 'Backspace']) {
    assert.equal(p.key(key), false);
  }
  assert.equal(p.clicks(), 0);
});
