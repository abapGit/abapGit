const assert = require('node:assert/strict');
const test = require('node:test');
const loadUi = require('./load-ui.cjs');

function picker(disabled = [], webgui = false) {
  const document = { activeElement: { nodeName: 'BODY' }, documentElement: { clientHeight: 100 }, addEventListener() {},
    querySelector() { return picklist; },
    getElementById(id) { return radios.find(radio => radio.id === id) || null; } };
  let clicks = 0, scrolls = 0;
  const radios = [0, 1, 2].map(index => {
    let checked = false;
    const radio = { id: `radio${index}`, disabled: disabled.includes(index),
      get checked() { return checked; },
      set checked(value) {
        if (value) radios.forEach(other => { if (other !== radio) other.checked = false; });
        checked = value;
      } };
    radio.nextElementSibling = { nodeName: 'LABEL',
      setAttribute() {}, getAttribute() { return radio.id; },
      focus() { document.activeElement = this; },
      click() { radio.checked = true; }, getBoundingClientRect() { return { top: 110, bottom: 120 }; },
      scrollIntoView() { scrolls++; } };
    return radio;
  });
  const picklist = { querySelectorAll() { return radios; },
    contains(element) { return radios.some(radio => radio.nextElementSibling === element); },
    querySelector(selector) {
      if (selector === '.main' || !webgui && selector === 'input[type="submit"].main') {
        return { nodeName: webgui ? 'A' : 'INPUT', click() { clicks++; } };
      }
      return null;
    } };
  const context = loadUi({ document });
  context.CommandPalette.isVisible = () => false;
  context.enablePicklistNavigation();
  function key(key, modifiers = {}) {
    let prevented = false;
    context.gKeyboard.dispatch('keydown', { key, ...modifiers,
      preventDefault() { this.defaultPrevented = prevented = true; } });
    return prevented;
  }
  return { radios, document, context, key, clicks: () => clicks, scrolls: () => scrolls,
    selected: () => radios.findIndex(radio => radio.checked) };
}

test('arrows mark entries without submitting, stop at boundaries, and Enter chooses', () => {
  const p = picker();
  assert.equal(p.key('ArrowDown'), true);
  assert.equal(p.selected(), 0);
  assert.equal(p.document.activeElement, p.radios[0].nextElementSibling);
  p.key('ArrowDown', { repeat: true });
  assert.equal(p.selected(), 1);
  p.key('ArrowDown'); p.key('ArrowDown');
  assert.equal(p.selected(), 2);
  p.key('ArrowUp');
  assert.equal(p.selected(), 1);
  assert.equal(p.clicks(), 0);
  assert.ok(p.scrolls() > 0);
  assert.equal(p.key('Enter'), true);
  assert.equal(p.clicks(), 1);
  p.key('Enter', { repeat: true });
  assert.equal(p.clicks(), 1);
});

test('mouse selection continues with arrows; Up starts at the last entry', () => {
  const p = picker();
  p.key('ArrowUp');
  assert.equal(p.selected(), 2);
  p.radios[0].nextElementSibling.click();
  p.key('Down');
  assert.equal(p.selected(), 1);
  p.key('Up'); p.key('Up');
  assert.equal(p.selected(), 0);
});

test('Tab-focused labels can be marked with Space or chosen with Enter', () => {
  const p = picker();
  p.radios[1].nextElementSibling.focus();
  p.key(' ');
  assert.equal(p.selected(), 1);
  assert.equal(p.clicks(), 0);
  p.radios[2].nextElementSibling.focus();
  p.key('Enter');
  assert.equal(p.selected(), 2);
  assert.equal(p.clicks(), 1);
});

test('disabled entries are skipped and an entirely disabled list is left alone', () => {
  const p = picker([1]);
  p.key('ArrowDown'); p.key('ArrowDown');
  assert.equal(p.selected(), 2);
  assert.equal(picker([0, 1, 2]).key('ArrowDown'), false);
});

test('fields, modified keys, and command palettes retain their keys', () => {
  const p = picker();
  for (const modifier of ['ctrlKey', 'altKey', 'metaKey', 'shiftKey', 'defaultPrevented']) {
    assert.equal(p.key('ArrowDown', { [modifier]: true }), false);
  }
  for (const nodeName of ['INPUT', 'TEXTAREA', 'SELECT', 'BUTTON', 'A']) {
    p.document.activeElement = { nodeName };
    assert.equal(p.key('ArrowDown'), false);
  }
  p.document.activeElement = { nodeName: 'BODY' };
  p.context.CommandPalette.isVisible = () => true;
  assert.equal(p.key('ArrowDown'), false);
  assert.equal(p.selected(), -1);
});

test('an in-page picker accepts arrows from the page behind it; another modal blocks them', () => {
  const p = picker();
  const originalGet = p.document.getElementById;
  p.document.getElementById = id => id === 'modal' ? { contains() { return true; } } : originalGet(id);
  p.document.activeElement = { nodeName: 'A' };
  assert.equal(p.key('ArrowDown'), true);
  assert.equal(p.selected(), 0);
  p.document.getElementById = id => id === 'modal' ? { contains() { return false; } } : originalGet(id);
  assert.equal(p.key('ArrowDown'), false);
  assert.equal(p.selected(), 0);
});

test('pages without a picklist do not register navigation', () => {
  const context = loadUi({ document: { addEventListener() {}, querySelector() { return null; } } });
  const count = (context.gKeyboard.handlers.keydown || []).length;
  context.enablePicklistNavigation();
  assert.equal((context.gKeyboard.handlers.keydown || []).length, count);
});

// Unlike desktop controls, WebGUI renders Choose as a form-submitting link.
test('Enter activates the WebGUI Choose link after arrow navigation', () => {
  const p = picker([], true);
  p.key('ArrowDown');
  p.key('ArrowDown');
  assert.equal(p.key('Enter'), true);
  assert.equal(p.selected(), 1);
  assert.equal(p.clicks(), 1);
  p.key('Enter', { repeat: true });
  assert.equal(p.clicks(), 1);
});
