const assert = require('node:assert/strict');
const test = require('node:test');
const loadUi = require('./load-ui.cjs');

function node(tagName, classes = []) {
  const set = new Set(classes);
  return { tagName, nodeType: 1, classList: { contains: name => set.has(name), add: name => set.add(name), remove: name => set.delete(name) },
    contains(child) { for (; child; child = child.parentElement) if (child === this) return true; return false; } };
}
function page(side = 'diff_left', patch = false) {
  const table = node('TABLE'), tbody = node('TBODY'), row = node('TR');
  const cell = node('TD', [side]), text = { nodeType: 3, parentNode: cell, parentElement: cell };
  cell.cellIndex = side === 'diff_unified' ? 3 : (side === 'diff_left' ? 2 : 5) + (patch ? 1 : 0);
  tbody.parentElement = table; row.parentElement = tbody; cell.parentElement = row;
  row.cells = [node('TD', patch ? ['patch'] : [])];
  const fragment = { textContent: 'selected code', querySelectorAll() { return []; } };
  const range = { startContainer: text, endContainer: text, cloneContents() { return fragment; } };
  const selection = { rangeCount: 1, isCollapsed: false, getRangeAt() { return range; }, removeAllRanges() {} };
  const context = loadUi({ getSelection: () => selection,
    document: { addEventListener() {}, body: {}, getSelection: () => selection } });
  const helper = new context.DiffColumnSelection();
  helper.mousedownEventListener({ button: 0, target: text });
  let copied, cancelled = false;
  const event = { target: cell, clipboardData: { setData(type, value) { copied = { type, value }; } },
    preventDefault() { cancelled = true; } };
  return { helper, context, table, tbody, cell, range, selection, fragment, event,
    copied: () => copied, cancelled: () => cancelled };
}

for (const [side, patch, index] of [['diff_left', false, 2], ['diff_right', false, 5],
  ['diff_left', true, 3], ['diff_right', true, 6], ['diff_unified', false, 3]]) {
  test(`valid ${side} selection (patch=${patch}) copies code`, () => {
    const p = page(side, patch);
    assert.equal(p.helper.selectedColumnIdx, index);
    p.helper.copyEventListener(p.event);
    assert.deepEqual(p.copied(), { type: 'text', value: 'selected code' });
    assert.equal(p.cancelled(), true);
  });
}

for (const remote of [false, true]) for (const right of [false, true]) for (const patch of [false, true]) {
  test(`split block copying: remote=${remote}, right=${right}, patch=${patch}`, () => {
    // The ABAP renderer swaps old/new cells when the remote file leads changes,
    // keeping diff_left on new and diff_right on old regardless of their position.
    const kind = remote === right ? 'new' : 'old';
    const p = page(kind === 'new' ? 'diff_left' : 'diff_right', patch);
    const index = (right ? 5 : 2) + (patch ? 1 : 0);
    p.cell.cellIndex = index;
    p.helper.mousedownEventListener({ button: 0, target: p.cell });
    function code(text, type) { return { ...node('TD', [type]), textContent: text }; }
    const rows = ['first', 'second', 'last'].map(text => {
      const old = code(text + ' old', 'old'), newer = code(text + ' new', 'new');
      const cells = [node('TD'), node('TD'), remote ? old : newer,
        node('TD'), node('TD'), remote ? newer : old];
      if (patch) cells.unshift(node('TD', ['patch']));
      return { cells };
    });
    // cloneContents omits cells preceding the start and following the end.
    rows[0].cells = rows[0].cells.slice(index);
    rows[2].cells = rows[2].cells.slice(0, index + 1);
    p.fragment.querySelectorAll = () => rows;
    p.helper.copyEventListener(p.event);
    assert.equal(p.copied().value, `first ${kind}\nsecond ${kind}\nlast ${kind}`);
    assert.equal(p.helper.selectedColumnIdx, index);
    assert.equal(p.cancelled(), true);
  });
}

for (const target of ['TBODY', 'outside', 'unrelated-cell']) {
  test(`clicking ${target} clears stale diff-copy state`, () => {
    const p = page();
    let element = target === 'TBODY' ? p.tbody : node('DIV');
    if (target === 'unrelated-cell') {
      element = node('TD');
      element.parentElement = { cells: [element], parentElement: {} };
    }
    assert.doesNotThrow(() => p.helper.mousedownEventListener({ button: 0, target: element }));
    p.helper.copyEventListener(p.event);
    assert.equal(p.copied(), undefined);
    assert.equal(p.cancelled(), false);
  });
}

for (const scenario of ['empty', 'collapsed', 'outside-range', 'outside-target', 'prevented', 'no-clipboard', 'denied', 'refused']) {
  test(`${scenario} leaves native copying available`, () => {
    const p = page();
    if (scenario === 'empty') { p.selection.rangeCount = 0; p.selection.getRangeAt = () => assert.fail('no range'); }
    if (scenario === 'collapsed') p.selection.isCollapsed = true;
    if (scenario === 'outside-range') p.range.endContainer = {};
    if (scenario === 'outside-target') p.event.target = node('TD');
    if (scenario === 'prevented') p.event.defaultPrevented = true;
    if (scenario === 'no-clipboard') p.event.clipboardData = null;
    if (scenario === 'denied') p.event.clipboardData.setData = () => { throw Error('denied'); };
    if (scenario === 'refused') p.event.clipboardData.setData = () => false;
    assert.doesNotThrow(() => p.helper.copyEventListener(p.event));
    assert.equal(p.copied(), undefined);
    assert.equal(p.cancelled(), false);
  });
}

test('text node selections copy code when contains() rejects text nodes (IE)', () => {
  const p = page();
  const contains = p.tbody.contains;
  p.tbody.contains = function(child) { return child.nodeType === 1 && contains.call(this, child); };
  p.helper.copyEventListener(p.event);
  assert.deepEqual(p.copied(), { type: 'text', value: 'selected code' });
  assert.equal(p.cancelled(), true);
});

test('legacy clipboard fallback remains supported', () => {
  const p = page();
  p.context.clipboardData = p.event.clipboardData;
  delete p.event.clipboardData;
  p.helper.copyEventListener(p.event);
  assert.equal(p.copied().value, 'selected code');
  assert.equal(p.cancelled(), true);
});

for (const side of ['new', 'old']) {
  test(`multiline copying keeps the ${side} side without duplicate newlines`, () => {
    const p = page('diff_unified');
    const other = side === 'new' ? 'old' : 'new';
    function code(text, kind) { return { ...node('TD', [kind]), textContent: text }; }
    p.fragment.querySelectorAll = () => [
      { cells: [code('first\n', side)] },
      { cells: [node('TD'), node('TD'), node('TD'), code('skip', other)] },
      { cells: [node('TD'), node('TD'), node('TD'), code('shared', '')] },
      { cells: [node('TD'), node('TD'), node('TD'), code('last', side)] }
    ];
    p.helper.copyEventListener(p.event);
    assert.equal(p.copied().value, 'first\nshared\nlast');
  });
}
