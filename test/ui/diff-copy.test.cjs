const assert = require('node:assert/strict');
const test = require('node:test');
const loadUi = require('./load-ui.cjs');

function node(tagName, classes = []) {
  const set = new Set(classes);
  return { tagName, classList: { contains: name => set.has(name), add: name => set.add(name), remove: name => set.delete(name) },
    contains(child) { for (; child; child = child.parentElement) if (child === this) return true; return false; } };
}
function page(side = 'diff_left', patch = false) {
  const table = node('TABLE'), tbody = node('TBODY'), row = node('TR');
  const cell = node('TD', [side]), text = { parentElement: cell };
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
