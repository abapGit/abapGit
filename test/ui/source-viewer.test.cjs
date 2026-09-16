const assert = require('node:assert/strict');
const test = require('node:test');
const loadUi = require('./load-ui.cjs');

function page() {
  const requests = [], errors = [];
  function XMLHttpRequest() { requests.push(this); }
  XMLHttpRequest.prototype.open = function(method, url, async) { this.request = { method, url, async }; };
  XMLHttpRequest.prototype.send = function() {};
  const context = loadUi({ XMLHttpRequest, alert(message) { errors.push(message); } });
  const viewer = new context.SourceViewer();
  const sources = [{ url: 'a.js', tab: {} }, { url: 'b.js', tab: {} }];
  viewer.sources = sources;
  viewer.source = { value: '', focus() {}, setSelectionRange() {} };
  viewer.lineNumbers = {};
  function respond(index, status, content) {
    Object.assign(requests[index], { readyState: 4, status, responseText: content });
    requests[index].onreadystatechange();
  }
  return { viewer, sources, requests, errors, respond };
}

for (const status of [200, 0]) {
  test(`asset load accepts status ${status} with content and builds line numbers`, () => {
    const p = page();
    p.viewer.selectSource(p.sources[0]);
    assert.deepEqual(p.requests[0].request, { method: 'GET', url: 'a.js', async: true });
    p.respond(0, status, 'one\r\ntwo\nthree\rfour');
    assert.equal(p.viewer.source.value, 'one\r\ntwo\nthree\rfour');
    assert.equal(p.viewer.lineNumbers.textContent, '1\n2\n3\n4');
    assert.equal(p.errors.length, 0);
  });
}

test('late response cannot overwrite the newly selected source and is cached', () => {
  const p = page();
  p.viewer.selectSource(p.sources[0]);
  p.viewer.selectSource(p.sources[1]);
  p.respond(1, 200, 'second');
  p.respond(0, 200, 'first');
  assert.equal(p.viewer.source.value, 'second');
  p.viewer.selectSource(p.sources[0]);
  assert.equal(p.viewer.source.value, 'first');
  assert.equal(p.requests.length, 2);
});

test('response arriving after closing the viewer does not access removed elements', () => {
  const p = page();
  p.viewer.selectSource(p.sources[0]);
  p.viewer.activeSource = p.viewer.source = p.viewer.lineNumbers = null;
  assert.doesNotThrow(() => p.respond(0, 200, 'late'));
});

for (const status of [404, 500, 0]) {
  test(`failed asset response (${status}) reports once and permits a retry`, () => {
    const p = page();
    p.viewer.selectSource(p.sources[0]);
    p.respond(0, status, '');
    p.requests[0].onerror();
    assert.equal(p.errors.length, 1);
    assert.equal(p.sources[0].content, undefined);
    p.viewer.selectSource(p.sources[0]);
    p.respond(1, 200, 'recovered');
    assert.equal(p.viewer.source.value, 'recovered');
  });
}

test('IE uses stylesheet rules and avoids requesting cached JavaScript', () => {
  const p = page();
  p.viewer.isInternetExplorer = () => true;
  p.viewer.getStylesheetSource = () => 'body {}';
  p.sources[0].url = 'css/common.css';
  p.viewer.selectSource(p.sources[0]);
  assert.equal(p.viewer.source.value, 'body {}');
  p.viewer.selectSource(p.sources[1]);
  assert.match(p.viewer.source.value, /Internet Explorer cannot display/);
  assert.equal(p.requests.length, 0);
});
