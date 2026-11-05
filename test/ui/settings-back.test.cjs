const assert = require('node:assert/strict');
const test = require('node:test');
const loadUi = require('./load-ui.cjs');

// Match the toolbar onclick and the existing form command rendered by ABAP.
// The ABAP menu tests verify that the toolbar produces this onclick, and the
// form-utils tests cover the server's clean/dirty cancel/confirm decisions.
function settingsPage(webgui, routingInFields, value) {
  const fields = [{ name: 'setting', value }, { name: 'PARAMS', value: 'save' }];
  if (!webgui || !routingInFields) fields.pop();
  const attrs = { action: webgui
    ? 'https://host/webgui?~control=116&~event=OnSAPEvent&PARAMS=save'
    : 'sapevent:save' };
  const submissions = [];
  const form = {
    id: 'settings-form', elements: fields,
    getAttribute(name) { return attrs[name] ?? null; },
    setAttribute(name, value) { attrs[name] = value; },
    querySelectorAll() { return fields.filter(f => f.name === 'PARAMS'); },
    submit(action = attrs.action) {
      assert.equal(context.gSapeventNavPending, true);
      submissions.push({ action, method: attrs.method || 'post', value: fields[0].value,
        params: fields.find(f => f.name === 'PARAMS')?.value });
    }
  };
  function element(nodeName, attributes, click) {
    return { nodeName, type: attributes.type, href: attributes.href, title: '', click,
      getAttribute(name) { return attributes[name] ?? null; } };
  }
  const toolbar = element('A', { href: '#', onclick: true }, () =>
    context.submitSapeventForm({}, 'go_back', 'post', form));
  const bottom = webgui
    ? element('A', { href: '#', 'data-sapevent': 'go_back' }, () =>
      context.submitSapeventForm({}, 'go_back', 'post', form))
    : element('INPUT', { type: 'submit', formaction: 'sapevent:go_back' }, () =>
      form.submit('sapevent:go_back'));
  bottom.form = form;
  const context = loadUi({ document: {
    addEventListener() {},
    querySelectorAll() { return [toolbar, bottom]; },
    querySelector() { return null; },
    getElementById(id) { return id === form.id ? form : null; }
  } });
  context.setEnvironment({ isWebGui: webgui });
  return { context, toolbar, bottom, submissions, form };
}

for (const [environment, webgui, routingInFields] of [
  ['desktop', false, false], ['WebGUI action routing', true, false], ['WebGUI hidden routing', true, true]
]) {
  for (const navigation of ['toolbar', 'bottom', 'browser', 'F3']) {
    test(`${environment}: cancelling ${navigation} Back preserves Save`, () => {
      const { context, toolbar, bottom, submissions, form } = settingsPage(webgui, routingInFields, 'unsaved edit');
      const originalAction = form.getAttribute('action');
      for (let attempt = 0; attempt < 2; attempt++) {
        if (navigation === 'toolbar') toolbar.click();
        else if (navigation === 'bottom') context.clickSapEvent(bottom);
        else if (navigation === 'browser') context.triggerSapEventBack('go_back');
        else new context.Hotkeys({ F3: 'go_back' }).onkeydown({ key: 'F3', preventDefault() {} });
        // The server's No response leaves this DOM in place.
        assert.equal(form.getAttribute('action'), originalAction);
        if (routingInFields) assert.equal(form.elements[1].value, 'save');
      }
      // Desktop's main Save button uses the form action without an override.
      if (webgui) context.submitSapeventForm({}, 'save', 'post', form);
      else form.submit();
      assert.equal(submissions.length, 3);
      assert.equal(submissions[2].value, 'unsaved edit');
      if (routingInFields) assert.equal(submissions[2].params, 'save');
      else assert.match(submissions[2].action, /(?:sapevent:|PARAMS=)save$/i);
    });
  }
}

test('failed toolbar submission restores the form action', () => {
  const { toolbar, form } = settingsPage(false, false, 'unsaved edit');
  const originalAction = form.getAttribute('action');
  form.submit = () => { throw new Error('submission failed'); };
  assert.throws(() => toolbar.click(), /submission failed/);
  assert.equal(form.getAttribute('action'), originalAction);
});

for (const [environment, webgui, routingInFields] of [
  ['desktop', false, false], ['WebGUI action routing', true, false], ['WebGUI hidden routing', true, true]
]) {
  for (const navigation of ['toolbar', 'bottom', 'browser', 'F3']) {
    for (const value of ['saved', 'unsaved edit']) {
      test(`${environment}: ${navigation} Back submits current ${value} fields`, () => {
        const { context, toolbar, bottom, submissions } = settingsPage(webgui, routingInFields, value);
        assert.equal(context.findSapEventElement('go_back'), bottom);
        if (navigation === 'toolbar') toolbar.click();
        else if (navigation === 'bottom') context.clickSapEvent(bottom);
        else if (navigation === 'browser') context.triggerSapEventBack('go_back');
        else new context.Hotkeys({ F3: 'go_back' }).onkeydown({ key: 'F3', preventDefault() {} });
        assert.equal(submissions.length, 1);
        assert.equal(submissions[0].value, value);
        assert.equal(submissions[0].method, 'post');
        if (routingInFields) assert.equal(submissions[0].params, 'go_back');
        else assert.match(submissions[0].action, /(?:sapevent:|PARAMS=)go_back$/i);
        if (webgui) assert.match(submissions[0].action, /~control=116&~event=OnSAPEvent&/);
      });
    }
  }
}
