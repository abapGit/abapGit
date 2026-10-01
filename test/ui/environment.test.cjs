const assert = require("node:assert/strict");
const test = require("node:test");
const loadUi = require("./load-ui.cjs");

const EDGE = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36 Edg/120.0.0.0";
const IE = "Mozilla/5.0 (Windows NT 10.0; WOW64; Trident/7.0; rv:11.0) like Gecko";
const CHROME = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36";

// The warning and the footer of a page, set up as zcl_abapgit_gui_page renders it
function page(userAgent, env, document = {}) {
  const warning = { style: {} };
  const footer = { innerHTML: "" };
  const context = loadUi({
    navigator: { userAgent },
    document: {
      addEventListener() {},
      getElementById(id) { return { "browser-control-warning": warning, "browser-control-footer": footer }[id] || null; },
      ...document
    }
  });
  context.setEnvironment(env);
  context.toggleBrowserControlWarning();
  context.displayBrowserControlFooter();
  return { context, warning, footer };
}

for (const [name, userAgent, env, control] of [
  ["the Edge control", EDGE, { isSapGuiForWindows: true }, "Edge"],
  ["the IE control", IE, { isSapGuiForWindows: true }, "IE"],
  ["the HTML GUI in Edge", EDGE, { isWebGui: true }, ""],
  ["the HTML GUI in Chrome", CHROME, { isWebGui: true }, ""],
  ["SAP GUI for Java", CHROME, {}, ""]
]) {
  test(`on ${name} the browser control is ${JSON.stringify(control)}`, () => {
    const { context, warning, footer } = page(userAgent, env);
    assert.equal(context.gEnv.browserControl, control);
    assert.equal(warning.style.display, control === "Edge" ? undefined : "none");
    assert.equal(footer.innerHTML, control ? " - " + control : "");
  });
}

test("the IE engine is recognized by document.documentMode, on any GUI", () => {
  assert.equal(page(IE, { isWebGui: true }, { documentMode: 11 }).context.gEnv.isInternetExplorer, true);
  assert.equal(page(EDGE, { isSapGuiForWindows: true }).context.gEnv.isInternetExplorer, false);
});

for (const [link, prefix] of [
  ['a[href*="file:///SAPEVENT:"]', "file:///"],
  ['a[href^="sap-cust"]', "sap-cust://sap-place-holder/"],
  [null, ""]
]) {
  test(`the sapevent prefix is ${JSON.stringify(prefix)} and probed only once`, () => {
    let probes = 0;
    const { context } = page(EDGE, { isSapGuiForWindows: true }, {
      querySelector(selector) { probes++; return selector === link ? {} : null; }
    });
    assert.equal(context.getSapeventPrefix(), prefix);
    const probed = probes;
    assert.equal(context.getSapeventPrefix(), prefix);
    assert.equal(probes, probed);
    assert.equal(context.gEnv.sapeventPrefix, prefix);
  });
}

test("an unknown environment key is reported, not added", () => {
  const logged = [];
  const context = loadUi({ console: { log(message) { logged.push(message); } } });
  context.setEnvironment({ isWebGUI: true });
  assert.equal(context.gEnv.isWebGui, false);
  assert.equal("isWebGUI" in context.gEnv, false);
  assert.match(logged[0], /unknown environment key 'isWebGUI'/);
});
