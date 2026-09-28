const assert = require("node:assert/strict");
const test = require("node:test");
const loadUi = require("./load-ui.cjs");

const PAGE_URL = "file:///C:/Users/me/AppData/Local/SAP/abapGit/page.html";

// The banner as render_js_error_banner renders it: an icon, then the text
function page() {
  const icon = { nodeName: "I" };
  const banner = {
    style: {}, childNodes: [icon, { nodeName: "#text", textContent: " If this does not disappear soon, ..." }],
    get firstChild() { return this.childNodes[0]; },
    get textContent() { return this.childNodes.map(child => child.textContent || "").join(""); },
    querySelector(selector) { return selector === "i" ? icon : null; },
    appendChild(child) { this.childNodes.push(child); },
    removeChild(child) { this.childNodes.splice(this.childNodes.indexOf(child), 1); }
  };
  const listeners = {};
  const context = loadUi({
    location: { href: PAGE_URL + "#" },
    addEventListener(name, fn) { (listeners[name] = listeners[name] || []).push(fn); },
    document: {
      addEventListener() {},
      getElementById(id) { return id === "js-error-banner" ? banner : { appendChild() {} }; },
      createElement() { return {}; },
      createTextNode(text) { return { nodeName: "#text", textContent: text }; }
    }
  });
  function error(message, filename, lineno) {
    listeners.error.forEach(fn => fn({ message, filename, lineno }));
  }
  return { context, banner, icon, error };
}

test("an error after initialization shows the banner again, with message and line", () => {
  const { context, banner, icon, error } = page();
  context.confirmInitialized();
  assert.equal(banner.style.display, "none");
  error("TypeError: x is undefined", "file:///C:/Users/me/AppData/Local/SAP/abapGit/js/common.js", 1234);
  assert.equal(banner.style.display, "");
  assert.equal(banner.firstChild, icon);
  assert.equal(banner.textContent, " JavaScript error: TypeError: x is undefined (common.js:1234), please log an issue");
});

test("errors of the inline page scripts are reported too", () => {
  const { banner, error } = page();
  error("ReferenceError: gHelper is not defined", PAGE_URL, 12);
  assert.match(banner.textContent, /gHelper is not defined \(page script:12\)/);
});

test("only the first error is reported, later ones are mostly its consequences", () => {
  const { banner, error } = page();
  error("first", "sap-cust://sap-place-holder/js/common.js", 1);
  error("second", "sap-cust://sap-place-holder/js/common.js", 2);
  assert.match(banner.textContent, /first/);
  assert.doesNotMatch(banner.textContent, /second/);
});

for (const [name, filename] of [
  ["a script of ITS", "https://host/sap/public/bc/its/lsgui/js/htmlviewer.js"],
  ["a script of another origin, which only reports \"Script error.\"", ""],
  ["a file that merely ends like ours", "https://host/xjs/common.js"]
]) {
  test(`errors of ${name} leave the banner alone`, () => {
    const { context, banner, error } = page();
    context.confirmInitialized();
    error("Script error.", filename, 0);
    assert.equal(banner.style.display, "none");
  });
}
