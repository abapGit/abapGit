const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");

const source = fs.readFileSync(path.join(__dirname, "../../src/ui/zabapgit_js_common.w3mi.data.js"), "utf8");

// Evaluate the shipped script in a fresh browser global for every test.
module.exports = function loadUi(overrides = {}) {
  const context = {
    document: { addEventListener() {} },
    addEventListener() {},
    ...overrides
  };
  context.window = context;
  vm.createContext(context);
  vm.runInContext(source, context);
  return context;
};
