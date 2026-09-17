const assert = require("node:assert/strict");
const test = require("node:test");
const loadUi = require("./load-ui.cjs");

function page() {
  const files = [
    { file: "/src/a.abap", extension: "abap", "object-type": "CLAS", "changed-by": "ME" },
    { file: "/src/b.abap", extension: "abap", "object-type": "PROG", "changed-by": "OTHER" },
    { file: "/src/a.abap.xml", extension: "xml", "object-type": "CLAS", "changed-by": "ME" }
  ];
  const rows = files.map(attrs => ({ className: "diff", style: { display: "" },
    getAttribute(name) { return attrs[name.slice(5)]; } }));
  const jumps = files.map(file => ({ text: file.file, style: { display: "" } }));
  const classes = new Set();
  const timers = [];
  const context = loadUi({
    document: { addEventListener() {}, querySelectorAll() { return jumps; } },
    setTimeout(fn) { timers.push(fn); }
  });
  context.runTimers = () => { timers.splice(0).forEach(fn => fn()); };
  const helper = Object.create(context.DiffHelper.prototype);
  helper.dom = { diffList: { children: rows }, filterButton: { classList: {
    add(name) { classes.add(name); }, remove(name) { classes.delete(name); }
  } } };
  return { context, helper, rows, jumps, classes };
}

test("diff filters intersect and staging/jump links follow the visible files", () => {
  const { helper, rows, jumps, classes } = page();
  helper.applyFilter("extension", "abap", false);
  helper.applyFilter("changed-by", "ME", false);
  helper.applyFilter("changed-by", "ME", true);
  assert.deepEqual(rows.map(row => row.style.display), ["none", "none", ""]);
  assert.deepEqual(jumps.map(row => row.style.display), ["none", "none", ""]);
  assert.deepEqual(Object.keys(helper.buildStageCache()), ["/src/a.abap.xml"]);
  helper.applyFilter("object-type", "CLAS", false);
  helper.applyFilter("extension", "abap", true);
  assert.deepEqual(Object.keys(helper.buildStageCache()), ["/src/b.abap"]);
  helper.applyFilter("object-type", "CLAS", true);
  helper.applyFilter("object-type", "CLAS", true);
  assert.equal(classes.has("bgorange"), false);
});

test("Only my changes preserves other filter categories when enabled and disabled", () => {
  const { helper, rows, classes } = page();
  helper.applyFilter("extension", "xml", false);
  helper.applyOnlyMyChangesFilter("ME", true);
  assert.deepEqual(rows.map(row => row.style.display), ["", "none", "none"]);
  helper.applyOnlyMyChangesFilter("ME", false);
  assert.deepEqual(rows.map(row => row.style.display), ["", "", "none"]);
  assert.equal(classes.has("bgorange"), true);
  helper.applyFilter("extension", "xml", true);
  assert.equal(classes.has("bgorange"), false);
});

test("staging still navigates if session storage is full", () => {
  const { context, helper } = page();
  context.sessionStorage = { setItem() { throw Error("full"); } };
  helper.repoKey = "1";
  helper.pageSeed = "diff";
  helper.stageAction = "stage";
  let submitted;
  context.submitSapeventForm = (params, action, method) => { submitted = { ...params, action, method }; };
  helper.onStage();
  assert.deepEqual(submitted, { key: "1", seed: "diff", action: "stage", method: "get" });
});

test("jump scrolls to the exact path, not to a path that merely contains it", () => {
  const { context, helper, rows } = page();
  const scrolled = [];
  rows.forEach((row, i) => { row.scrollIntoView = () => scrolled.push(i); });

  helper.onJump({ target: { text: "/src/a.abap" } });
  context.runTimers();
  assert.deepEqual(scrolled, [0]);

  // A palette entry passes the title through as a plain string.
  helper.onJump("/src/a.abap.xml");
  context.runTimers();
  assert.deepEqual(scrolled, [0, 2]);
});

test("jump ignores an unknown path and tolerates quotes in it", () => {
  const { context, helper, rows } = page();
  const scrolled = [];
  rows.forEach((row, i) => { row.scrollIntoView = () => scrolled.push(i); });

  helper.onJump("/src/a.ab");       // prefix of a real path
  helper.onJump("/src/o'brien.abap");
  context.runTimers();
  assert.deepEqual(scrolled, []);
});
