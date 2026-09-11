const assert = require("node:assert/strict");
const test = require("node:test");
const loadUi = require("./load-ui.cjs");

function cell(className, text) {
  return {
    className, innerText: text, innerHTML: text, children: [],
    classList: { add() {}, remove() {} },
    // These fixtures contain plain cells, with no links or link hints.
    getElementsByTagName() { return []; },
    cloneNode() { return { textContent: text, querySelectorAll() { return []; } }; }
  };
}

function page() {
  const rows = [
    { className: "local", name: "zcl_keep.clas.abap" },
    { className: "remote", name: "zcl_keep_old.clas.abap" },
    { className: "local", name: "zcl_hidden.clas.abap" }
  ].map(({ className, name }) => ({
    className, style: { display: "" },
    cells: [cell("name", name), cell("user", "CHRIS"), cell("transport", "DEVK900001"), cell("status", "?"), cell("cmd", "")]
  }));
  const nodes = {
    table: { tBodies: [{ rows }], style: { display: "" } },
    commit: { innerHTML: "Add All and Commit (3)" },
    patch: { innerHTML: "Patch All (3)" },
    search: { value: "" }
  };
  const context = loadUi({
    document: {
      addEventListener() {},
      getElementById(id) { return nodes[id]; },
      querySelector() { return null; }
    },
    scrollTo() {},
    alert(message) { assert.fail(message); }
  });
  const submissions = [];
  // Observe the staging-to-form boundary; forms.test.cjs exercises transport.
  context.submitSapeventForm = (params, action) => submissions.push({ params: { ...params }, action });
  const helper = new context.StageHelper({
    seed: "stage", formAction: "stage_commit", patchAction: "stage_patch", stageAllAction: "stage_all",
    ids: { stageTab: "table", commitBtn: "commit", patchBtn: "patch", objectSearch: "search" }
  });
  return { helper, rows, nodes, submissions };
}

test("commit without selection or filter uses the stage-all action", () => {
  const { nodes, submissions } = page();
  nodes.commit.onclick();
  assert.deepEqual(submissions, [{ params: {}, action: "stage_all" }]);
});

test("filtering then committing adds matching local files and removes matching remote files", () => {
  const { helper, rows, nodes, submissions } = page();
  helper.applyFilterValue("KEEP");
  assert.deepEqual(rows.map(row => row.style.display), ["", "", "none"]);
  assert.equal(nodes.commit.innerHTML, "Add <b>Filtered</b> and Commit (2)");
  nodes.commit.onclick();
  assert.deepEqual(submissions, [{ action: "stage_commit", params: {
    "zcl_keep.clas.abap": "A", "zcl_keep_old.clas.abap": "R", "zcl_hidden.clas.abap": "?"
  } }]);
  assert.equal(helper.selectedCount, 2);
});

for (const button of ["commit", "patch"]) {
  test(`explicit selection takes precedence over the filter for ${button}`, () => {
    const { helper, rows, nodes, submissions } = page();
    helper.updateRow(rows[2], helper.STATUS.add);
    helper.applyFilterValue("keep");
    assert.equal(nodes[button].innerHTML, (button === "commit" ? "Commit" : "Patch") + " <b>Selected</b> (1)");
    nodes[button].onclick();
    assert.deepEqual(submissions, [{ action: button === "commit" ? "stage_commit" : "stage_patch", params: {
      "zcl_keep.clas.abap": "?", "zcl_keep_old.clas.abap": "?", "zcl_hidden.clas.abap": "A"
    } }]);
  });
}

test("filtered patch submission includes only matching files", () => {
  const { helper, nodes, submissions } = page();
  helper.applyFilterValue("keep");
  assert.equal(nodes.patch.innerHTML, "Patch <b>Filtered</b> (2)");
  nodes.patch.onclick();
  assert.deepEqual(submissions, [{ action: "stage_patch", params: {
    "zcl_keep.clas.abap": "A", "zcl_keep_old.clas.abap": "R", "zcl_hidden.clas.abap": "?"
  } }]);
});

test("clearing a filter restores all rows and the default commit action", () => {
  const { helper, rows, nodes, submissions } = page();
  helper.applyFilterValue("keep");
  helper.applyFilterValue("");
  assert.deepEqual(rows.map(row => row.style.display), ["", "", ""]);
  assert.equal(nodes.commit.innerHTML, "Add All and Commit (3)");
  assert.equal(nodes.patch.innerHTML, "Patch All (3)");
  nodes.commit.onclick();
  assert.deepEqual(submissions, [{ action: "stage_all", params: {} }]);
});

test("a filter with no matches does not fall back to committing everything", () => {
  const { helper, nodes, submissions } = page();
  helper.applyFilterValue("no_such_file");
  assert.equal(nodes.commit.innerHTML, "Add <b>Filtered</b> and Commit (0)");
  nodes.commit.onclick();
  assert.deepEqual(submissions, [{ action: "stage_commit", params: {
    "zcl_keep.clas.abap": "?", "zcl_keep_old.clas.abap": "?", "zcl_hidden.clas.abap": "?"
  } }]);
});

test("changing a selected status does not double-count and resetting restores filtered mode", () => {
  const { helper, rows, nodes } = page();
  helper.applyFilterValue("keep");
  helper.updateRow(rows[1], helper.STATUS.ignore);
  helper.updateRow(rows[1], helper.STATUS.remove);
  helper.updateRow(rows[1], helper.STATUS.remove);
  helper.updateMenu();
  assert.equal(helper.selectedCount, 1);
  assert.equal(nodes.commit.innerHTML, "Commit <b>Selected</b> (1)");
  helper.updateRow(rows[1], helper.STATUS.reset);
  helper.updateMenu();
  assert.equal(helper.selectedCount, 0);
  assert.equal(nodes.commit.innerHTML, "Add <b>Filtered</b> and Commit (2)");
});
