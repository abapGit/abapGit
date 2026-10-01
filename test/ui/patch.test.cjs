const assert = require("node:assert/strict");
const test = require("node:test");
const loadUi = require("./load-ui.cjs");

function page() {
  const lines = ["patch_line_file.abap_1_1", "patch_line_file.abap_1_2", "patch_line_file.abap_10_1",
    "patch_line_file.abap_11_1", "patch_line_file.abapx_1_1"].map(id => ({ id, checked: false }));
  const sections = ["patch_section_file.abap_1", "patch_section_file.abap_10", "patch_section_file.abap_11",
    "patch_section_file.abapx_1"].map(id => ({ id, checked: false }));
  const context = loadUi({ document: {
    addEventListener() {},
    querySelectorAll(selector) {
      const match = selector.match(/^input\[id\^='([^']+)'\]$/);
      assert.ok(match, selector);
      return [...lines, ...sections].filter(input => input.id.startsWith(match[1].replace(/\\([.#])/g, "$1")));
    }
  } });
  const patch = new context.Patch();
  const submissions = [];
  context.submitSapeventForm = (params, action) => submissions.push({
    action, add: Array.from(params.add), remove: Array.from(params.remove)
  });
  return { patch, lines, sections, submissions };
}

test("section 1 changes only its own lines, including the submitted selection", () => {
  const { patch, lines, sections, submissions } = page();
  sections[0].checked = true;
  patch.onClickSectionCheckbox({ srcElement: { ...sections[0], nodeName: "INPUT" } });
  assert.deepEqual(lines.map(line => line.checked), [true, true, false, false, false]);
  patch.submitPatch("patch_stage");
  assert.deepEqual(submissions[0], { action: "patch_stage",
    add: lines.slice(0, 2).map(line => line.id), remove: lines.slice(2).map(line => line.id) });
  patch.onClickSectionCheckbox({ srcElement: { ...sections[0], checked: false, nodeName: "INPUT" } });
  assert.ok(lines.every(line => !line.checked));
});

test("file toggle updates every section and line in that file", () => {
  const { patch, lines, sections } = page();
  patch.onClickFileCheckbox({ srcElement: { id: "patch_file_file.abap", checked: true, nodeName: "INPUT" } });
  assert.ok(lines.slice(0, 4).every(line => line.checked));
  assert.ok(sections.slice(0, 3).every(section => section.checked));
  assert.equal(lines[4].checked, false);
  assert.equal(sections[3].checked, false);
});
