const assert = require("node:assert/strict");
const crypto = require("node:crypto");
const fs = require("node:fs");
const path = require("node:path");
const test = require("node:test");

const root = __dirname;
const expected = JSON.parse(fs.readFileSync(path.join(root, "expected-digests.json"), "utf8"));

function digest(data) {
  return "sha256:" + crypto.createHash("sha256").update(data).digest("hex");
}

function readTar(file) {
  const archive = fs.readFileSync(file);
  const files = new Map();
  let offset = 0;

  while (offset + 512 <= archive.length) {
    const header = archive.subarray(offset, offset + 512);
    if (header.every(byte => byte === 0)) break;

    assert.equal(header.toString("ascii", 257, 262), "ustar");
    const trimField = (start, end) => header.toString("utf8", start, end).replace(/\0.*$/s, "");
    const name = trimField(0, 100);
    const prefix = trimField(345, 500);
    const fullName = (prefix ? prefix + "/" : "") + name;
    const size = parseInt(trimField(124, 136).trim() || "0", 8);
    const type = String.fromCharCode(header[156]);
    const contentStart = offset + 512;

    if (type === "\0" || type === "0") {
      files.set(fullName.replace(/^\.\//, ""), archive.subarray(contentStart, contentStart + size));
    }

    offset = contentStart + Math.ceil(size / 512) * 512;
  }

  return files;
}

function readFixture(version) {
  const directory = path.join(root, "fixtures", version);
  const archive = fs.readFileSync(path.join(directory, "snapshot.tar"));
  const manifestBytes = fs.readFileSync(path.join(directory, "manifest.json"));
  const manifest = JSON.parse(manifestBytes.toString("utf8"));
  const expectedVersion = expected[version];

  assert.equal(digest(manifestBytes), expectedVersion.manifestDigest);
  assert.equal(manifest.schemaVersion, 2);
  assert.equal(manifest.mediaType, "application/vnd.oci.image.manifest.v1+json");
  assert.equal(manifest.artifactType, "application/vnd.abapgit.repository.v1");
  assert.equal(manifest.layers.length, 1);
  assert.equal(manifest.layers[0].mediaType, "application/vnd.oci.image.layer.v1.tar");
  assert.equal(manifest.layers[0].digest, digest(archive));
  assert.equal(manifest.layers[0].digest, expectedVersion.layerDigest);
  assert.equal(manifest.layers[0].size, archive.length);
  assert.equal(manifest.layers[0].size, expectedVersion.layerSize);

  const files = readTar(path.join(directory, "snapshot.tar"));
  assert.ok(files.has(".abapgit.xml"));
  assert.ok(files.has("src/package.devc.xml"));
  return files;
}

test("ORAS fixtures contain the documented one-layer USTAR profile", () => {
  assert.equal(expected.orasVersion, "1.2.3");
  const first = readFixture("v1");
  const second = readFixture("v2");

  assert.match(first.get("src/zcl_oci_fixture.clas.abap").toString("utf8"), /'v1'/);
  assert.match(second.get("src/zcl_oci_fixture.clas.abap").toString("utf8"), /'v2'/);
  assert.ok(first.has("src/zcl_oci_removed.clas.abap"));
  assert.ok(first.has("src/zcl_oci_removed.clas.xml"));
  assert.equal(second.has("src/zcl_oci_removed.clas.abap"), false);
  assert.equal(second.has("src/zcl_oci_removed.clas.xml"), false);
});
