import { spawn, spawnSync } from "node:child_process";
import crypto from "node:crypto";
import fs from "node:fs";
import https from "node:https";
import path from "node:path";
import { fileURLToPath } from "node:url";

const projectRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "../..");
const ociRoot = path.join(projectRoot, "test", "oci");
const outputRoot = path.join(projectRoot, "output");
const workRoot = path.join(ociRoot, ".work");
const certificatePath = path.join(workRoot, "registry.crt");
const keyPath = path.join(workRoot, "registry.key");
const opensslConfig = path.join(ociRoot, "registry-openssl.cnf");
const testEntry = path.join(outputRoot, `index.oci-integration-${process.pid}.mjs`);
const manifestMediaType = "application/vnd.oci.image.manifest.v1+json";
const layerMediaType = "application/vnd.oci.image.layer.v1.tar";
const fixtureByReference = new Map();
const layerByDigest = new Map();

function sha256(data) {
  return `sha256:${crypto.createHash("sha256").update(data).digest("hex")}`;
}

function loadFixtures() {
  for (const version of ["v1", "v2"]) {
    const fixtureRoot = path.join(ociRoot, "fixtures", version);
    const manifestBytes = fs.readFileSync(path.join(fixtureRoot, "manifest.json"));
    const manifest = JSON.parse(manifestBytes.toString("utf8"));
    const layerBytes = fs.readFileSync(path.join(fixtureRoot, "snapshot.tar"));
    const manifestDigest = sha256(manifestBytes);
    const layer = manifest.layers?.[0];

    if (layer?.digest !== sha256(layerBytes) || layer.size !== layerBytes.length) {
      throw new Error(`OCI ${version} fixture layer does not match its manifest`);
    }

    fixtureByReference.set(`/v2/team/library/manifests/${version}`, {
      data: manifestBytes,
      digest: manifestDigest,
    });
    layerByDigest.set(layer.digest, layerBytes);
  }
}

function ensureCertificate() {
  fs.mkdirSync(workRoot, { recursive: true });
  if (fs.existsSync(certificatePath) && fs.existsSync(keyPath)) {
    return;
  }

  const result = spawnSync("openssl", [
    "req",
    "-x509",
    "-nodes",
    "-newkey",
    "rsa:2048",
    "-keyout",
    keyPath,
    "-out",
    certificatePath,
    "-days",
    "30",
    "-config",
    opensslConfig,
  ], { cwd: projectRoot, stdio: "inherit" });
  if (result.error) throw result.error;
  if (result.status !== 0) throw new Error(`openssl exited with status ${result.status}`);
}

function send(res, status, headers, body = "") {
  const data = Buffer.isBuffer(body) ? body : Buffer.from(body);
  res.writeHead(status, {
    "Content-Length": data.length,
    "Docker-Distribution-Api-Version": "registry/2.0",
    ...headers,
  });
  res.end(data);
}

function createRegistry() {
  const key = fs.readFileSync(keyPath);
  const cert = fs.readFileSync(certificatePath);

  return https.createServer({ key, cert }, (req, res) => {
    if (req.method !== "GET") {
      send(res, 405, { Allow: "GET" });
      return;
    }

    const requestPath = new URL(req.url ?? "/", "https://127.0.0.1:5443").pathname;
    if (requestPath === "/v2/" || requestPath === "/v2") {
      send(res, 200, {});
      return;
    }

    const manifest = fixtureByReference.get(requestPath);
    if (manifest) {
      send(res, 200, {
        "Content-Type": manifestMediaType,
        "Docker-Content-Digest": manifest.digest,
      }, manifest.data);
      return;
    }

    const blobMatch = requestPath.match(/^\/v2\/team\/library\/blobs\/(sha256:[0-9a-f]{64})$/);
    const layer = blobMatch ? layerByDigest.get(blobMatch[1]) : undefined;
    if (layer) {
      send(res, 200, { "Content-Type": layerMediaType }, layer);
      return;
    }

    send(res, 404, { "Content-Type": "application/json" }, '{"errors":[{"code":"NOT_FOUND"}]}');
  });
}

function createTargetedTestEntry() {
  const sourcePath = path.join(outputRoot, "index.mjs");
  if (!fs.existsSync(sourcePath)) {
    throw new Error("Transpiled output is missing; run npm run build before the OCI integration test");
  }

  const source = fs.readFileSync(sourcePath, "utf8");
  const loop = "  for (const st of getData()) {";
  if (!source.includes(loop)) throw new Error("Could not find the transpiled test runner loop");

  const selectedLoop = `  for (const st of getData().filter((item) => item.objectName === "ZCL_ABAPGIT_OCI_INTEGRATION").map((item) => ({...item, methods: item.methods.filter((method) => method.name === "fetch_local_tags")}))) {`;
  fs.writeFileSync(testEntry, source.replace(loop, selectedLoop));
}

function runTargetedTest() {
  return new Promise((resolve, reject) => {
    const child = spawn(process.execPath, [testEntry, "--only-critical"], {
      cwd: outputRoot,
      env: { ...process.env, NODE_EXTRA_CA_CERTS: certificatePath },
      stdio: "inherit",
    });
    child.once("error", reject);
    child.once("exit", (code, signal) => {
      if (signal) reject(new Error(`OCI integration test stopped by ${signal}`));
      else resolve(code ?? 1);
    });
  });
}

async function main() {
  ensureCertificate();
  loadFixtures();
  createTargetedTestEntry();

  const server = createRegistry();
  try {
    await new Promise((resolve, reject) => {
      server.once("error", reject);
      server.listen(5443, "127.0.0.1", resolve);
    });
    const status = await runTargetedTest();
    if (status !== 0) process.exitCode = status;
  } finally {
    await new Promise((resolve) => {
      if (!server.listening) return resolve();
      server.close(() => resolve());
    });
    fs.rmSync(testEntry, { force: true });
  }
}

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
