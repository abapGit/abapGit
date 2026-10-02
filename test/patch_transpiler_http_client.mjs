import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const outputFile = path.join(root, "output", "cl_http_client.clas.mjs");
const source = fs.readFileSync(outputFile, "utf8");

// open-abap-core's generated HTTP client misparses URL hosts with explicit ports.
// Correct the transpiled test shim so HTTPS registry integration works like SAP's client.
if (!source.includes("const parsedUrl = new URL(url.get());")) {
  const urlParser = /    abap\.statements\.split\(\{source: url,[\s\S]*?(?=    this\.if_http_client\$request\.set)/;
  if (!urlParser.test(source)) {
    throw new Error("Could not find the transpiled CL_HTTP_CLIENT URL parser");
  }

  const fixedParser = [
    "    const parsedUrl = new URL(url.get());",
    "    this.#mv_host.set(parsedUrl.origin);",
    "    lv_uri.set(parsedUrl.pathname);",
    "    lv_query.set(parsedUrl.search.startsWith(\"?\") ? parsedUrl.search.slice(1) : \"\");",
    "",
  ].join("\n");
  fs.writeFileSync(outputFile, source.replace(urlParser, fixedParser));
}
