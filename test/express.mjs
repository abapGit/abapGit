import express from 'express';
import {initializeABAP} from "../output/init.mjs";
import {cl_express_icf_shim} from "../output/cl_express_icf_shim.clas.mjs";
import {zcl_abapgit_inject_setup} from "../output/zcl_abapgit_inject_setup.clas.mjs";
await initializeABAP();

const PORT = 3000;

const app = express();
app.disable('x-powered-by');
app.set('etag', false);
app.use(express.raw({type: "*/*"}));

zcl_abapgit_inject_setup.setup();

// ------------------

app.get('/', function (req, res) {
  res.send('path: /');
});

// ------------------

app.all(["/sap", "/sap*"], async function (req, res) {
  await cl_express_icf_shim.run({req, res, class: "ZCL_ABAPGIT_WEB_SICF"});
});

const server = app.listen(PORT);
console.log("Listening on port http://localhost:" + PORT + "/sap/zabapgit/");

let shuttingDown = false;
function shutdown(signal) {
  if (shuttingDown) {
    console.log("Forcing exit");
    process.exit(1);
  }
  shuttingDown = true;
  console.log("Received " + signal + ", shutting down, press again to force exit");
  server.close(function (err) {
    if (err) {
      console.error(err.message);
      process.exit(1);
    }
    process.exit(0);
  });
  // force exit if connections do not drain in time
  setTimeout(function () {
    console.log("Shutdown timed out, forcing exit");
    process.exit(1);
  }, 5000).unref();
}

process.on("SIGINT", () => shutdown("SIGINT"));
process.on("SIGTERM", () => shutdown("SIGTERM"));