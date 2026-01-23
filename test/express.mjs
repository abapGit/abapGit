import express from 'express';
import {initializeABAP} from "../output/init.mjs";
import {cl_express_icf_shim} from "../output/cl_express_icf_shim.clas.mjs";
import {zcl_abapgit_web_setup} from "../output/zcl_abapgit_web_setup.clas.mjs";
await initializeABAP();

const PORT = 3000;

const app = express();
app.disable('x-powered-by');
app.set('etag', false);
app.use(express.raw({type: "*/*"}));

zcl_abapgit_web_setup.setup();

// ------------------

app.get('/', function (req, res) {
  res.send('path: /');
});

// ------------------

app.all(["/sap", "/sap*"], async function (req, res) {
  await cl_express_icf_shim.run({req, res, class: "ZCL_ABAPGIT_WEB_SICF"});
});

app.listen(PORT);
console.log("Listening on port http://localhost:" + PORT + "/sap/zabapgit/");