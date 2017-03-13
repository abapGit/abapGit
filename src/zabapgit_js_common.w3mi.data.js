/**********************************************************
 * ABAPGIT JS function library
 **********************************************************/

/**********************************************************
 * Polyfills
 **********************************************************/

// Bind polyfill (for IE7), taken from https://developer.mozilla.org/
if (!Function.prototype.bind) {
  Function.prototype.bind = function(oThis) {
    if (typeof this !== "function") {
      throw new TypeError("Function.prototype.bind - subject is not callable");
    }

    var aArgs   = Array.prototype.slice.call(arguments, 1),
        fToBind = this,
        fNOP    = function() {},
        fBound  = function() {
          return fToBind.apply(this instanceof fNOP
                 ? this
                 : oThis,
                 aArgs.concat(Array.prototype.slice.call(arguments)));
        };

    if (this.prototype) {
      fNOP.prototype = this.prototype; 
    }
    fBound.prototype = new fNOP();

    return fBound;
  };
}

/**********************************************************
 * Common functions
 **********************************************************/

// Output text to the debug div
function debugOutput(text, dstID) {
  var stdout       = document.getElementById(dstID || "debug-output");
  var wrapped      = "<p>" + text + "</p>";
  stdout.innerHTML = stdout.innerHTML + wrapped;
}

// Create hidden form and submit with sapevent
function submitSapeventForm(params, action) {
  var form = document.createElement("form");
  form.setAttribute("method", "post");
  form.setAttribute("action", "sapevent:" + action);
  
  for(var key in params) {
    var hiddenField = document.createElement("input");
    hiddenField.setAttribute("type", "hidden");
    hiddenField.setAttribute("name", key);
    hiddenField.setAttribute("value", params[key]);
    form.appendChild(hiddenField);
  }

  document.body.appendChild(form);
  form.submit();
}

// Set focus to a control
function setInitialFocus(id) {
  document.getElementById(id).focus();
}

// Submit an existing form
function submitFormById(id) {
  document.getElementById(id).submit();
}

// JS error stub
function errorStub(event) {
  var element    = event.target || event.srcElement;
  var targetName = element.id || element.name || "???";
  alert("JS Error, please log an issue (@" + targetName + ")");
}

// confirm JS initilization
function confirmInitialized() {
  var errorBanner = document.getElementById("js-error-banner");
  if (errorBanner) {
    errorBanner.style.display = "none";
  }
  debugOutput("js: OK"); // Final final confirmation :)
}

/**********************************************************
 * Performance utils (for debugging)
 **********************************************************/

var gPerf = [];

function perfOut(prefix) {
  var totals = {};
  for (var i = gPerf.length - 1; i >= 0; i--) {
    if (!totals[gPerf[i].name]) totals[gPerf[i].name] = {count: 0, time: 0};
    totals[gPerf[i].name].time  += gPerf[i].time;
    totals[gPerf[i].name].count += 1;
  }

  var keys = Object.keys(totals);
  for (var i = keys.length - 1; i >= 0; i--) {
    console.log(prefix 
      + " " + keys[i] + ": " 
      + totals[keys[i]].time.toFixed(3) + "ms"
      + " (" + totals[keys[i]].count.toFixed() +")");
  }
}

function perfLog(name, startTime) {
  gPerf.push({name: name, time: window.performance.now() - startTime});
}

function perfClear() {
  gPerf = [];
}

/**********************************************************
 * STAGE PAGE Logic
 **********************************************************/

// Stage helper constructor
function StageHelper(params) {
  this.pageSeed        = params.seed;
  this.formAction      = params.formAction;
  this.choiseCount     = 0;
  this.lastSearchValue = "";

  // DOM nodes
  this.dom = {
    stageTab:     document.getElementById(params.ids.stageTab),
    commitBtn:    document.getElementById(params.ids.commitBtn),
    commitAllBtn: document.getElementById(params.ids.commitAllBtn),
    objectSearch: document.getElementById(params.ids.objectSearch),
    fileCounter:  document.getElementById(params.ids.fileCounter)
  };
  
  // Table columns (autodetection)
  this.col = this.detectColumns();

  // Constants
  this.HIGHLIGHT_STYLE = "highlight";
  this.STATUS = {
    add:    "A",
    remove: "R",
    ignore: "I",
    reset:  "?",
    isValid: function (status) { return "ARI?".indexOf(status) == -1; }
  };
  
  this.TEMPLATES = {
    cmdReset:  "<a>reset</a>",
    cmdLocal:  "<a>add</a>",
    cmdRemote: "<a>ignore</a><a>remove</a>"
  };

  this.setHooks();
}

// Hook global click listener on table, load/unload actions
StageHelper.prototype.setHooks = function() {
  this.dom.stageTab.onclick        = this.onTableClick.bind(this);
  this.dom.commitBtn.onclick       = this.submit.bind(this);
  this.dom.objectSearch.oninput    = this.onSearch.bind(this);
  this.dom.objectSearch.onkeypress = this.onSearch.bind(this);
  window.onbeforeunload            = this.onPageUnload.bind(this);
  window.onload                    = this.onPageLoad.bind(this);
}

// Detect column index
StageHelper.prototype.detectColumns = function() {
  var dataRow  = this.dom.stageTab.tBodies[0].rows[0];
  var cols     = {};

  for (var i = dataRow.cells.length - 1; i >= 0; i--) {
    if (dataRow.cells[i].className) cols[dataRow.cells[i].className] = i;
  }

  return cols;
}

// Store table state on leaving the page
StageHelper.prototype.onPageUnload = function() {
  if (!window.sessionStorage) return;

  var data = this.collectData();
  window.sessionStorage.setItem(this.pageSeed, JSON.stringify(data));
}

// Re-store table state on entering the page
StageHelper.prototype.onPageLoad = function() {
  var data = window.sessionStorage && JSON.parse(window.sessionStorage.getItem(this.pageSeed));

  this.iterateStageTab(true, function (row) {
    var status = data && data[row.cells[this.col.name].innerText];
    this.updateRow(row, status || this.STATUS.reset);
  });

  this.updateMenu();
  debugOutput("StageHelper.onPageLoad: " + ((data) ? "from Storage" : "initial state"));
}

// Table event handler, change status
StageHelper.prototype.onTableClick = function (event) {
  var target = event.target || event.srcElement;
  if (!target) return;

  if (target.tagName === "A") {
    var td = target.parentNode;
  } else if (target.tagName === "TD") {
    var td = target;
    if (td.children.length === 1 && td.children[0].tagName === "A") {
      target = td.children[0];
    } else return;
  } else return;

  if (["TD","TH"].indexOf(td.tagName) == -1 || td.className != "cmd") return;
  
  var status    = this.STATUS[target.innerText]; // Convert anchor text to status
  var targetRow = td.parentNode;
  
  if (td.tagName === "TD") {
    this.updateRow(targetRow, status);
  } else { // TH
    this.iterateStageTab(true, function (row) {
      if (row.style.display !== "none"            // Not filtered out
        && row.className === targetRow.className  // Same context as header
        ) {
        this.updateRow(row, status);
      }
    });
  }

  this.updateMenu();
}

// Search object
StageHelper.prototype.onSearch = function (e) {
  if ( // Enter hit or clear, IE SUCKS !
       e.type === "input" && !e.target.value && this.lastSearchValue
    || e.type === "keypress" && e.which === 13 ) { 

    this.lastSearchValue = e.target.value;
    this.iterateStageTab(true, this.applyFilterToRow, e.target.value);
  }
}

StageHelper.prototype.applyFilterToRow = function (row, filter) {
  var td      = row.cells[this.col.name];
  var origTxt = td.innerText; // without tags
  var newTxt  = "";

  if (filter) {
    newTxt = origTxt.replace(filter, "<mark>"+filter+"</mark>");
    row.style.display = (newTxt !== origTxt) ? "" : "none";
  } else { // No filter -> just reset the value
    newTxt            = origTxt;
    row.style.display = ""; // default, visible
  }

  if (td.firstChild.tagName === "A") {
    td.firstChild.innerHTML = newTxt;
  } else {
    td.innerHTML = newTxt;
  }
}

// Get how status should affect object counter
StageHelper.prototype.getStatusImpact = function (status) {
  if (typeof status !== "string" 
    || status.length !== 1 
    || this.STATUS.isValid(status) ) {
    alert("Unknown status");
  } else {
    return (status !== this.STATUS.reset) ? 1 : 0;
  }
}

// Update table line
StageHelper.prototype.updateRow = function (row, newStatus) {
  var oldStatus = row.cells[this.col.status].innerText;

  if (oldStatus !== newStatus) {
    this.updateRowStatus(row, newStatus);
    this.updateRowCommand(row, newStatus);
  } else if (!row.cells[this.col.cmd].children.length) {
    this.updateRowCommand(row, newStatus); // For initial run
  }

  this.choiseCount += this.getStatusImpact(newStatus) - this.getStatusImpact(oldStatus);
}

// Update Status cell (render set of commands)
StageHelper.prototype.updateRowStatus = function (row, status) {
  row.cells[this.col.status].innerText = status;
  if (status === this.STATUS.reset) {
    row.cells[this.col.status].classList.remove(this.HIGHLIGHT_STYLE);
  } else {
    row.cells[this.col.status].classList.add(this.HIGHLIGHT_STYLE);
  }
}

// Update Command cell (render set of commands)
StageHelper.prototype.updateRowCommand = function (row, status) {
  var cell = row.cells[this.col.cmd];
  if (status === this.STATUS.reset) {
    cell.innerHTML = (row.className == "local") 
      ? this.TEMPLATES.cmdLocal 
      : this.TEMPLATES.cmdRemote;
  } else {
    cell.innerHTML = this.TEMPLATES.cmdReset;
  }
}

// Update menu items visibility
StageHelper.prototype.updateMenu = function () {
  this.dom.commitBtn.style.display    = (this.choiseCount > 0) ? "" : "none";
  this.dom.commitAllBtn.style.display = (this.choiseCount > 0) ? "none" : "";
  this.dom.fileCounter.innerHTML      = this.choiseCount.toString();
}

// Submit stage state to the server
StageHelper.prototype.submit = function () {
  submitSapeventForm(this.collectData(), this.formAction);
}

// Extract data from the table
StageHelper.prototype.collectData = function () {
  var data  = {};
  this.iterateStageTab(false, function (row) {
    data[row.cells[this.col.name].innerText] = row.cells[this.col.status].innerText;
  });
  return data;
}

// Table iteration helper
StageHelper.prototype.iterateStageTab = function (changeMode, cb /*, ...*/) {
  
  var restArgs = Array.prototype.slice.call(arguments, 2);
  var table    = this.dom.stageTab;

  if (changeMode) {
    var scrollOffset = window.pageYOffset;
    this.dom.stageTab.style.display = "none";
    // var stageTabParent = this.dom.stageTab.parentNode;
    // table = stageTabParent.removeChild(this.dom.stageTab);
  }

  for (var b = 0, bN = table.tBodies.length; b < bN; b++) {
    var tbody = table.tBodies[b];
    for (var r = 0, rN = tbody.rows.length; r < rN; r++) {
      args = [tbody.rows[r]].concat(restArgs);
      cb.apply(this, args); // callback
    }
  }

  if (changeMode) {
    this.dom.stageTab.style.display = "";
    // stageTabParent.appendChild(table);
    window.scrollTo(0, scrollOffset);
  }
}
