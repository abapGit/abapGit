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

/**********************************************************
 * STAGE PAGE Logic
 **********************************************************/

// Stage helper constructor
function StageHelper(params) {
  this.pageSeed        = params.seed;
  this.tabId           = params.stageTabId;
  this.formAction      = params.formAction;
  this.commitNodeId    = params.commitNodeId;
  this.commitAllNodeId = params.commitAllNodeId;
  this.choiseCount     = 0;
  this.setHook();
}

// Hook global click listener on table, load/unload actions
StageHelper.prototype.setHook = function() {
  var stageTab = document.getElementById(this.tabId);

  if (stageTab.addEventListener) {
    stageTab.addEventListener("click", this.onEvent.bind(this));
  } else {
    stageTab.attachEvent("onclick", this.onEvent.bind(this));
  }

  window.onbeforeunload = this.onPageUnload.bind(this);
  window.onload         = this.onPageLoad.bind(this);
}

// Store table state on leaving the page
StageHelper.prototype.onPageUnload = function() {
  var data = this.collectData();
  window.sessionStorage.setItem(this.pageSeed, JSON.stringify(data));
}

// Re-store table state on entering the page
StageHelper.prototype.onPageLoad = function() {
  var data  = JSON.parse(window.sessionStorage.getItem(this.pageSeed));
  var stage = document.getElementById(this.tabId);

  for (var i = stage.rows.length - 1; i >= 0; i--) {
    var tr      = stage.rows[i];
    if (tr.parentNode.tagName == "THEAD") continue;
    var context = tr.parentNode.className;
    var cmd     = data[tr.cells[1].innerText];
    if (!cmd) continue;
    
    this.formatTR(tr, cmd, context);
    this.choiseCount += (this.countChoiceImpact(cmd) > 0) ? 1 : 0;
  }

  this.updateMenu();
}

// Event handler, change status
StageHelper.prototype.onEvent = function (event) {
  if (!event.target) {
    if (event.srcElement) { event.target = event.srcElement; }
    else { return; }
  }

  if (event.target.tagName != "A") return;

  var td = event.target.parentNode;
  if (!td || td.tagName != "TD" || td.className != "cmd") return;
  
  var cmd     = event.target.innerText;
  var tr      = td.parentNode;
  var context = tr.parentNode.className;
  
  switch (cmd) {
    case "add":    cmd = "A"; break;
    case "remove": cmd = "R"; break;
    case "ignore": cmd = "I"; break;
    case "reset":  cmd = "?"; break;
  }
  
  this.formatTR(tr, cmd, context);
  this.choiseCount += this.countChoiceImpact(cmd);
  this.updateMenu();
}

// Update action counter -> affects menu update after
StageHelper.prototype.countChoiceImpact = function (cmd) {
  if ("ARI".indexOf(cmd) > -1) { return 1; }
  else if ("?".indexOf(cmd) > -1) { return -1; }
  else { alert("Unknown command"); }
}

// Re-format table line
StageHelper.prototype.formatTR = function (tr, cmd, context) {
  var cmdReset  = "<a>reset</a>";
  var cmdLocal  = "<a>add</a>";
  var cmdRemote = "<a>ignore</a><a>remove</a>";

  tr.cells[0].innerText   = cmd;
  if (cmd == "?") {
    tr.cells[0].style.color = "#CCC"; //grey
    tr.cells[2].innerHTML   = (context == "local") ? cmdLocal : cmdRemote;
  } else {
    tr.cells[0].style.color = "";
    tr.cells[2].innerHTML   = cmdReset;
  }
}

// Update menu items visibility
StageHelper.prototype.updateMenu = function () {
  if (this.choiseCount > 0) {
    document.getElementById(this.commitNodeId).style.display    = "inline";
    document.getElementById(this.commitAllNodeId).style.display = "none";
  } else {
    document.getElementById(this.commitNodeId).style.display    = "none";
    document.getElementById(this.commitAllNodeId).style.display = "inline";
  }
}

// Submin stage state to the server
StageHelper.prototype.submit = function () {
  var data = this.collectData();
  submitSapeventForm(data, this.formAction);
}

// Extract data from the table
StageHelper.prototype.collectData = function () {
  var stage = document.getElementById(this.tabId);
  var data  = {};

  for (var i = 0; i < stage.rows.length; i++) {
    var row = stage.rows[i];
    if (row.parentNode.tagName == "THEAD") continue;
    data[row.cells[1].innerText] = row.cells[0].innerText;
  }

  return data;
}
