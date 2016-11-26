//
// ABAPGIT JS function library
//

// Output text to the debug div
function debugOutput(text, dstID) {
  var stdout       = document.getElementById(dstID || "stdout");
  var wrapped      = "<p>" + text + "</p>";
  stdout.innerHTML = stdout.innerHTML + wrapped;
}

// Submit form data with sapevent
function submitForm(params, action) {
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

function setInitialFocus(id) {
  document.getElementById(id).focus();
}

function submitFormById(id) {
  document.getElementById(id).submit();
}


// STAGE
// Hook global click listener on table, global action counter
function setHook() {
  var stageTab = document.getElementById("stage_tab");

  if (stageTab.addEventListener) {
    stageTab.addEventListener("click", onEvent);
  } else {
    stageTab.attachEvent("onclick", onEvent);
  }

  window.onbeforeunload = onPageUnload;
  window.onload         = onPageLoad;
}

// Store table state on leaving the page
function onPageUnload() {
  var data = collectData();
  window.sessionStorage.setItem(gPageID, JSON.stringify(data));
}

// Re-store table state on entering the page
function onPageLoad() {
  var data  = JSON.parse(window.sessionStorage.getItem(gPageID));
  var stage = document.getElementById("stage_tab");

  for (var i = stage.rows.length - 1; i >= 0; i--) {
    var tr      = stage.rows[i];
    if (tr.parentNode.tagName == "THEAD") continue;
    var context = tr.parentNode.className;
    var cmd     = data[tr.cells[1].innerText];
    if (!cmd) continue;
    
    formatTR(tr, cmd, context);
    if (countChoiceImpact(cmd) > 0) {
      gChoiceCount++;
    }
  }

  updateMenu();
}

// Event handler, change status
function onEvent(event) {
  if (!event.target) {
    if (event.srcElement) {
      event.target = event.srcElement;
    } else {
      return;
    }
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
  
  formatTR(tr, cmd, context);
  gChoiceCount += countChoiceImpact(cmd);

  updateMenu();
}

// Update action counter -> affects menu update after
function countChoiceImpact(cmd) {
  if ("ARI".indexOf(cmd) > -1) {
    return 1;
  } else if ("?".indexOf(cmd) > -1) {
    return -1;
  } else {
    alert("Unknown command");
  }
}

// Re-format table line
function formatTR(tr, cmd, context) {
  var cmdReset  = "<a>reset</a>";
  var cmdLocal  = "<a>add</a>";
  var cmdRemote = "<a>ignore</a><a>remove</a>";

  tr.cells[0].innerText   = cmd;
  tr.cells[0].style.color = (cmd == "?") ? "#CCC" : "";
  tr.cells[2].innerHTML   = (cmd != "?") ? cmdReset
                                         :(context == "local") ? cmdLocal : cmdRemote;
}

// Update menu items visibility
function updateMenu() {
  if (gChoiceCount > 0) {
    document.getElementById("act_commit").style.display     = "inline";
    document.getElementById("act_commit_all").style.display = "none";
  } else {
    document.getElementById("act_commit").style.display     = "none";
    document.getElementById("act_commit_all").style.display = "inline";
  }
}

// Commit change to the server
function commit(action) {
  var data = collectData();
  submitForm(data, action);
}

// Extract data from the table
function collectData() {
  var stage = document.getElementById("stage_tab");
  var data  = {};

  for (var i = stage.rows.length - 1; i >= 0; i--) {
    var row = stage.rows[i];
    if (row.parentNode.tagName == "THEAD") continue;
    data[row.cells[1].innerText] = row.cells[0].innerText;
  }

  return data;
}
