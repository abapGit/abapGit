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
function submitSapeventForm(params, action, method) {
  var form = document.createElement("form");
  form.setAttribute("method", method || "post");
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

// Set focus to a element with query selector
function setInitialFocusWithQuerySelector(sSelector, bFocusParent) {
  var oSelected = document.querySelector(sSelector);

  if (oSelected) {
    if (bFocusParent) {
      oSelected.parentElement.focus();
    } else {
      oSelected.focus();
    }
  }

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
 * TAG PAGE Logic
 **********************************************************/
// somehow only functions on window are visible for the select tag
window.onTagTypeChange = function(oSelectObject){
  var sValue = oSelectObject.value;
  submitSapeventForm({ 'type': sValue }, "change_tag_type", "post");
}

/**********************************************************
 * Repo Overview Logic
 **********************************************************/
// somehow only functions on window are visible for the select tag
window.onOrderByChange = function(oSelectObject){
  var sValue = oSelectObject.value;
  submitSapeventForm({ 'orderBy': sValue }, "change_order_by", "post");
}

window.onDirectionChange = function(oSelectObject){
  var sValue = oSelectObject.value;
  submitSapeventForm({ 'direction': sValue }, "direction", "post");
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
  this.colIndex      = this.detectColumns();
  this.filterTargets = ["name", "user"];

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
  var colIndex = {};

  for (var i = dataRow.cells.length - 1; i >= 0; i--) {
    if (dataRow.cells[i].className) colIndex[dataRow.cells[i].className] = i;
  }

  return colIndex;
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
    var status = data && data[row.cells[this.colIndex["name"]].innerText];
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

// Apply filter to a single stage line - hide or show
StageHelper.prototype.applyFilterToRow = function (row, filter) {
  // Collect data cells
  var targets = this.filterTargets.map(function(attr) {
    var elem = row.cells[this.colIndex[attr]];
    if (elem.firstChild && elem.firstChild.tagName === "A") elem = elem.firstChild;
    return {
      elem:      elem,
      plainText: elem.innerText, // without tags
      curHtml:   elem.innerHTML
    };
  }, this);

  var isVisible = false; 

  // Apply filter to cells, mark filtered text
  for (var i = targets.length - 1; i >= 0; i--) {
    var target = targets[i];
    target.newHtml = (filter)
      ? target.plainText.replace(filter, "<mark>"+filter+"</mark>")
      : target.plainText;
    target.isChanged = target.newHtml !== target.curHtml;
    isVisible        = isVisible || !filter || target.newHtml !== target.plainText;
  }

  // Update DOM
  row.style.display = isVisible ? "" : "none";
  for (var i = targets.length - 1; i >= 0; i--) {
    if (targets[i].isChanged) targets[i].elem.innerHTML = targets[i].newHtml;
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
  var oldStatus = row.cells[this.colIndex["status"]].innerText;

  if (oldStatus !== newStatus) {
    this.updateRowStatus(row, newStatus);
    this.updateRowCommand(row, newStatus);
  } else if (!row.cells[this.colIndex["cmd"]].children.length) {
    this.updateRowCommand(row, newStatus); // For initial run
  }

  this.choiseCount += this.getStatusImpact(newStatus) - this.getStatusImpact(oldStatus);
}

// Update Status cell (render set of commands)
StageHelper.prototype.updateRowStatus = function (row, status) {
  row.cells[this.colIndex["status"]].innerText = status;
  if (status === this.STATUS.reset) {
    row.cells[this.colIndex["status"]].classList.remove(this.HIGHLIGHT_STYLE);
  } else {
    row.cells[this.colIndex["status"]].classList.add(this.HIGHLIGHT_STYLE);
  }
}

// Update Command cell (render set of commands)
StageHelper.prototype.updateRowCommand = function (row, status) {
  var cell = row.cells[this.colIndex["cmd"]];
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
  this.dom.commitBtn.style.display    = (this.choiseCount > 0) ? ""     : "none";
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
    data[row.cells[this.colIndex["name"]].innerText] = row.cells[this.colIndex["status"]].innerText;
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
    window.scrollTo(0, scrollOffset);
  }
}

/**********************************************************
 * Check list wrapper
 **********************************************************/

function CheckListWrapper(id, cbAction) {
  this.id         = document.getElementById(id);
  this.cbAction   = cbAction;
  this.id.onclick = this.onClick.bind(this);
}

CheckListWrapper.prototype.onClick = function(e) {
  // Get nodes
  var target = event.target || event.srcElement;
  if (!target) return;
  if (target.tagName !== "A") { target = target.parentNode; } // icon clicked
  if (target.tagName !== "A") return;
  if (target.parentNode.tagName !== "LI") return;

  var nodeA    = target;
  var nodeLi   = target.parentNode;
  var nodeIcon = target.children[0];
  if (!nodeIcon.classList.contains("octicon")) return;

  // Node updates
  var option   = nodeA.innerText;
  var oldState = nodeLi.getAttribute("data-check");
  if (oldState === null) return; // no data-check attribute - non-checkbox
  var newState = oldState === "X" ? false : true;

  if (newState) {
    nodeIcon.classList.remove("grey");
    nodeIcon.classList.add("blue");
    nodeLi.setAttribute("data-check", "X");
  } else {
    nodeIcon.classList.remove("blue");
    nodeIcon.classList.add("grey");
    nodeLi.setAttribute("data-check", "");
  }

  // Action callback
  this.cbAction(nodeLi.getAttribute("data-aux"), option, newState);
}

/**********************************************************
 * Diff page logic
 **********************************************************/

// Diff helper constructor
function DiffHelper(params) {
  this.pageSeed    = params.seed;
  this.counter     = 0;
  this.stageAction = params.stageAction;

  // DOM nodes
  this.dom = {
    diffList:    document.getElementById(params.ids.diffList),
    stageButton: document.getElementById(params.ids.stageButton)
  };

  this.repoKey = this.dom.diffList.getAttribute("data-repo-key");
  if (!this.repoKey) return; // Unexpected

  // Checklist wrapper
  if (document.getElementById(params.ids.filterMenu)) {
    this.checkList = new CheckListWrapper(params.ids.filterMenu, this.onFilter.bind(this));
    this.dom.filterButton = document.getElementById(params.ids.filterMenu).parentNode;
  } 

  // Hijack stage command
  if (this.dom.stageButton) {
    this.dom.stageButton.href    = "#";
    this.dom.stageButton.onclick = this.onStage.bind(this);
  }
}

// Action on filter click
DiffHelper.prototype.onFilter = function(attr, target, state) {
  this.applyFilter(attr, target, state);
  this.highlightButton(state);
};

// Hide/show diff based on params
DiffHelper.prototype.applyFilter = function (attr, target, state) {
  this.iterateDiffList(function(div) {
    if (div.getAttribute("data-"+attr) === target) {
      div.style.display = state ? "" : "none";
    }
  });
}

// Action on stage -> save visible diffs as state for stage page
DiffHelper.prototype.onStage = function (e) {
  if (window.sessionStorage) {
    var data = this.buildStageCache();
    window.sessionStorage.setItem(this.pageSeed, JSON.stringify(data));
  }
  var getParams = {key: this.repoKey, seed: this.pageSeed};
  submitSapeventForm(getParams, this.stageAction, "get");
}

// Collect visible diffs
DiffHelper.prototype.buildStageCache = function () {
  var list = {};
  this.iterateDiffList(function(div) {
    var filename = div.getAttribute("data-file");
    if (!div.style.display && filename) { // No display override - visible !!
      list[filename] = "A"; // Add
    }
  });
  return list;
}

// Table iterator
DiffHelper.prototype.iterateDiffList = function (cb /*, ...*/) {
  var restArgs = Array.prototype.slice.call(arguments, 1);
  var diffList = this.dom.diffList;

  for (var i = 0, iN = diffList.children.length; i < iN; i++) {
    var div = diffList.children[i];
    if (div.className !== "diff") continue;
    args = [div].concat(restArgs);
    cb.apply(this, args); // callback
  }
}

// Highlight Filter button if filter is activate
DiffHelper.prototype.highlightButton = function(state) {
  this.counter += state ? -1 : 1;
  if (this.counter > 0) {
    this.dom.filterButton.classList.add("bgorange");
  } else {
    this.dom.filterButton.classList.remove("bgorange");
  }
};

/**********************************************************
 * Other functions
 **********************************************************/

// News announcement
function displayNews() {
  var div = document.getElementById("news");
  div.style.display = (div.style.display) ? '' : 'none';
}

function KeyNavigation() {
  
}

KeyNavigation.prototype.onkeydown = function(oEvent) {

  if (oEvent.defaultPrevented) {
    return;
  }

  // navigate with arrows through list items and support pressing links with enter and space
  if (oEvent.key === "ENTER" || oEvent.key === "") {
    this.onEnterOrSpace(oEvent);
  } else if (/Down$/.test(oEvent.key)) {
    this.onArrowDown(oEvent);
  } else if (/Up$/.test(oEvent.key)) {
    this.onArrowUp(oEvent);
  }

};

KeyNavigation.prototype.getLiSelected = function() {
  return document.querySelector('li .selected');
};

KeyNavigation.prototype.getActiveElement = function () {
  return document.activeElement;
};

KeyNavigation.prototype.getActiveElementParent = function () {
  return this.getActiveElement().parentElement;
};

KeyNavigation.prototype.onEnterOrSpace = function (oEvent) {
  
  // Enter or space clicks the selected link

  var liSelected = this.getLiSelected();

  if (liSelected) {
    liSelected.firstElementChild.click();
  }

};


KeyNavigation.prototype.onArrowDown = function (oEvent) {

  var
    liNext,
    liSelected = this.getLiSelected(),
    oActiveElementParent = this.getActiveElementParent();

  if (liSelected) {

    // we deselect the current li and select the next sibling
    liNext = oActiveElementParent.nextElementSibling;
    if (liNext) {
      liSelected.classList.toggle('selected');
      liNext.firstElementChild.focus();
      oActiveElementParent.classList.toggle('selected');
      oEvent.preventDefault();
    }

  } else {

    // we don't have any li selected, we have lookup where to start...
    // the right element should have been activated in fnTooltipActivate
    liNext = this.getActiveElement().nextElementSibling;
    if (liNext) {
      liNext.classList.toggle('selected');
      liNext.firstElementChild.firstElementChild.focus();
      oEvent.preventDefault();
    }

  }

};


KeyNavigation.prototype.onArrowUp = function (oEvent) {

  var
    liSelected = this.getLiSelected(),
    liPrevious = this.getActiveElementParent().previousElementSibling;

  if (liSelected && liPrevious) {

    liSelected.classList.toggle('selected');
    liPrevious.firstElementChild.focus();
    this.getActiveElementParent().classList.toggle('selected');
    oEvent.preventDefault();

  }

};

// this functions enables the navigation with arrows through list items (li)
// e.g. in dropdown menus
function enableArrowListNavigation() {

  var oKeyNavigation = new KeyNavigation();

  document.addEventListener('keydown', oKeyNavigation.onkeydown.bind(oKeyNavigation));

};

function LinkHints(sLinkHintKey, sColor){
  this.sLinkHintKey = sLinkHintKey; 
  this.sColor = sColor;
  this.oTooltipMap = {};
  this.bTooltipsOn = false;
  this.sPending = "";
  this.aTooltipElements = document.querySelectorAll('a span');
}

LinkHints.prototype.fnRenderTooltip = function (oTooltip, iTooltipCounter) {
  if (this.bTooltipsOn) {
    oTooltip.classList.remove('hidden');
  } else {
    oTooltip.classList.add('hidden');
  }
  oTooltip.innerHTML = iTooltipCounter;
  oTooltip.style.backgroundColor = this.sColor;
  this.oTooltipMap[iTooltipCounter] = oTooltip;
};

LinkHints.prototype.getTooltipStartValue = function(iToolTipCount){
  
  // if whe have 333 tooltips we start from 100
  return Math.pow(10,iToolTipCount.toString().length - 1);

};

LinkHints.prototype.fnRenderTooltips = function () {

  // all possible links which should be accessed via tooltip have
  // sub span which is hidden by default. If we like to show the 
  // tooltip we have to toggle the css class 'hidden'.
  // 
  // We use numeric values for the tooltip label. Maybe we can 
  // support also alphanumeric chars in the future. Then we have to
  // calculate permutations and that's work. So for the sake of simplicity
  // we stick to numeric values and just increment them.

  var
    iTooltipCounter = this.getTooltipStartValue(this.aTooltipElements.length),
    that = this;

  [].forEach.call(this.aTooltipElements, function(oTooltip){
    iTooltipCounter += 1;
    this.fnRenderTooltip(oTooltip, iTooltipCounter)
  }.bind(that));

};

LinkHints.prototype.fnToggleAllTooltips = function () {

  this.sPending = "";
  this.bTooltipsOn = !this.bTooltipsOn;
  this.fnRenderTooltips();

};

LinkHints.prototype.fnRemoveAllTooltips = function () {

  this.sPending = "";
  this.bTooltipsOn = false;

  [].forEach.call(this.aTooltipElements, function (oTooltip) {
    oTooltip.classList.add('hidden');
  });

};

LinkHints.prototype.fnFilterTooltips = function (sPending) {

  var that = this;

  Object
    .keys(this.oTooltipMap)
    .forEach(function (sKey) {

      // we try to partially match, but only from the beginning!
      var regex = new RegExp("^" + this.sPending);
      var oTooltip = this.oTooltipMap[sKey];

      if (regex.test(sKey)) {
        // we have a partial match, grey out the matched part
        oTooltip.innerHTML = sKey.replace(regex, "<div style='display:inline;color:lightgray'>" + this.sPending + '</div>');
      } else {
        // and hide the not matched tooltips
        oTooltip.classList.add('hidden');
      }

    }.bind(that));

};

LinkHints.prototype.fnActivateDropDownMenu = function (oTooltip) {
  // to enable link hint navigation for drop down menu, we must expand 
  // like if they were hovered
  oTooltip.parentElement.parentElement.classList.toggle("block");
};


LinkHints.prototype.fnTooltipActivate = function (oTooltip) {

  // a tooltips was successfully specified, so we try to trigger the link
  // and remove all tooltips
  this.fnRemoveAllTooltips();
  oTooltip.parentElement.click();

  // in case it is a dropdownmenu we have to expand and focus it
  this.fnActivateDropDownMenu(oTooltip);
  oTooltip.parentElement.focus();

}

LinkHints.prototype.onkeypress = function(oEvent){

  if (oEvent.defaultPrevented) {
    return;
  }

  var activeElementType = ((document.activeElement && document.activeElement.nodeName) || "");
  
  // link hints are disabled for input and textareas for obvious reasons.
  // Maybe we must add other types here in the future
  if (oEvent.key === this.sLinkHintKey && activeElementType !== "INPUT" && activeElementType !== "TEXTAREA") {

    this.fnToggleAllTooltips();

  } else if (this.bTooltipsOn === true) {
    
    // the user tries to reach a tooltip
    this.sPending += oEvent.key;
    var oTooltip = this.oTooltipMap[this.sPending];

    if (oTooltip) {
      // we are there, we have a fully specified tooltip. Let's activate it
      this.fnTooltipActivate(oTooltip);
    } else {
      // we are not there yet, but let's filter the link so that only
      // the partially matched are shown
      this.fnFilterTooltips(this.sPending);
    }

  }

}

// Vimium like link hints
function setLinkHints(sLinkHintKey, sColor) {

  if (!sLinkHintKey || !sColor) {
    return;
  }

  var oLinkHint = new LinkHints(sLinkHintKey, sColor);

  document.addEventListener("keypress", oLinkHint.onkeypress.bind(oLinkHint));

}


