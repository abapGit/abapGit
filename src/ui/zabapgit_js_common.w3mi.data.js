/**********************************************************
 * ABAPGIT JS function library
 **********************************************************/

/**********************************************************
  Global variables used from outside
 **********************************************************/
/* exported setInitialFocus */
/* exported setInitialFocusWithQuerySelector */
/* exported submitFormById */
/* exported errorStub */
/* exported confirmInitialized */
/* exported perfOut */
/* exported perfLog */
/* exported perfClear */
/* exported enableArrowListNavigation */
/* exported setLinkHints */
/* exported setKeyBindings */
/* exported preparePatch */
/* exported registerStagePatch */
/* exported toggleRepoListDetail */
/* exported onDirectionChange */
/* exported onOrderByChange  */
/* exported onTagTypeChange */
/* exported errorMessagePanelRegisterClick */

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
        return fToBind.apply(
          this instanceof fNOP
            ? this
            : oThis,
          aArgs.concat(Array.prototype.slice.call(arguments))
        );
      };

    if (this.prototype) {
      fNOP.prototype = this.prototype;
    }
    fBound.prototype = new fNOP();

    return fBound;
  };
}

// String includes polyfill, taken from https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/includes
if (!String.prototype.includes) {
  String.prototype.includes = function(search, start) {
    "use strict";
    if (typeof start !== "number") {
      start = 0;
    }

    if (start + search.length > this.length) {
      return false;
    } else {
      return this.indexOf(search, start) !== -1;
    }
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
  for (var j = keys.length - 1; j >= 0; j--) {
    console.log(prefix
      + " " + keys[j] + ": "
      + totals[keys[j]].time.toFixed(3) + "ms"
      + " (" + totals[keys[j]].count.toFixed() +")");
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
function onTagTypeChange(oSelectObject){
  var sValue = oSelectObject.value;
  submitSapeventForm({ type: sValue }, "change_tag_type", "post");
}

/**********************************************************
 * Repo Overview Logic
 **********************************************************/
function onOrderByChange(oSelectObject){
  var sValue = oSelectObject.value;
  submitSapeventForm({ orderBy: sValue }, "change_order_by", "post");
}

function onDirectionChange(oSelectObject){
  var sValue = oSelectObject.value;
  submitSapeventForm({ direction: sValue }, "direction", "post");
}

function findStyleSheetByName(name) {
  var classes = document.styleSheets[0].cssRules || document.styleSheets[0].rules;
  for (var i = 0; i < classes.length; i++) {
    if (classes[i].selectorText === name) return classes[i];
  }
}

function toggleRepoListDetail() {
  var detailClass = findStyleSheetByName(".ro-detail");
  if (detailClass) {
    detailClass.style.display = detailClass.style.display === "none" ? "" : "none";
  }
}

/**********************************************************
 * STAGE PAGE Logic
 **********************************************************/

// Stage helper constructor
function StageHelper(params) {
  this.pageSeed        = params.seed;
  this.formAction      = params.formAction;
  this.choiseCount     = 0;
  this.lastFilterValue = "";

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
  this.filterTargets = ["name", "user", "transport"];

  // Constants
  this.HIGHLIGHT_STYLE = "highlight";
  this.STATUS = {
    add:    "A",
    remove: "R",
    ignore: "I",
    reset:  "?",
    isValid: function (status) { return "ARI?".indexOf(status) == -1 }
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
  this.dom.objectSearch.oninput    = this.onFilter.bind(this);
  this.dom.objectSearch.onkeypress = this.onFilter.bind(this);
  window.onbeforeunload            = this.onPageUnload.bind(this);
  window.onload                    = this.onPageLoad.bind(this);
};

// Detect column index
StageHelper.prototype.detectColumns = function() {
  var dataRow  = this.dom.stageTab.tBodies[0].rows[0];
  var colIndex = {};

  for (var i = dataRow.cells.length - 1; i >= 0; i--) {
    if (dataRow.cells[i].className) colIndex[dataRow.cells[i].className] = i;
  }

  return colIndex;
};

// Store table state on leaving the page
StageHelper.prototype.onPageUnload = function() {
  if (!window.sessionStorage) return;

  var data = this.collectData();
  window.sessionStorage.setItem(this.pageSeed, JSON.stringify(data));
};

// Re-store table state on entering the page
StageHelper.prototype.onPageLoad = function() {
  var data = window.sessionStorage && JSON.parse(window.sessionStorage.getItem(this.pageSeed));

  this.iterateStageTab(true, function (row) {
    var status = data && data[row.cells[this.colIndex["name"]].innerText];
    this.updateRow(row, status || this.STATUS.reset);
  });

  this.updateMenu();
  if (this.dom.objectSearch.value) {
    this.applyFilterValue(this.dom.objectSearch.value);
  }
  debugOutput("StageHelper.onPageLoad: " + ((data) ? "from Storage" : "initial state"));
};

// Table event handler, change status
StageHelper.prototype.onTableClick = function (event) {
  var target = event.target || event.srcElement;
  if (!target) return;

  var td;
  if (target.tagName === "A") {
    td = target.parentNode;
  } else if (target.tagName === "TD") {
    td = target;
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
};

// Search object
StageHelper.prototype.onFilter = function (e) {
  if ( // Enter hit or clear, IE SUCKS !
    e.type === "input" && !e.target.value && this.lastFilterValue
    || e.type === "keypress" && e.which === 13 ) {

    this.applyFilterValue(e.target.value);
    submitSapeventForm({ filterValue: e.target.value }, "stage_filter", "post");
  }
};

StageHelper.prototype.applyFilterValue = function(sFilterValue) {

  this.lastFilterValue = sFilterValue;
  this.iterateStageTab(true, this.applyFilterToRow, sFilterValue);

};

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
  for (var j = targets.length - 1; j >= 0; j--) {
    if (targets[j].isChanged) targets[j].elem.innerHTML = targets[j].newHtml;
  }
};

// Get how status should affect object counter
StageHelper.prototype.getStatusImpact = function (status) {
  if (typeof status !== "string"
    || status.length !== 1
    || this.STATUS.isValid(status) ) {
    alert("Unknown status");
  } else {
    return (status !== this.STATUS.reset) ? 1 : 0;
  }
};

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
};

// Update Status cell (render set of commands)
StageHelper.prototype.updateRowStatus = function (row, status) {
  row.cells[this.colIndex["status"]].innerText = status;
  if (status === this.STATUS.reset) {
    row.cells[this.colIndex["status"]].classList.remove(this.HIGHLIGHT_STYLE);
  } else {
    row.cells[this.colIndex["status"]].classList.add(this.HIGHLIGHT_STYLE);
  }
};

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
};

// Update menu items visibility
StageHelper.prototype.updateMenu = function () {
  this.dom.commitBtn.style.display    = (this.choiseCount > 0) ? ""     : "none";
  this.dom.commitAllBtn.style.display = (this.choiseCount > 0) ? "none" : "";
  this.dom.fileCounter.innerHTML      = this.choiseCount.toString();
};

// Submit stage state to the server
StageHelper.prototype.submit = function () {
  submitSapeventForm(this.collectData(), this.formAction);
};

// Extract data from the table
StageHelper.prototype.collectData = function () {
  var data  = {};
  this.iterateStageTab(false, function (row) {
    data[row.cells[this.colIndex["name"]].innerText] = row.cells[this.colIndex["status"]].innerText;
  });
  return data;
};

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
      var args = [tbody.rows[r]].concat(restArgs);
      cb.apply(this, args); // callback
    }
  }

  if (changeMode) {
    this.dom.stageTab.style.display = "";
    window.scrollTo(0, scrollOffset);
  }
};

/**********************************************************
 * Check list wrapper
 **********************************************************/

function CheckListWrapper(id, cbAction) {
  this.id         = document.getElementById(id);
  this.cbAction   = cbAction;
  this.id.onclick = this.onClick.bind(this);
}

CheckListWrapper.prototype.onClick = function(e) { // eslint-disable-line no-unused-vars
  // Get nodes
  var target = event.target || event.srcElement;
  if (!target) return;
  if (target.tagName !== "A") { target = target.parentNode } // icon clicked
  if (target.tagName !== "A") return;
  if (target.parentNode.tagName !== "LI") return;

  var nodeA    = target;
  var nodeLi   = target.parentNode;
  var nodeIcon = target.children[0];
  if (!nodeIcon.classList.contains("icon")) return;

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
};

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
};

// Action on stage -> save visible diffs as state for stage page
DiffHelper.prototype.onStage = function (e) { // eslint-disable-line no-unused-vars
  if (window.sessionStorage) {
    var data = this.buildStageCache();
    window.sessionStorage.setItem(this.pageSeed, JSON.stringify(data));
  }
  var getParams = {key: this.repoKey, seed: this.pageSeed};
  submitSapeventForm(getParams, this.stageAction, "get");
};

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
};

// Table iterator
DiffHelper.prototype.iterateDiffList = function (cb /*, ...*/) {
  var restArgs = Array.prototype.slice.call(arguments, 1);
  var diffList = this.dom.diffList;

  for (var i = 0, iN = diffList.children.length; i < iN; i++) {
    var div = diffList.children[i];
    if (div.className !== "diff") continue;
    var args = [div].concat(restArgs);
    cb.apply(this, args); // callback
  }
};

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
function toggleDisplay(divId) {
  var div = document.getElementById(divId);
  if (div) div.style.display = (div.style.display) ? "" : "none";
}

function KeyNavigation() {

}

KeyNavigation.prototype.onkeydown = function(oEvent) {

  if (oEvent.defaultPrevented) {
    return;
  }

  // navigate with arrows through list items and support pressing links with enter and space
  if (oEvent.key === "ENTER" || oEvent.key === "") {
    this.onEnterOrSpace();
  } else if (/Down$/.test(oEvent.key)) {
    this.onArrowDown(oEvent);
  } else if (/Up$/.test(oEvent.key)) {
    this.onArrowUp(oEvent);
  }

};

KeyNavigation.prototype.getLiSelected = function() {
  return document.querySelector("li .selected");
};

KeyNavigation.prototype.getActiveElement = function () {
  return document.activeElement;
};

KeyNavigation.prototype.getActiveElementParent = function () {
  return this.getActiveElement().parentElement;
};

KeyNavigation.prototype.onEnterOrSpace = function () {

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
      liSelected.classList.toggle("selected");
      liNext.firstElementChild.focus();
      oActiveElementParent.classList.toggle("selected");
      oEvent.preventDefault();
    }

  } else {

    // we don't have any li selected, we have lookup where to start...
    // the right element should have been activated in fnTooltipActivate
    liNext = this.getActiveElement().nextElementSibling;
    if (liNext) {
      liNext.classList.toggle("selected");
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

    liSelected.classList.toggle("selected");
    liPrevious.firstElementChild.focus();
    this.getActiveElementParent().classList.toggle("selected");
    oEvent.preventDefault();

  }

};

// this functions enables the navigation with arrows through list items (li)
// e.g. in dropdown menus
function enableArrowListNavigation() {

  var oKeyNavigation = new KeyNavigation();

  document.addEventListener("keydown", oKeyNavigation.onkeydown.bind(oKeyNavigation));

}

function LinkHints(sLinkHintKey, sColor){
  this.sLinkHintKey = sLinkHintKey;
  this.sColor = sColor;
  this.oTooltipMap = {};
  this.bTooltipsOn = false;
  this.sPending = "";
  this.aTooltipElements = document.querySelectorAll("span.tooltiptext");
}

LinkHints.prototype.renderTooltip = function (oTooltip, iTooltipCounter) {
  if (this.bTooltipsOn) {
    oTooltip.classList.remove("hidden");
  } else {
    oTooltip.classList.add("hidden");
  }
  oTooltip.innerHTML = iTooltipCounter;
  oTooltip.style.backgroundColor = this.sColor;
  this.oTooltipMap[iTooltipCounter] = oTooltip;
};

LinkHints.prototype.getTooltipStartValue = function(iToolTipCount){

  // if whe have 333 tooltips we start from 100
  return Math.pow(10,iToolTipCount.toString().length - 1);

};

LinkHints.prototype.renderTooltips = function () {

  // all possible links which should be accessed via tooltip have
  // sub span which is hidden by default. If we like to show the
  // tooltip we have to toggle the css class 'hidden'.
  //
  // We use numeric values for the tooltip label. Maybe we can
  // support also alphanumeric chars in the future. Then we have to
  // calculate permutations and that's work. So for the sake of simplicity
  // we stick to numeric values and just increment them.

  var
    iTooltipCounter = this.getTooltipStartValue(this.aTooltipElements.length);

  [].forEach.call(this.aTooltipElements, function(oTooltip){
    iTooltipCounter += 1;
    this.renderTooltip(oTooltip, iTooltipCounter);
  }.bind(this));

};

LinkHints.prototype.toggleAllTooltips = function () {

  this.sPending = "";
  this.bTooltipsOn = !this.bTooltipsOn;
  this.renderTooltips();

};

LinkHints.prototype.disableTooltips = function(){
  this.sPending = "";
  this.bTooltipsOn = false;
};

LinkHints.prototype.removeAllTooltips = function () {

  this.disableTooltips();

  [].forEach.call(this.aTooltipElements, function (oTooltip) {
    oTooltip.classList.add("hidden");
  });

};

LinkHints.prototype.filterTooltips = function () {

  Object
    .keys(this.oTooltipMap)
    .forEach(function (sKey) {

      // we try to partially match, but only from the beginning!
      var regex = new RegExp("^" + this.sPending);
      var oTooltip = this.oTooltipMap[sKey];

      if (regex.test(sKey)) {
        // we have a partial match, grey out the matched part
        oTooltip.innerHTML = sKey.replace(regex, "<div style='display:inline;color:lightgray'>" + this.sPending + "</div>");
      } else {
        // and hide the not matched tooltips
        oTooltip.classList.add("hidden");
      }

    }.bind(this));

};

LinkHints.prototype.activateDropDownMenu = function (oTooltip) {
  // to enable link hint navigation for drop down menu, we must expand
  // like if they were hovered
  oTooltip.parentElement.parentElement.classList.toggle("block");
};


LinkHints.prototype.tooltipActivate = function (oTooltip) {

  // a tooltips was successfully specified, so we try to trigger the link
  // and remove all tooltips
  this.removeAllTooltips();

  // we have technically 2 scenarios
  // 1) hint to a checkbox: as input field cannot include tags
  //    we place the span after input
  // 2) hint to a link: the span in included in the anchor tag

  var elInput = oTooltip.parentElement.querySelector("input");

  if (elInput) {
    // case 1) toggle the checkbox
    elInput.click();
  } else {
    // case 2) click the link
    oTooltip.parentElement.click();
  }

  // in case it is a dropdownmenu we have to expand and focus it
  this.activateDropDownMenu(oTooltip);
  oTooltip.parentElement.focus();

};

LinkHints.prototype.onkeypress = function(oEvent){

  if (oEvent.defaultPrevented) {
    return;
  }

  var activeElementType = ((document.activeElement && document.activeElement.nodeName) || "");

  // link hints are disabled for input and textareas for obvious reasons.
  // Maybe we must add other types here in the future
  if (oEvent.key === this.sLinkHintKey && activeElementType !== "INPUT" && activeElementType !== "TEXTAREA") {

    this.toggleAllTooltips();

  } else if (this.bTooltipsOn === true) {

    // the user tries to reach a tooltip
    this.sPending += oEvent.key;
    var oTooltip = this.oTooltipMap[this.sPending];

    if (oTooltip) {
      // we are there, we have a fully specified tooltip. Let's activate it
      this.tooltipActivate(oTooltip);
    } else {
      // we are not there yet, but let's filter the link so that only
      // the partially matched are shown
      this.filterTooltips();
      this.disableTooltipsIfNoTooltipIsVisible();
    }

  }

};

LinkHints.prototype.disableTooltipsIfNoTooltipIsVisible = function(){

  if (!this.isAnyTooltipVisible()) {
    this.disableTooltips();
  }
};

LinkHints.prototype.isAnyTooltipVisible = function(){

  return (Object
    .keys(this.oTooltipMap)
    .filter(function (key) {
      return !this.oTooltipMap[key].classList.contains("hidden");
    }.bind(this)).length > 0);

};

// Vimium like link hints
function setLinkHints(sLinkHintKey, sColor) {

  if (!sLinkHintKey || !sColor) {
    return;
  }

  var oLinkHint = new LinkHints(sLinkHintKey, sColor);

  document.addEventListener("keypress", oLinkHint.onkeypress.bind(oLinkHint));

}

function Hotkeys(oKeyMap){

  this.oKeyMap = oKeyMap || {};

  // these are the hotkeys provided by the backend
  Object.keys(this.oKeyMap).forEach(function(sKey){

    var action = this.oKeyMap[sKey];

    // We replace the actions with callback functions to unify
    // the hotkey execution
    this.oKeyMap[sKey] = function(oEvent) {

      // We have either a js function on this
      if (this[action]) {
        this[action].call(this);
        return;
      }

      // Or a global function
      if (window[action]) {
        window[action].call(this);
      }

      // Or a SAP event
      var sUiSapEvent = this.getSapEvent(action);
      if (sUiSapEvent) {
        submitSapeventForm({}, sUiSapEvent, "post");
        oEvent.preventDefault();
        return;
      }

    };

  }.bind(this));

}

Hotkeys.prototype.showHotkeys = function() {
  var elHotkeys = document.querySelector("#hotkeys");

  if (elHotkeys) {
    elHotkeys.style.display = (elHotkeys.style.display) ? "" : "none";
  }
};

Hotkeys.prototype.getSapEvent = function(sSapEvent) {

  var fnNormalizeSapEventHref = function(sSapEvent, oSapEvent) {
    if (new RegExp(sSapEvent + "$" ).test(oSapEvent.href)
    || (new RegExp(sSapEvent + "\\?" ).test(oSapEvent.href))) {
      return oSapEvent.href.replace("sapevent:","");
    }
  };

  var aSapEvents = document.querySelectorAll('a[href^="sapevent:' + sSapEvent + '"]');

  var aFilteredAndNormalizedSapEvents =
    [].map.call(aSapEvents, function(oSapEvent){
      return fnNormalizeSapEventHref(sSapEvent, oSapEvent);
    }).filter(function(elem){
      // remove false positives
      return (elem && !elem.includes("sapevent:"));
    });

  return (aFilteredAndNormalizedSapEvents && aFilteredAndNormalizedSapEvents[0]);

};

Hotkeys.prototype.onkeydown = function(oEvent){

  if (oEvent.defaultPrevented) {
    return;
  }

  var activeElementType = ((document.activeElement && document.activeElement.nodeName) || "");

  if (activeElementType === "INPUT" || activeElementType === "TEXTAREA") {
    return;
  }

  var
    sKey = oEvent.key || String.fromCharCode(oEvent.keyCode),
    fnHotkey = this.oKeyMap[sKey];

  if (fnHotkey) {
    fnHotkey.call(this, oEvent);
  }
};

function setKeyBindings(oKeyMap){

  var oHotkeys = new Hotkeys(oKeyMap);

  document.addEventListener("keypress", oHotkeys.onkeydown.bind(oHotkeys));
  setTimeout(function(){
    var div = document.getElementById("hotkeys-hint");
    if (div) div.style.opacity = 0.2;
  }, 4900);
  setTimeout(function(){ toggleDisplay("hotkeys-hint") }, 5000);
}

/*
  Patch / git add -p
  */

/*
  We have three type of cascading checkboxes.
  Which means that by clicking a file or section checkbox all corresponding line checkboxes are checked.

  The id of the checkbox indicates its semantics and its membership.

  */

/*
  1) file links

      example id of file link

      patch_file_zcl_abapgit_user_exit.clas.abap
      \________/ \_____________________________/
          |                   |
          |                   |____ file name
          |
          |
          |
      constant prefix

  */

function PatchFile(sId){
  var oRegex = new RegExp("(" + this.ID + ")_(.*$)");
  var oMatch = sId.match(oRegex);
  this.id        = sId;
  this.prefix    = oMatch[1];
  this.file_name = oMatch[2];
}

PatchFile.prototype.ID = "patch_file";

/*
  2) section links within a file

      example id of section link

      patch_section_zcl_abapgit_user_exit.clas.abap_1
      \___________/ \_____________________________/ ^
            |                   |                   |
            |               file name               |
            |                                       |
            |                                       ------ section
            |
      constant prefix

    */

function PatchSection(sId){
  var oRegex = new RegExp("(" + this.ID + ")_(.*)_(\\d+$)");
  var oMatch = sId.match(oRegex);
  this.id        = sId;
  this.prefix    = oMatch[1];
  this.file_name = oMatch[2];
  this.section   = oMatch[3];
}

PatchSection.prototype.ID = "patch_section";

/*
  3) line links within a section

      example id of line link

      patch_line_zcl_abapgit_user_exit.clas.abap_1_25
      \________/ \_____________________________/ ^  ^
            ^                  ^                 |  |
            |                  |                 |  ------- line number
            |               file name            |
            |                                 section
            |
            |
      constant prefix

  */
function PatchLine(){
}

PatchLine.prototype.ID = "patch_line";

function Patch() { }

Patch.prototype.ID = {
  STAGE: "stage"
};

Patch.prototype.ACTION = {
  PATCH_STAGE: "patch_stage"
};

Patch.prototype.escape = function(sFileName){
  return sFileName
    .replace(/\./g, "\\.")
    .replace(/#/g, "\\#");
};

Patch.prototype.preparePatch = function(){

  this.registerClickHandlerForFiles();
  this.registerClickHandlerForSections();
  this.registerClickHandlerForLines();

};

Patch.prototype.buildSelectorInputStartsWithId = function(sId){
  return "input[id^='" + sId + "']";
};

Patch.prototype.registerClickHandlerForFiles = function(){
  this.registerClickHandlerForSelectorParent(this.buildSelectorInputStartsWithId(PatchFile.prototype.ID), this.onClickFileCheckbox);
};

Patch.prototype.registerClickHandlerForSections = function(){
  this.registerClickHandlerForSelectorParent(this.buildSelectorInputStartsWithId(PatchSection.prototype.ID), this.onClickSectionCheckbox);
};

Patch.prototype.registerClickHandlerForLines = function(){
  this.registerClickHandlerForSelectorParent(this.buildSelectorInputStartsWithId(PatchLine.prototype.ID), this.onClickLineCheckbox);
};

Patch.prototype.registerClickHandlerForSelectorParent = function(sSelector, fnCallback){

  var elAll = document.querySelectorAll(sSelector);

  [].forEach.call(elAll, function(elem){
    elem.parentElement.addEventListener("click", fnCallback.bind(this));
  }.bind(this));

};

Patch.prototype.getAllLineCheckboxesForFile = function(oFile){
  return this.getAllLineCheckboxesForId(oFile.id, PatchFile.prototype.ID);
};

Patch.prototype.getAllSectionCheckboxesForFile = function(oFile){
  return this.getAllSectionCheckboxesForId(oFile.id, PatchFile.prototype.ID);
};

Patch.prototype.getAllLineCheckboxesForSection = function(oSection){
  return this.getAllLineCheckboxesForId(oSection.id, PatchSection.prototype.ID);
};

Patch.prototype.getAllLineCheckboxesForId = function(sId, sIdPrefix){
  return this.getAllCheckboxesForId(sId, sIdPrefix,PatchLine.prototype.ID);
};

Patch.prototype.getAllSectionCheckboxesForId = function(sId, sIdPrefix){
  return this.getAllCheckboxesForId(sId, sIdPrefix, PatchSection.prototype.ID);
};

Patch.prototype.getAllCheckboxesForId = function(sId, sIdPrefix, sNewIdPrefix){
  var oRegex = new RegExp("^" + sIdPrefix);
  sId = sId.replace(oRegex, sNewIdPrefix);
  return document.querySelectorAll(this.buildSelectorInputStartsWithId(this.escape(sId)));
};

Patch.prototype.getToggledCheckbox = function(oEvent){

  var elCheckbox = null;

  // We have either an input element or any element with input child
  // in the latter case we have to toggle the checkbox manually
  if (oEvent.srcElement.nodeName === "INPUT"){
    elCheckbox = oEvent.srcElement;
  } else {
    elCheckbox = this.toggleCheckbox(oEvent.srcElement.querySelector("INPUT"));
  }

  return elCheckbox;
};

Patch.prototype.toggleCheckbox = function(elCheckbox) {
  elCheckbox.checked = !elCheckbox.checked;
  return elCheckbox;
};

Patch.prototype.onClickFileCheckbox = function(oEvent) {

  var elCheckbox = this.getToggledCheckbox(oEvent);
  var oFile = new PatchFile(elCheckbox.id);
  var elAllLineCheckboxesOfFile = this.getAllLineCheckboxesForFile(oFile);
  var elAllSectionCheckboxesOfFile = this.getAllSectionCheckboxesForFile(oFile);

  [].forEach.call(elAllLineCheckboxesOfFile,function(elem){
    elem.checked = elCheckbox.checked;
  }.bind(this));

  [].forEach.call(elAllSectionCheckboxesOfFile,function(elem){
    elem.checked = elCheckbox.checked;
  }.bind(this));

};

Patch.prototype.onClickSectionCheckbox = function(oEvent){
  var elSrcElement = this.getToggledCheckbox(oEvent);
  var oSection = new PatchSection(elSrcElement.id);
  this.clickAllLineCheckboxesInSection(oSection, elSrcElement.checked);
};

Patch.prototype.onClickLineCheckbox = function(oEvent){
  this.getToggledCheckbox(oEvent);
};

Patch.prototype.clickAllLineCheckboxesInSection = function(oSection, bChecked){

  var elAllLineCheckboxesOfSection = this.getAllLineCheckboxesForSection(oSection);

  [].forEach.call(elAllLineCheckboxesOfSection,function(elem){
    elem.checked = bChecked;
  }.bind(this));

};

Patch.prototype.registerStagePatch = function registerStagePatch(){

  var elStage = document.querySelector("#" + this.ID.STAGE);
  elStage.addEventListener("click", this.stagePatch.bind(this));

  // for hotkeys
  window.stagePatch = function(){
    this.stagePatch();
  }.bind(this);

};

Patch.prototype.stagePatch = function() {

  // Collect add and remove info and submit to backend

  var aAddPatch = this.collectElementsForCheckboxId(PatchLine.prototype.ID, true);
  var aRemovePatch = this.collectElementsForCheckboxId(PatchLine.prototype.ID, false);

  submitSapeventForm({"add": aAddPatch, "remove": aRemovePatch}, this.ACTION.PATCH_STAGE, "post");

};

Patch.prototype.collectElementsForCheckboxId = function(sId, bChecked){

  var sSelector = this.buildSelectorInputStartsWithId(sId);

  return [].slice.call(document.querySelectorAll(sSelector))
    .filter(function(elem){
      return (elem.checked === bChecked);
    }).map(function(elem){
      return elem.id;
    });

};

function preparePatch(){
  var oPatch = new Patch();
  oPatch.preparePatch();
}

function registerStagePatch(){
  var oPatch = new Patch();
  oPatch.registerStagePatch();
}

/**********************************************************
 * Page branch overview
 *
 * Hovering a commit node in the branch overview will show
 * a popup with the commit details. Single click on a node
 * will fix the popup, so that users can select text. The
 * fixation is removed when any node is hovered or the popup
 * is closed via 'X'.
 *
 **********************************************************/

function BranchOverview() {
  this.bFixed = false;
  this.elCurrentCommit = {
    style : {
      display: "none"
    }
  };
}

BranchOverview.prototype.toggleCommit = function(sSha1, bFixPopup) {

  // If the popup is fixed, we just remove the fixation.
  // The popup will then be hidden by the next call of hideCommit
  if (this.bFixed) {
    this.bFixed = false;
    return;
  }

  // We hide the previous shown commit popup
  this.elCurrentCommit.style.display = "none";

  // Display the new commit popup if sha1 is supplied
  if (sSha1){
    this.elCurrentCommit = document.getElementById(sSha1);
    this.elCurrentCommit.style.display = "";

    // and fix the popup so that the next hideCommit won't hide it.
    this.bFixed = bFixPopup;

  }

};

// called by onClick of commit nodes in branch overview
BranchOverview.prototype.onCommitClick = function(commit){
  this.toggleCommit(commit.sha1, true);
};

// Called by commit:mouseover
BranchOverview.prototype.showCommit = function(event){
  this.toggleCommit(event.data.sha1);
};

// Called by commit:mouseout
BranchOverview.prototype.hideCommit = function (){
  this.toggleCommit();
};

// Initialize Top Horizontal Scroller on GitGraph
function setGitGraphScroller(){ // eslint-disable-line no-unused-vars

  // Get gitGraph Element Canvas Width
  var gitGraphEl = document.getElementById("gitGraph");
  var gitGraphWidth = gitGraphEl.offsetWidth;

  // Initialize gitGraph-HTopScroller Element width as gitGraph
  var HTopScrollerEl = document.querySelector(".gitGraph-HTopScroller");
  HTopScrollerEl.style.width = gitGraphWidth + "px";

}

// Setup Top Horizontal Scroller on GitGraph event
function GitGraphScroller() { // eslint-disable-line no-unused-vars
  var gitGraphWrapperEl = document.querySelector(".gitGraph-Wrapper");
  var gitGraphscrollWrapperEl = document.querySelector(".gitGraph-scrollWrapper");
  gitGraphWrapperEl.scrollLeft = gitGraphscrollWrapperEl.scrollLeft;
}

// Click on error message panel toggles longtext
function errorMessagePanelRegisterClick(){
  var elMessage = document.getElementById("message");
  if (elMessage){
    elMessage.addEventListener("click", function(oEvent){
      toggleMessageDetail(oEvent);
    });
  }
}

function toggleMessageDetail(oEvent){
  if (oEvent &&  ( oEvent.target.id === "a_goto_source"
                || oEvent.target.id === "a_callstack"
                || oEvent.target.id === "a_goto_message") ) {
    return;
  }
  toggleDisplay("message-detail");
}
