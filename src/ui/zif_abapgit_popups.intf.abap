interface ZIF_ABAPGIT_POPUPS
  public .


  types:
    BEGIN OF ty_popup,
      url          TYPE string,
      package      TYPE devclass,
      branch_name  TYPE string,
      display_name TYPE string,
      folder_logic TYPE string,
      ign_subpkg   TYPE abap_bool,
      cancel       TYPE abap_bool,
    END OF ty_popup .

  constants C_NEW_BRANCH_LABEL type STRING value '+ create new ...' ##NO_TEXT.

  methods POPUP_PACKAGE_EXPORT
    exporting
      !EV_PACKAGE type DEVCLASS
      !EV_FOLDER_LOGIC type STRING
      !EV_SERIALIZE_MASTER_LANG_ONLY type ABAP_BOOL
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods POPUP_FOLDER_LOGIC
    returning
      value(RV_FOLDER_LOGIC) type STRING
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods POPUP_OBJECT
    returning
      value(RS_TADIR) type ZIF_ABAPGIT_DEFINITIONS=>TY_TADIR
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods CREATE_BRANCH_POPUP
    exporting
      !EV_NAME type STRING
      !EV_CANCEL type ABAP_BOOL
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods RUN_PAGE_CLASS_POPUP
    exporting
      !EV_NAME type STRING
      !EV_CANCEL type ABAP_BOOL
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods REPO_NEW_OFFLINE
    returning
      value(RS_POPUP) type ZIF_ABAPGIT_POPUPS=>TY_POPUP
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods BRANCH_LIST_POPUP
    importing
      !IV_URL type STRING
      !IV_DEFAULT_BRANCH type STRING optional
      !IV_SHOW_NEW_OPTION type ABAP_BOOL optional
      !IV_HIDE_BRANCH type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-BRANCH_NAME optional
      !IV_HIDE_HEAD type ABAP_BOOL optional
    returning
      value(RS_BRANCH) type ZIF_ABAPGIT_DEFINITIONS=>TY_GIT_BRANCH
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods REPO_POPUP
    importing
      !IV_URL type STRING
      !IV_PACKAGE type DEVCLASS optional
      !IV_BRANCH type STRING default 'refs/heads/master'
      !IV_FREEZE_PACKAGE type ABAP_BOOL optional
      !IV_FREEZE_URL type ABAP_BOOL optional
      !IV_TITLE type CLIKE default 'New Online Project'
      !IV_DISPLAY_NAME type STRING optional
    returning
      value(RS_POPUP) type ZIF_ABAPGIT_POPUPS=>TY_POPUP
    raising
      ZCX_ABAPGIT_EXCEPTION  ##NO_TEXT.
  methods POPUP_TO_CONFIRM
    importing
      !IV_TITLEBAR type CLIKE
      !IV_TEXT_QUESTION type CLIKE
      !IV_TEXT_BUTTON_1 type CLIKE default 'Yes'
      !IV_ICON_BUTTON_1 type ICON-NAME default SPACE
      !IV_TEXT_BUTTON_2 type CLIKE default 'No'
      !IV_ICON_BUTTON_2 type ICON-NAME default SPACE
      !IV_DEFAULT_BUTTON type CHAR1 default '1'
      !IV_DISPLAY_CANCEL_BUTTON type CHAR1 default ABAP_TRUE
    returning
      value(RV_ANSWER) type CHAR1
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods POPUP_TO_INFORM
    importing
      !IV_TITLEBAR type CLIKE
      !IV_TEXT_MESSAGE type CLIKE
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods POPUP_TO_CREATE_PACKAGE
    exporting
      !ES_PACKAGE_DATA type SCOMPKDTLN
      !EV_CREATE type ABAP_BOOL
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods POPUP_TO_CREATE_TRANSP_BRANCH
    importing
      !IT_TRANSPORT_HEADERS type TRWBO_REQUEST_HEADERS
    returning
      value(RS_TRANSPORT_BRANCH) type ZIF_ABAPGIT_DEFINITIONS=>TY_TRANSPORT_TO_BRANCH
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods POPUP_TO_SELECT_TRANSPORTS
    returning
      value(RT_TRKORR) type TRWBO_REQUEST_HEADERS .
  methods POPUP_TO_SELECT_FROM_LIST
    importing
      !IT_LIST type STANDARD TABLE
      !IV_HEADER_TEXT type CSEQUENCE
      !IV_SELECT_COLUMN_TEXT type CSEQUENCE
      !IT_COLUMNS_TO_DISPLAY type STRING_TABLE
    exporting
      value(ET_LIST) type STANDARD TABLE
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods BRANCH_POPUP_CALLBACK
    importing
      !IV_CODE type CLIKE
    changing
      !CT_FIELDS type ZIF_ABAPGIT_DEFINITIONS=>TY_SVAL_TT
      !CS_ERROR type SVALE
      !CV_SHOW_POPUP type CHAR01
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods PACKAGE_POPUP_CALLBACK
    importing
      !IV_CODE type CLIKE
    changing
      !CT_FIELDS type ZIF_ABAPGIT_DEFINITIONS=>TY_SVAL_TT
      !CS_ERROR type SVALE
      !CV_SHOW_POPUP type CHAR01
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods POPUP_TRANSPORT_REQUEST
    importing
      !IS_TRANSPORT_TYPE type ZIF_ABAPGIT_DEFINITIONS=>TY_TRANSPORT_TYPE
    returning
      value(RV_TRANSPORT) type TRKORR
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods POPUP_PROXY_BYPASS
    importing
      !IT_PROXY_BYPASS type ZIF_ABAPGIT_DEFINITIONS=>TY_RANGE_PROXY_BYPASS_URL
    returning
      value(RT_PROXY_BYPASS) type ZIF_ABAPGIT_DEFINITIONS=>TY_RANGE_PROXY_BYPASS_URL
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods POPUP_TO_SELECT_CUSTOMIZING_TR
    returning
      value(RV_TRANSPORT_REQUEST) type TRKORR .
endinterface.
