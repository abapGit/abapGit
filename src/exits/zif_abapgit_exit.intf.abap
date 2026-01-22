interface ZIF_ABAPGIT_EXIT
  public .


  types:
    BEGIN OF ty_ci_repo,
      name      TYPE string,
      clone_url TYPE string,
    END OF ty_ci_repo .
  types:
    ty_ci_repos TYPE STANDARD TABLE OF ty_ci_repo WITH DEFAULT KEY .
  types:
    ty_object_types TYPE HASHED TABLE OF tadir-object WITH UNIQUE KEY table_line .
  types:
    BEGIN OF ty_class_key,
      clsname TYPE abap_classname,
    END OF ty_class_key .

  methods ADJUST_DISPLAY_COMMIT_URL
    importing
      !IV_REPO_URL type CSEQUENCE
      !IV_REPO_NAME type CSEQUENCE
      !IV_REPO_KEY type CSEQUENCE
      !IV_COMMIT_HASH type ZIF_ABAPGIT_GIT_DEFINITIONS=>TY_SHA1
    changing
      !CV_DISPLAY_URL type CSEQUENCE
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods ADJUST_DISPLAY_FILENAME
    importing
      !IS_REPO_META type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO
      !IV_FILENAME type STRING
    returning
      value(RV_FILENAME) type STRING .
  methods ALLOW_SAP_OBJECTS
    returning
      value(RV_ALLOWED) type ABAP_BOOL .
  methods CHANGE_COMMITTER_INFO
    importing
      !IV_REPO_URL type CSEQUENCE
    changing
      !CV_NAME type CSEQUENCE
      !CV_EMAIL type CSEQUENCE .
  methods CHANGE_LOCAL_HOST
    changing
      !CT_HOSTS type ZIF_ABAPGIT_DEFINITIONS=>TY_STRING_TT .
  methods CHANGE_MAX_PARALLEL_PROCESSES
    importing
      !IV_PACKAGE type DEVCLASS
    changing
      !CV_MAX_PROCESSES type I .
  methods CHANGE_PROXY_AUTHENTICATION
    importing
      !IV_REPO_URL type CSEQUENCE
    changing
      !CV_PROXY_AUTHENTICATION type ABAP_BOOL .
  methods CHANGE_PROXY_PORT
    importing
      !IV_REPO_URL type CSEQUENCE
    changing
      !CV_PROXY_PORT type STRING .
  methods CHANGE_PROXY_URL
    importing
      !IV_REPO_URL type CSEQUENCE
    changing
      !CV_PROXY_URL type STRING .
  methods CHANGE_RFC_SERVER_GROUP
    changing
      !CV_GROUP type RZLLI_APCL .
  methods CHANGE_SUPPORTED_DATA_OBJECTS
    changing
      !CT_OBJECTS type ZIF_ABAPGIT_DATA_SUPPORTER=>TY_OBJECTS .
  methods CHANGE_SUPPORTED_OBJECT_TYPES
    changing
      !CT_TYPES type TY_OBJECT_TYPES .
  methods CHANGE_TADIR
    importing
      !IV_PACKAGE type DEVCLASS
      !II_LOG type ref to ZIF_ABAPGIT_LOG
      !IS_DOT_ABAPGIT type ZIF_ABAPGIT_DOT_ABAPGIT=>TY_DOT_ABAPGIT
      !IV_IGNORE_SUBPACKAGES type ABAP_BOOL default ABAP_FALSE
      !IV_ONLY_LOCAL_OBJECTS type ABAP_BOOL default ABAP_FALSE
    changing
      !CT_TADIR type ZIF_ABAPGIT_DEFINITIONS=>TY_TADIR_TT .
  methods CREATE_HTTP_CLIENT
    importing
      !IV_URL type STRING
    returning
      value(RI_CLIENT) type ref to IF_HTTP_CLIENT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods CUSTOM_SERIALIZE_ABAP_CLIF
    importing
      !IS_CLASS_KEY type TY_CLASS_KEY
      !IT_SOURCE type ZIF_ABAPGIT_DEFINITIONS=>TY_STRING_TT optional
    returning
      value(RT_SOURCE) type ZIF_ABAPGIT_DEFINITIONS=>TY_STRING_TT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods DESERIALIZE_POSTPROCESS
    importing
      !IS_STEP type ZIF_ABAPGIT_OBJECTS=>TY_STEP_DATA
      !II_LOG type ref to ZIF_ABAPGIT_LOG .
  methods DETERMINE_TRANSPORT_REQUEST
    importing
      !II_REPO type ref to ZIF_ABAPGIT_REPO
      !IV_TRANSPORT_TYPE type ZIF_ABAPGIT_DEFINITIONS=>TY_TRANSPORT_TYPE
    changing
      !CV_TRANSPORT_REQUEST type TRKORR .
  methods ENHANCE_ANY_TOOLBAR
    importing
      !IO_MENU type ref to ZCL_ABAPGIT_HTML_TOOLBAR .
  methods ENHANCE_REPO_TOOLBAR
    importing
      !IO_MENU type ref to ZCL_ABAPGIT_HTML_TOOLBAR
      !IV_KEY type ZIF_ABAPGIT_PERSISTENCE=>TY_VALUE
      !IV_ACT type STRING .
  methods GET_CI_TESTS
    importing
      !IV_OBJECT type TADIR-OBJECT
    changing
      !CT_CI_REPOS type TY_CI_REPOS .
  methods GET_SSL_ID
    returning
      value(RV_SSL_ID) type SSFAPPLSSL .
  methods HTTP_CLIENT
    importing
      !IV_URL type STRING
      !II_CLIENT type ref to IF_HTTP_CLIENT .
  methods ON_EVENT
    importing
      !II_EVENT type ref to ZIF_ABAPGIT_GUI_EVENT
    returning
      value(RS_HANDLED) type ZIF_ABAPGIT_GUI_EVENT_HANDLER=>TY_HANDLING_RESULT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods PRE_CALCULATE_REPO_STATUS
    importing
      !IS_REPO_META type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO
    changing
      !CT_LOCAL type ZIF_ABAPGIT_DEFINITIONS=>TY_FILES_ITEM_TT
      !CT_REMOTE type ZIF_ABAPGIT_GIT_DEFINITIONS=>TY_FILES_TT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods SERIALIZE_POSTPROCESS
    importing
      !IV_PACKAGE type DEVCLASS
      !II_LOG type ref to ZIF_ABAPGIT_LOG
    changing
      !CT_FILES type ZIF_ABAPGIT_DEFINITIONS=>TY_FILES_ITEM_TT .
  methods VALIDATE_AFTER_PUSH
    importing
      !II_REPO_ONLINE type ref to ZIF_ABAPGIT_REPO_ONLINE
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods VALIDATE_BEFORE_PUSH
    importing
      !IS_COMMENT type ZIF_ABAPGIT_GIT_DEFINITIONS=>TY_COMMENT
      !IO_STAGE type ref to ZCL_ABAPGIT_STAGE
      !II_REPO_ONLINE type ref to ZIF_ABAPGIT_REPO_ONLINE
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods WALL_MESSAGE_LIST
    importing
      !II_HTML type ref to ZIF_ABAPGIT_HTML .
  methods WALL_MESSAGE_REPO
    importing
      !IS_REPO_META type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO
      !II_HTML type ref to ZIF_ABAPGIT_HTML .
endinterface.
