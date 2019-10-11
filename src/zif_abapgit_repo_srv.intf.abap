interface ZIF_ABAPGIT_REPO_SRV
  public .


  methods DELETE
    importing
      !IO_REPO type ref to ZCL_ABAPGIT_REPO
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods GET
    importing
      !IV_KEY type ZIF_ABAPGIT_PERSISTENCE=>TY_VALUE
    returning
      value(RO_REPO) type ref to ZCL_ABAPGIT_REPO
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods IS_REPO_INSTALLED
    importing
      !IV_URL type STRING
      !IV_TARGET_PACKAGE type DEVCLASS optional
    returning
      value(RV_INSTALLED) type ABAP_BOOL
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods LIST
    returning
      value(RT_LIST) type ZIF_ABAPGIT_DEFINITIONS=>TY_REPO_REF_TT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods NEW_OFFLINE
    importing
      !IV_URL type STRING
      !IV_PACKAGE type DEVCLASS
      !IV_FOLDER_LOGIC type STRING default ZIF_ABAPGIT_DOT_ABAPGIT=>C_FOLDER_LOGIC-FULL
    returning
      value(RO_REPO) type ref to ZCL_ABAPGIT_REPO_OFFLINE
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods NEW_ONLINE
    importing
      !IV_URL type STRING
      !IV_BRANCH_NAME type STRING
      !IV_DISPLAY_NAME type STRING optional
      !IV_PACKAGE type DEVCLASS
      !IV_FOLDER_LOGIC type STRING default 'PREFIX'
      !IV_IGN_SUBPKG type ABAP_BOOL default ABAP_FALSE
    returning
      value(RO_REPO) type ref to ZCL_ABAPGIT_REPO_ONLINE
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods PURGE
    importing
      !IO_REPO type ref to ZCL_ABAPGIT_REPO
      !IS_CHECKS type ZIF_ABAPGIT_DEFINITIONS=>TY_DELETE_CHECKS
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods VALIDATE_PACKAGE
    importing
      !IV_PACKAGE type DEVCLASS
      !IV_IGN_SUBPKG type ABAP_BOOL default ABAP_FALSE
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods INVALIDATE_BUFFER .
endinterface.
