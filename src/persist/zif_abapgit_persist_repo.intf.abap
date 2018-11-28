interface ZIF_ABAPGIT_PERSIST_REPO
  public .


  methods ADD
    importing
      !IV_URL type STRING
      !IV_BRANCH_NAME type STRING
      !IV_BRANCH type ZIF_ABAPGIT_DEFINITIONS=>TY_SHA1 optional
      !IV_PACKAGE type DEVCLASS
      !IV_OFFLINE type SAP_BOOL default ABAP_FALSE
      !IS_DOT_ABAPGIT type ZIF_ABAPGIT_DOT_ABAPGIT=>TY_DOT_ABAPGIT
    returning
      value(RV_KEY) type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-KEY
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods DELETE
    importing
      !IV_KEY type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-KEY
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods LIST
    returning
      value(RT_REPOS) type ZIF_ABAPGIT_PERSISTENCE=>TT_REPO
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods LOCK
    importing
      !IV_MODE type ENQMODE
      !IV_KEY type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-KEY
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods READ
    importing
      !IV_KEY type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-KEY
    returning
      value(RS_REPO) type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO
    raising
      ZCX_ABAPGIT_EXCEPTION
      ZCX_ABAPGIT_NOT_FOUND .
  methods UPDATE_METADATA
    importing
      !IV_KEY type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-KEY
      !IS_META type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO_XML
      !IS_CHANGE_MASK type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO_META_MASK
    raising
      ZCX_ABAPGIT_EXCEPTION .
endinterface.
