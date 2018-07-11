interface ZIF_ABAPGIT_SAP_PACKAGE
  public .


  types:
    ty_devclass_tt TYPE STANDARD TABLE OF devclass WITH DEFAULT KEY .

  methods CREATE
    importing
      !IS_PACKAGE type SCOMPKDTLN
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods CREATE_LOCAL
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods LIST_SUBPACKAGES
    importing
      !IT_DEVC_INFO type ZIF_ABAPGIT_DEFINITIONS=>TT_DEVC_BUFFER optional
    returning
      value(RT_LIST) type TY_DEVCLASS_TT .
  methods LIST_SUPERPACKAGES
    importing
      !IT_DEVC_INFO type ZIF_ABAPGIT_DEFINITIONS=>TT_DEVC_BUFFER optional
    returning
      value(RT_LIST) type TY_DEVCLASS_TT .
  methods READ_PARENT
    importing
      !IT_DEVC_INFO type ZIF_ABAPGIT_DEFINITIONS=>TT_DEVC_BUFFER optional
    returning
      value(RV_PARENTCL) type TDEVC-PARENTCL .
  methods CREATE_CHILD
    importing
      !IV_CHILD type DEVCLASS
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods EXISTS
    returning
      value(RV_BOOL) type ABAP_BOOL .
  methods ARE_CHANGES_RECORDED_IN_TR_REQ
    returning
      value(RV_ARE_CHANGES_REC_IN_TR_REQ) type ABAP_BOOL
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods GET_TRANSPORT_TYPE
    returning
      value(RV_TRANSPORT_TYPE) type ZIF_ABAPGIT_DEFINITIONS=>TY_TRANSPORT_TYPE
    raising
      ZCX_ABAPGIT_EXCEPTION .
endinterface.
