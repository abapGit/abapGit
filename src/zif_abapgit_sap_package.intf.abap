INTERFACE zif_abapgit_sap_package
  PUBLIC .


  TYPES:
    ty_devclass_tt TYPE STANDARD TABLE OF devclass WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_devclass_info,
      DEVCLASS  TYPE DEVCLASS,
      NAMESPACE TYPE NAMESPACE,
      PARENTCL  TYPE PARENTCL,
    END OF ty_devclass_info .
  TYPES:
    ty_devclass_info_tt TYPE SORTED TABLE OF ty_devclass_info
      WITH UNIQUE KEY devclass
      WITH NON-UNIQUE SORTED KEY parent COMPONENTS parentcl .

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
      !IV_BUFFERED type ABAP_BOOL default ABAP_FALSE
    returning
      value(RT_LIST) type TY_DEVCLASS_TT .
  methods LIST_SUPERPACKAGES
    returning
      value(RT_LIST) type TY_DEVCLASS_TT .
  methods READ_PARENT
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
