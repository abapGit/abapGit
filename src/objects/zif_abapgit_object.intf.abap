interface ZIF_ABAPGIT_OBJECT
  public .


  types TY_DESERIALIZATION_STEP type STRING .
  types:
    ty_deserialization_step_tt TYPE STANDARD TABLE OF ty_deserialization_step WITH DEFAULT KEY .
  types:
    ty_complete_status TYPE c LENGTH 1 .

  data MO_FILES type ref to ZCL_ABAPGIT_OBJECTS_FILES .
  constants:
    BEGIN OF gc_step_id,
      abap TYPE zif_abapgit_object=>ty_deserialization_step VALUE `ABAP`,
      ddic TYPE zif_abapgit_object=>ty_deserialization_step VALUE `DDIC`,
      late TYPE zif_abapgit_object=>ty_deserialization_step VALUE `LATE`,
    END OF gc_step_id .
  constants C_ABAP_VERSION_SAP_CP type PROGDIR-UCCHECK value '5' ##NO_TEXT.
  constants C_ABAP_VERSION_DEFAULT type PROGDIR-UCCHECK value 'X' ##NO_TEXT.

  methods SERIALIZE
    importing
      !IO_XML type ref to ZCL_ABAPGIT_XML_OUTPUT
      !II_LOG type ref to ZIF_ABAPGIT_LOG
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods DESERIALIZE
    importing
      !IV_PACKAGE type DEVCLASS
      !IO_XML type ref to ZCL_ABAPGIT_XML_INPUT
      !IV_STEP type TY_DESERIALIZATION_STEP
      !II_LOG type ref to ZIF_ABAPGIT_LOG
    returning
      value(RV_COMPLETE_STATUS) type TY_COMPLETE_STATUS
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods DELETE
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods EXISTS
    returning
      value(RV_BOOL) type ABAP_BOOL
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods IS_LOCKED
    returning
      value(RV_IS_LOCKED) type ABAP_BOOL
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods IS_ACTIVE
    returning
      value(RV_ACTIVE) type ABAP_BOOL
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods CHANGED_BY
    returning
      value(RV_USER) type XUBNAME
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods JUMP
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods GET_METADATA
    returning
      value(RS_METADATA) type ZIF_ABAPGIT_DEFINITIONS=>TY_METADATA .
  methods GET_COMPARATOR
    returning
      value(RI_COMPARATOR) type ref to ZIF_ABAPGIT_COMPARATOR
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods GET_DESERIALIZE_STEPS
    returning
      value(RT_STEPS) type TY_DESERIALIZATION_STEP_TT .
endinterface.
