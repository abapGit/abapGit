interface ZIF_ABAPGIT_OO_OBJECT_FNC
  public .


  types:
    BEGIN OF ty_includes,
           programm TYPE programm,
         END OF ty_includes .
  types:
    ty_includes_tt TYPE STANDARD TABLE OF ty_includes WITH DEFAULT KEY .

  methods CREATE
    importing
      !IV_PACKAGE type DEVCLASS
      !IV_OVERWRITE type ABAP_BOOL default ABAP_TRUE
      !IT_ATTRIBUTES type ZIF_ABAPGIT_DEFINITIONS=>TY_OBJ_ATTRIBUTE_TT optional
    changing
      !CG_PROPERTIES type ANY
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods GENERATE_LOCALS
    importing
      !IS_KEY type SEOCLSKEY
      !IV_FORCE type ABAP_BOOL default ABAP_TRUE
      !IT_LOCAL_DEFINITIONS type SEOP_SOURCE_STRING optional
      !IT_LOCAL_IMPLEMENTATIONS type SEOP_SOURCE_STRING optional
      !IT_LOCAL_MACROS type SEOP_SOURCE_STRING optional
      !IT_LOCAL_TEST_CLASSES type SEOP_SOURCE_STRING optional
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods DESERIALIZE_SOURCE
    importing
      !IS_KEY type SEOCLSKEY
      !IT_SOURCE type ZIF_ABAPGIT_DEFINITIONS=>TY_STRING_TT
    raising
      ZCX_ABAPGIT_EXCEPTION
      CX_SY_DYN_CALL_ERROR .
  methods INSERT_TEXT_POOL
    importing
      !IV_CLASS_NAME type SEOCLSNAME
      !IT_TEXT_POOL type TEXTPOOL_TABLE
      !IV_LANGUAGE type SPRAS
      !IV_STATE type C default 'I'
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods UPDATE_DESCRIPTIONS
    importing
      !IS_KEY type SEOCLSKEY
      !IT_DESCRIPTIONS type ZIF_ABAPGIT_DEFINITIONS=>TY_SEOCOMPOTX_TT .
  methods ADD_TO_ACTIVATION_LIST
    importing
      !IS_ITEM type ZIF_ABAPGIT_DEFINITIONS=>TY_ITEM
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods CREATE_SOTR
    importing
      !IV_PACKAGE type DEVCLASS
      !IT_SOTR type ZIF_ABAPGIT_DEFINITIONS=>TY_SOTR_TT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods CREATE_DOCUMENTATION
    importing
      !IT_LINES type TLINETAB
      !IV_OBJECT_NAME type DOKHL-OBJECT
      !IV_LANGUAGE type SPRAS
      !IV_NO_MASTERLANG type ABAP_BOOL optional
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods GET_INCLUDES
    importing
      !IV_OBJECT_NAME type SOBJ_NAME
    returning
      value(RT_INCLUDES) type TY_INCLUDES_TT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods EXISTS
    importing
      !IS_OBJECT_NAME type SEOCLSKEY
    returning
      value(RV_EXISTS) type ABAP_BOOL .
  methods SERIALIZE_ABAP
    importing
      !IS_CLASS_KEY type SEOCLSKEY
      !IV_TYPE type SEOP_INCLUDE_EXT_APP optional
    returning
      value(RT_SOURCE) type ZIF_ABAPGIT_DEFINITIONS=>TY_STRING_TT
    raising
      ZCX_ABAPGIT_EXCEPTION
      CX_SY_DYN_CALL_ERROR .
  methods GET_SKIP_TEST_CLASSES
    returning
      value(RV_SKIP) type ABAP_BOOL .
  methods GET_CLASS_PROPERTIES
    importing
      !IS_CLASS_KEY type SEOCLSKEY
    returning
      value(RS_CLASS_PROPERTIES) type VSEOCLASS
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods GET_INTERFACE_PROPERTIES
    importing
      !IS_INTERFACE_KEY type SEOCLSKEY
    returning
      value(RS_INTERFACE_PROPERTIES) type VSEOINTERF
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods READ_TEXT_POOL
    importing
      !IV_CLASS_NAME type SEOCLSNAME
      !IV_LANGUAGE type SPRAS
    returning
      value(RT_TEXT_POOL) type TEXTPOOL_TABLE .
  methods READ_DOCUMENTATION
    importing
      !IV_CLASS_NAME type SEOCLSNAME
      !IV_LANGUAGE type SPRAS
    returning
      value(RT_LINES) type TLINETAB .
  methods READ_SOTR
    importing
      !IV_OBJECT_NAME type SOBJ_NAME
    returning
      value(RT_SOTR) type ZIF_ABAPGIT_DEFINITIONS=>TY_SOTR_TT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods READ_DESCRIPTIONS
    importing
      !IV_OBEJCT_NAME type SEOCLSNAME
    returning
      value(RT_DESCRIPTIONS) type ZIF_ABAPGIT_DEFINITIONS=>TY_SEOCOMPOTX_TT .
  methods DELETE
    importing
      !IS_DELETION_KEY type SEOCLSKEY
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods READ_SUPERCLASS
    importing
      !IV_CLASSNAME type SEOCLSNAME
    returning
      value(RV_SUPERCLASS) type SEOCLSNAME .
  methods READ_ATTRIBUTES
    importing
      !IV_OBJECT_NAME type SEOCLSNAME
    returning
      value(RT_ATTRIBUTES) type ZIF_ABAPGIT_DEFINITIONS=>TY_OBJ_ATTRIBUTE_TT .
endinterface.
