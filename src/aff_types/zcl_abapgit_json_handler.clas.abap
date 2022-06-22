CLASS zcl_abapgit_json_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.

    "! Serializes data to xstring. Type of data is specified in the
    "! implementing class.
    "!
    "! @parameter iv_data | data to be serialized
    "! @parameter rv_result | serialized data
    METHODS serialize
      IMPORTING iv_data          TYPE data
      RETURNING VALUE(rv_result) TYPE xstring
      RAISING   cx_static_check.

    "! Deserializes xstring into data. The type of data is specified in
    "! the implementing class
    "!
    "! @parameter iv_content | xstring to be deserialized
    "! @parameter ev_data | data of the xstring
    METHODS deserialize
      IMPORTING iv_content TYPE xstring
      EXPORTING ev_data    TYPE data
      RAISING   cx_static_check.
  PROTECTED SECTION.

  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_abapgit_json_handler IMPLEMENTATION.


  METHOD deserialize.
    DATA lv_json    TYPE string.
    DATA lo_ajson   TYPE REF TO zcl_abapgit_ajson.
    DATA lo_mapping TYPE REF TO zif_abapgit_ajson_mapping.

    CLEAR ev_data.

    lv_json = zcl_abapgit_convert=>xstring_to_string_utf8( iv_content ).
    lo_mapping = zcl_abapgit_ajson_mapping=>create_camel_case( ).

    lo_ajson = zcl_abapgit_ajson=>parse( iv_json = lv_json
                                         ii_custom_mapping = lo_mapping ).

    lo_ajson->zif_abapgit_ajson~to_abap( IMPORTING ev_container = ev_data ).

  ENDMETHOD.


  METHOD serialize.
    DATA lt_st_source  TYPE abap_trans_srcbind_tab.
    DATA lo_mapping    TYPE REF TO lcl_mapping.
    DATA lv_json       TYPE string.
    DATA lo_ajson      TYPE REF TO zcl_abapgit_ajson.

    FIELD-SYMBOLS: <lg_source> LIKE LINE OF lt_st_source.

    APPEND INITIAL LINE TO lt_st_source ASSIGNING <lg_source>.
    GET REFERENCE OF iv_data INTO <lg_source>-value.
    CREATE OBJECT lo_mapping.

    lo_ajson = zcl_abapgit_ajson=>create_empty( ii_custom_mapping = lo_mapping ).
    lo_ajson->keep_item_order( ).
    lo_ajson->set(
      iv_path = '/'
      iv_val  = iv_data ).

    lv_json = lo_ajson->stringify( 2 ).

    rv_result = zcl_abapgit_convert=>string_to_xstring_utf8( iv_string = lv_json ).

  ENDMETHOD.
ENDCLASS.
