CLASS zcl_abapgit_ajson_cnt_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Serializes data to xstring. Type of data is specified in the
    "! implementing class.
    "!
    "! @parameter i_data | data to be serialized
    "! @parameter r_result | serialized data
    METHODS serialize
      IMPORTING i_data          TYPE data
      RETURNING VALUE(r_result) TYPE xstring
      RAISING   cx_static_check.

    "! Deserializes xstring into data. The type of data is specified in
    "! the implementing class
    "!
    "! @parameter i_content | xstring to be deserialized
    "! @parameter e_data | data of the xstring
    METHODS deserialize
      IMPORTING i_content TYPE xstring
      EXPORTING e_data    TYPE data
      RAISING   cx_static_check.

  PROTECTED SECTION.

  PRIVATE SECTION.


ENDCLASS.



CLASS ZCL_ABAPGIT_AJSON_CNT_HANDLER IMPLEMENTATION.


  METHOD deserialize.
    DATA l_json       TYPE string.
    DATA lobj_ajson   TYPE REF TO zcl_abapgit_ajson.
    DATA lobj_mapping TYPE REF TO zif_abapgit_ajson_mapping.

    CLEAR e_data.

    l_json = zcl_abapgit_convert=>xstring_to_string_utf8( i_content ).
    lobj_mapping = zcl_abapgit_ajson_mapping=>create_camel_case( ).

    lobj_ajson = zcl_abapgit_ajson=>parse( iv_json = l_json
                                         ii_custom_mapping = lobj_mapping ).
    lobj_ajson->zif_abapgit_ajson~to_abap( IMPORTING ev_container = e_data ).

  ENDMETHOD.


  METHOD serialize.
    DATA lst_source   TYPE abap_trans_srcbind_tab.
    DATA lobj_mapping TYPE REF TO lcl_mapping.
    DATA l_json       TYPE string.
    DATA li_ajson     TYPE REF TO zcl_abapgit_ajson.

    FIELD-SYMBOLS: <st_source> LIKE LINE OF lst_source.

    APPEND INITIAL LINE TO lst_source ASSIGNING <st_source>.
    GET REFERENCE OF i_data INTO <st_source>-value.
    CREATE OBJECT lobj_mapping.

    li_ajson = zcl_abapgit_ajson=>create_empty( ii_custom_mapping = lobj_mapping ).
    li_ajson->keep_item_order( ).
    li_ajson->set(
      iv_path = '/'
      iv_val  = i_data ).

    l_json = li_ajson->stringify( 2 ).

    r_result = zcl_abapgit_convert=>string_to_xstring_utf8( iv_string = l_json ).

  ENDMETHOD.
ENDCLASS.
