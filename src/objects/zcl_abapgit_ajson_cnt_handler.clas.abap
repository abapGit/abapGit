CLASS zcl_abapgit_ajson_cnt_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Serializes data to xstring. Type of data is specified in the
    "! implementing class.
    "!
    "! @parameter data | data to be serialized
    "! @parameter result | serialized data
    METHODS serialize
      IMPORTING data          TYPE data
      RETURNING VALUE(result) TYPE xstring
      RAISING   cx_root.

    "! Deserializes xstring into data. The type of data is specified in
    "! the implementing class
    "!
    "! @parameter content | xstring to be deserialized
    "! @parameter data | data of the xstring
    METHODS deserialize
      IMPORTING content TYPE xstring
      EXPORTING data    TYPE data
      RAISING   cx_root.

  PROTECTED SECTION.

  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_abapgit_ajson_cnt_handler IMPLEMENTATION.


  METHOD deserialize.

    CLEAR data.

    DATA(json) = zcl_abapgit_convert=>xstring_to_string_utf8( content ).
    DATA(lo_ajson) = zcl_abapgit_ajson=>parse( iv_json = json ii_custom_mapping = zcl_abapgit_ajson_mapping=>create_camel_case( ) ).
    lo_ajson->zif_abapgit_ajson~to_abap( IMPORTING ev_container = data ).

  ENDMETHOD.


  METHOD serialize.
    DATA st_source TYPE abap_trans_srcbind_tab.
    FIELD-SYMBOLS: <st_source> LIKE LINE OF st_source.

    APPEND INITIAL LINE TO st_source ASSIGNING <st_source>.
    GET REFERENCE OF data INTO <st_source>-value.

    DATA(li_ajson) = zcl_abapgit_ajson=>create_empty( ii_custom_mapping = NEW lcl_mapping( ) ).
    li_ajson->keep_item_order( ).
    li_ajson->set(
      iv_path = '/'
      iv_val  = data ).

    DATA(json) = li_ajson->stringify( 2 ).

    result = zcl_abapgit_convert=>string_to_xstring_utf8( iv_string = json ).

  ENDMETHOD.
ENDCLASS.
