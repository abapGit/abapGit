CLASS zcl_abapgit_properties_file DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_i18n_file.

    CONSTANTS:
      c_properties_feature TYPE string VALUE 'TRANSL'.

    METHODS constructor
      IMPORTING iv_lang TYPE laiso.

    METHODS push_text_pairs
      IMPORTING it_translation TYPE string_table.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_lang TYPE laiso.
    DATA mt_translation TYPE string_table.

ENDCLASS.



CLASS zcl_abapgit_properties_file IMPLEMENTATION.



  METHOD constructor.
    mv_lang = to_lower( iv_lang ).
  ENDMETHOD.


  METHOD push_text_pairs.
    mt_translation = it_translation.
  ENDMETHOD.


  METHOD zif_abapgit_i18n_file~ext.
    rv_ext = 'properties'.
  ENDMETHOD.


  METHOD zif_abapgit_i18n_file~lang.
    rv_lang = mv_lang.
  ENDMETHOD.


  METHOD zif_abapgit_i18n_file~render.
    DATA: lv_translation TYPE string,
          lo_buf      TYPE REF TO zcl_abapgit_string_buffer,
          lv_str      TYPE string.

    lv_translation = concat_lines_of( table = mt_translation
                                      sep   = cl_abap_char_utilities=>newline ).
    CREATE OBJECT lo_buf.
    lo_buf->add( lv_translation ).

    lv_str = lo_buf->join_w_newline_and_flush( ) && cl_abap_char_utilities=>newline.
    rv_data = zcl_abapgit_convert=>string_to_xstring_utf8_bom( lv_str ).

  ENDMETHOD.


  METHOD zif_abapgit_i18n_file~translate.
  ENDMETHOD.
ENDCLASS.
