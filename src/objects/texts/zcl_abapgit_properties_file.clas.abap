CLASS zcl_abapgit_properties_file DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_i18n_file.

    CONSTANTS:
      c_properties_feature TYPE string VALUE 'TRANSL'.

    METHODS constructor
      IMPORTING
        iv_lang TYPE laiso.

    METHODS parse
      IMPORTING
        iv_xdata TYPE xstring
      RAISING
        zcx_abapgit_exception.

    METHODS push_text_pairs
      IMPORTING it_translation TYPE string_table.

    METHODS get_translations
      EXPORTING
        ev_data TYPE data
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_lang TYPE laiso.
    DATA mt_translation TYPE string_table.

ENDCLASS.



CLASS zcl_abapgit_properties_file IMPLEMENTATION.


  METHOD constructor.
    mv_lang = to_lower( iv_lang ).
  ENDMETHOD.


  METHOD get_translations.

    DATA:
      lv_translation TYPE string,
      lo_ajson       TYPE REF TO zif_abapgit_ajson,
      lo_json_path   TYPE REF TO zcl_abapgit_json_path,
      lx_exception   TYPE REF TO cx_static_check.

    CREATE OBJECT lo_json_path.

    TRY.
        lv_translation = lo_json_path->deserialize( mt_translation ).

        lo_ajson = zcl_abapgit_ajson=>parse( lv_translation
          )->map( zcl_abapgit_ajson_mapping=>create_to_snake_case( ) ).

        lo_ajson->to_abap( IMPORTING ev_container = ev_data ).

      CATCH cx_static_check INTO lx_exception.
        zcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDTRY.

  ENDMETHOD.


  METHOD parse.

    DATA lv_data TYPE string.

    lv_data = zcl_abapgit_convert=>xstring_to_string_utf8( iv_xdata ).

    SPLIT lv_data AT cl_abap_char_utilities=>newline INTO TABLE mt_translation.

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

    DATA lv_translation TYPE string.

    lv_translation = concat_lines_of( table = mt_translation
                                      sep   = cl_abap_char_utilities=>newline ) && cl_abap_char_utilities=>newline.
    rv_data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_translation ).

  ENDMETHOD.


  METHOD zif_abapgit_i18n_file~translate.
  ENDMETHOD.
ENDCLASS.
