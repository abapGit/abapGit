CLASS zcl_abapgit_data_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_data_config .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_config TYPE zif_abapgit_data_config=>ty_config_tt .

    METHODS dump
      IMPORTING
        !is_config     TYPE zif_abapgit_data_config=>ty_config
      RETURNING
        VALUE(rv_json) TYPE xstring
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_data_config IMPLEMENTATION.


  METHOD dump.

    DATA lo_ajson TYPE REF TO zcl_abapgit_ajson.
    DATA lx_ajson TYPE REF TO zcx_abapgit_ajson_error.

    TRY.
        lo_ajson = zcl_abapgit_ajson=>create_empty( ).
        lo_ajson->zif_abapgit_ajson~set(
          iv_path = '/'
          iv_val  = is_config ).
        rv_json = zcl_abapgit_convert=>string_to_xstring_utf8( lo_ajson->stringify( 2 ) ).
      CATCH zcx_abapgit_ajson_error INTO lx_ajson.
        zcx_abapgit_exception=>raise( lx_ajson->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_data_config~add_config.

    DATA lv_where TYPE string.

    FIELD-SYMBOLS <ls_config> LIKE LINE OF mt_config.

    ASSERT is_config-type IS NOT INITIAL.
    ASSERT is_config-name IS NOT INITIAL.
    ASSERT is_config-name = to_upper( is_config-name ).

    INSERT is_config INTO TABLE mt_config.
    IF sy-subrc <> 0.
* append to existing
      READ TABLE mt_config ASSIGNING <ls_config> WITH KEY type = is_config-type name = is_config-name.
      ASSERT sy-subrc = 0.
      LOOP AT is_config-where INTO lv_where.
        READ TABLE <ls_config>-where TRANSPORTING NO FIELDS WITH KEY table_line = lv_where.
        IF sy-subrc <> 0.
          INSERT lv_where INTO TABLE <ls_config>-where.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_data_config~from_json.

    DATA ls_file LIKE LINE OF it_files.
    DATA ls_config TYPE zif_abapgit_data_config=>ty_config.
    DATA lo_ajson TYPE REF TO zcl_abapgit_ajson.
    DATA lx_ajson TYPE REF TO zcx_abapgit_ajson_error.

    CLEAR mt_config.
    LOOP AT it_files INTO ls_file
        USING KEY file_path
        WHERE path = zif_abapgit_data_config=>c_default_path
        AND filename CP |*.{ zif_abapgit_data_config=>c_config }.{ zif_abapgit_data_config=>c_default_format }|.
      TRY.
          lo_ajson = zcl_abapgit_ajson=>parse( zcl_abapgit_convert=>xstring_to_string_utf8( ls_file-data ) ).
          lo_ajson->zif_abapgit_ajson~to_abap( IMPORTING ev_container = ls_config ).
        CATCH zcx_abapgit_ajson_error INTO lx_ajson.
          zcx_abapgit_exception=>raise( lx_ajson->get_text( ) ).
      ENDTRY.

      zif_abapgit_data_config~add_config( ls_config ).
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_data_config~get_configs.
    rt_configs = mt_config.
  ENDMETHOD.


  METHOD zif_abapgit_data_config~remove_config.

    ASSERT is_config-type IS NOT INITIAL.
    ASSERT is_config-name IS NOT INITIAL.
    ASSERT is_config-name = to_upper( is_config-name ).

    DELETE mt_config WHERE name = is_config-name AND type = is_config-type.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Not found' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_data_config~to_json.

    DATA ls_config LIKE LINE OF mt_config.
    DATA ls_file LIKE LINE OF rt_files.

    ls_file-path = zif_abapgit_data_config=>c_default_path.

    LOOP AT mt_config INTO ls_config.
      ls_file-data = dump( ls_config ).
      ls_file-sha1 = zcl_abapgit_hash=>sha1_blob( ls_file-data ).
      ls_config-type = zif_abapgit_data_config=>c_config.
      ls_file-filename = zcl_abapgit_data_utils=>build_data_filename( ls_config ).
      APPEND ls_file TO rt_files.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_data_config~update_config.

    zif_abapgit_data_config~remove_config( is_config ).
    zif_abapgit_data_config~add_config( is_config ).

  ENDMETHOD.
ENDCLASS.
