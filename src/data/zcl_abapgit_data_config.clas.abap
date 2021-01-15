CLASS zcl_abapgit_data_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_data_config .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_extension TYPE string VALUE '.config.json'.
    DATA mt_config TYPE zif_abapgit_data_config=>ty_config_tt .

    METHODS dump
      IMPORTING
        !is_config     TYPE zif_abapgit_data_config=>ty_config
      RETURNING
        VALUE(rv_json) TYPE string
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_DATA_CONFIG IMPLEMENTATION.


  METHOD dump.

    DATA lo_ajson TYPE REF TO zcl_abapgit_ajson.
    DATA lx_ajson TYPE REF TO zcx_abapgit_ajson_error.


    TRY.
        lo_ajson = zcl_abapgit_ajson=>create_empty( ).
        lo_ajson->zif_abapgit_ajson_writer~set(
          iv_path = '/'
          iv_val = is_config ).
        rv_json = zcl_abapgit_convert=>string_to_xstring_utf8( lo_ajson->stringify( 2 ) ).
      CATCH zcx_abapgit_ajson_error INTO lx_ajson.
        zcx_abapgit_exception=>raise( lx_ajson->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_data_config~add_config.

    ASSERT NOT is_config-type IS INITIAL.
    ASSERT NOT is_config-name IS INITIAL.
    ASSERT is_config-name = to_upper( is_config-name ).

    INSERT is_config INTO TABLE mt_config.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Already in table' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_data_config~from_json.

    DATA ls_file LIKE LINE OF it_files.
    DATA ls_config TYPE zif_abapgit_data_config=>ty_config.
    DATA lo_ajson TYPE REF TO zcl_abapgit_ajson.
    DATA lx_ajson TYPE REF TO zcx_abapgit_ajson_error.

    CLEAR mt_config.
    LOOP AT it_files INTO ls_file WHERE path = zif_abapgit_data_config=>c_default_path
        AND filename CP |*{ c_extension }|.
      TRY.
          lo_ajson = zcl_abapgit_ajson=>parse( zcl_abapgit_convert=>xstring_to_string_utf8( ls_file-data ) ).
          lo_ajson->zif_abapgit_ajson_reader~to_abap( IMPORTING ev_container = ls_config ).
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

    ASSERT NOT is_config-type IS INITIAL.
    ASSERT NOT is_config-name IS INITIAL.
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
      ls_file-filename = to_lower( |{ ls_config-name }{ c_extension }| ).
      ls_file-data = dump( ls_config ).
      ls_file-sha1 = zcl_abapgit_hash=>sha1_blob( ls_file-data ).
      APPEND ls_file TO rt_files.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_data_config~update_config.

    zif_abapgit_data_config~remove_config( is_config ).
    zif_abapgit_data_config~add_config( is_config ).

  ENDMETHOD.
ENDCLASS.
