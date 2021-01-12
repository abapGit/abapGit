CLASS zcl_abapgit_data_serializer DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_data_factory .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_data_serializer .
  PROTECTED SECTION.

    METHODS dump_itab
      IMPORTING
        !ir_data       TYPE REF TO data
      RETURNING
        VALUE(rv_data) TYPE xstring
      RAISING
        zcx_abapgit_exception .
    METHODS read_database_table
      IMPORTING
        !iv_name       TYPE tadir-obj_name
        !it_where      TYPE string_table
      RETURNING
        VALUE(rr_data) TYPE REF TO data .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_DATA_SERIALIZER IMPLEMENTATION.


  METHOD dump_itab.

    DATA lo_ajson TYPE REF TO zcl_abapgit_ajson.
    DATA lv_string TYPE string.
    DATA lx_ajson TYPE REF TO zcx_abapgit_ajson_error.
    FIELD-SYMBOLS <lg_tab> TYPE ANY TABLE.


    ASSIGN ir_data->* TO <lg_tab>.

    TRY.
        lo_ajson = zcl_abapgit_ajson=>create_empty( ).
        lo_ajson->keep_item_order( ).
        lo_ajson->set(
          iv_path = '/'
          iv_val = <lg_tab> ).
        lv_string = lo_ajson->stringify( 2 ).
      CATCH zcx_abapgit_ajson_error INTO lx_ajson.
        zcx_abapgit_exception=>raise( lx_ajson->get_text( ) ).
    ENDTRY.

    rv_data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.


  METHOD read_database_table.

    DATA lv_where LIKE LINE OF it_where.
    FIELD-SYMBOLS: <lg_tab> TYPE ANY TABLE.

    rr_data = zcl_abapgit_data_utils=>build_table_itab( iv_name ).
    ASSIGN rr_data->* TO <lg_tab>.

    LOOP AT it_where INTO lv_where.
      SELECT * FROM (iv_name) APPENDING TABLE <lg_tab> WHERE (lv_where).
    ENDLOOP.
    IF lines( it_where ) = 0.
      SELECT * FROM (iv_name) INTO TABLE <lg_tab>.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_data_serializer~serialize.

    DATA lt_configs TYPE zif_abapgit_data_config=>ty_config_tt.
    DATA ls_config LIKE LINE OF lt_configs.
    DATA ls_file LIKE LINE OF rt_files.
    DATA lr_data TYPE REF TO data.


    ls_file-path = zif_abapgit_data_config=>c_default_path.
    lt_configs = ii_config->get_configs( ).

    LOOP AT lt_configs INTO ls_config.
      ASSERT ls_config-type = zif_abapgit_data_config=>c_data_type-tabu. " todo
      ASSERT NOT ls_config-name IS INITIAL.

      lr_data = read_database_table(
        iv_name  = ls_config-name
        it_where = ls_config-where ).

      ls_file-filename = zcl_abapgit_data_utils=>build_filename( ls_config ).
      ls_file-data = dump_itab( lr_data ).
      ls_file-sha1 = zcl_abapgit_hash=>sha1_blob( ls_file-data ).
      APPEND ls_file TO rt_files.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
