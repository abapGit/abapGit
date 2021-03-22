CLASS zcl_abapgit_data_deserializer DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_data_factory .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_data_deserializer .

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS convert_json_to_itab
      IMPORTING
        !is_file TYPE zif_abapgit_definitions=>ty_file
        !ir_data TYPE REF TO data
      RAISING
        zcx_abapgit_exception .
    METHODS preview_database_changes
      IMPORTING
        !iv_name         TYPE tadir-obj_name
        !it_where        TYPE string_table
        !ir_data         TYPE REF TO data
      RETURNING
        VALUE(rs_result) TYPE zif_abapgit_data_deserializer=>ty_result
      RAISING
        zcx_abapgit_exception .
    METHODS write_database_table
      IMPORTING
        !iv_name TYPE tadir-obj_name
        !ir_del  TYPE REF TO data
        !ir_ins  TYPE REF TO data
      RAISING
        zcx_abapgit_exception .
    METHODS read_database_table
      IMPORTING
        !iv_name       TYPE tadir-obj_name
        !it_where      TYPE string_table
      RETURNING
        VALUE(rr_data) TYPE REF TO data .

ENDCLASS.



CLASS zcl_abapgit_data_deserializer IMPLEMENTATION.


  METHOD convert_json_to_itab.

    DATA lo_ajson TYPE REF TO zcl_abapgit_ajson.
    DATA lx_ajson TYPE REF TO zcx_abapgit_ajson_error.

    FIELD-SYMBOLS <lg_tab> TYPE ANY TABLE.

    ASSIGN ir_data->* TO <lg_tab>.

    TRY.
        lo_ajson = zcl_abapgit_ajson=>parse( zcl_abapgit_convert=>xstring_to_string_utf8( is_file-data ) ).
        lo_ajson->zif_abapgit_ajson_reader~to_abap( IMPORTING ev_container = <lg_tab> ).
      CATCH zcx_abapgit_ajson_error INTO lx_ajson.
        zcx_abapgit_exception=>raise( lx_ajson->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD preview_database_changes.

* method currently distinguishes between records be deleted and inserted (comparison of complete record)
* to-do: compare records based on database key of table to determine updates to existing records

    DATA lr_data TYPE REF TO data.

    FIELD-SYMBOLS <lg_old> TYPE ANY TABLE.
    FIELD-SYMBOLS <lg_new> TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_del> TYPE any.
    FIELD-SYMBOLS <ls_ins> TYPE any.
    FIELD-SYMBOLS <lg_del> TYPE ANY TABLE.
    FIELD-SYMBOLS <lg_ins> TYPE ANY TABLE.

    lr_data = read_database_table(
      iv_name  = iv_name
      it_where = it_where ).

    ASSIGN lr_data->* TO <lg_old>.
    ASSIGN ir_data->* TO <lg_new>.

    rs_result-table = iv_name.
    rs_result-deletes = zcl_abapgit_data_utils=>build_table_itab( iv_name ).
    rs_result-inserts = zcl_abapgit_data_utils=>build_table_itab( iv_name ).
    ASSIGN rs_result-deletes->* TO <lg_del>.
    ASSIGN rs_result-inserts->* TO <lg_ins>.

    <lg_del> = <lg_old>.
    <lg_ins> = <lg_new>.

    " Remove identical records
    LOOP AT <lg_del> ASSIGNING <ls_del>.
      READ TABLE <lg_ins> ASSIGNING <ls_ins> FROM <ls_del>.
      IF sy-subrc = 0.
        DELETE TABLE <lg_del> FROM <ls_del>.
        DELETE TABLE <lg_ins> FROM <ls_ins>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_database_table.

    DATA lv_where LIKE LINE OF it_where.

    FIELD-SYMBOLS <lg_tab> TYPE ANY TABLE.

    rr_data = zcl_abapgit_data_utils=>build_table_itab( iv_name ).
    ASSIGN rr_data->* TO <lg_tab>.

    LOOP AT it_where INTO lv_where.
      SELECT * FROM (iv_name) APPENDING TABLE <lg_tab> WHERE (lv_where).
    ENDLOOP.
    IF lines( it_where ) = 0.
      SELECT * FROM (iv_name) INTO TABLE <lg_tab>.
    ENDIF.

  ENDMETHOD.


  METHOD write_database_table.

    FIELD-SYMBOLS <lg_del> TYPE ANY TABLE.
    FIELD-SYMBOLS <lg_ins> TYPE ANY TABLE.

    ASSIGN ir_del->* TO <lg_del>.
    ASSIGN ir_ins->* TO <lg_ins>.

    IF lines( <lg_del> ) > 0.
      DELETE (iv_name) FROM TABLE <lg_del>.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Error deleting { lines( <lg_del> ) } records from table { iv_name }| ).
      ENDIF.
    ENDIF.

    IF lines( <lg_ins> ) > 0.
      INSERT (iv_name) FROM TABLE <lg_ins>.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Error inserting { lines( <lg_ins> ) } records into table { iv_name }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_data_deserializer~actualize.

* this method updates the database

    DATA ls_result LIKE LINE OF it_result.

    LOOP AT it_result INTO ls_result.
      write_database_table(
        iv_name = ls_result-table
        ir_del  = ls_result-deletes
        ir_ins  = ls_result-inserts ).
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_data_deserializer~deserialize.

* this method does not persist any changes to the database

    DATA lt_configs TYPE zif_abapgit_data_config=>ty_config_tt.
    DATA ls_config LIKE LINE OF lt_configs.
    DATA lr_data  TYPE REF TO data.
    DATA ls_file LIKE LINE OF it_files.
    DATA ls_result LIKE LINE OF rt_result.

    lt_configs = ii_config->get_configs( ).

    LOOP AT lt_configs INTO ls_config.

      lr_data = zcl_abapgit_data_utils=>build_table_itab( ls_config-name ).

      READ TABLE it_files INTO ls_file WITH KEY
        path = zif_abapgit_data_config=>c_default_path
        filename = zcl_abapgit_data_utils=>build_filename( ls_config ).
      IF sy-subrc = 0.
        convert_json_to_itab(
          ir_data = lr_data
          is_file = ls_file ).

        ls_result = preview_database_changes(
          iv_name  = ls_config-name
          it_where = ls_config-where
          ir_data  = lr_data ).

        INSERT ls_result INTO TABLE rt_result.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
