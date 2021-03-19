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
        !iv_name  TYPE tadir-obj_name
        !it_where TYPE string_table
        !ir_data  TYPE REF TO data
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

    DATA:
      lo_ajson TYPE REF TO zcl_abapgit_ajson,
      lx_ajson TYPE REF TO zcx_abapgit_ajson_error.

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

    DATA:
      lr_data TYPE REF TO data.

    FIELD-SYMBOLS:
      <lg_old> TYPE ANY TABLE,
      <lg_new> TYPE ANY TABLE,
      <ls_del> TYPE any,
      <ls_ins> TYPE any,
      <lg_del> TYPE ANY TABLE,
      <lg_ins> TYPE ANY TABLE.

    lr_data = read_database_table(
      iv_name  = iv_name
      it_where = it_where ).

    ASSIGN lr_data->* TO <lg_old>.
    ASSIGN ir_data->* TO <lg_new>.

    rs_result-table = iv_name.
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

    DATA:
      lv_where   LIKE LINE OF it_where,
      lv_tabname TYPE tabname,
      lv_subrc   TYPE sy-subrc.

    FIELD-SYMBOLS <lg_tab> TYPE ANY TABLE.

    lv_tabname = iv_name.

    ASSIGN ir_data->* TO <lg_tab>.

    LOOP AT it_where INTO lv_where.
      DELETE FROM (lv_tabname) WHERE (lv_where) ##SUBRC_OK.
    ENDLOOP.
    IF lines( it_where ) = 0.
      CALL FUNCTION 'DB_TRUNCATE_TABLE'
        EXPORTING
          tabname = lv_tabname
        IMPORTING
          subrc   = lv_subrc.
      IF lv_subrc <> 0.
        zcx_abapgit_exception=>raise( |Error truncating table { lv_tabname }| ).
      ENDIF.
    ENDIF.

    IF lines( <lg_tab> ) > 0.
      INSERT (lv_tabname) FROM TABLE <lg_tab>.
      IF sy-subrc = 0.
        zcx_abapgit_exception=>raise( |Error inserting { lines( <lg_tab> ) } records into table { lv_tabname }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_data_deserializer~deserialize.

    DATA:
      lt_configs TYPE zif_abapgit_data_config=>ty_config_tt,
      ls_config  LIKE LINE OF lt_configs,
      lr_data    TYPE REF TO data,
      ls_file    LIKE LINE OF it_files.

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

        IF iv_persist = abap_true.
          write_database_table(
            iv_name  = ls_config-name
            it_where = ls_config-where
            ir_data  = lr_data ).
        ELSE.
          rs_result = preview_database_changes(
            iv_name  = ls_config-name
            it_where = ls_config-where
            ir_data  = lr_data ).
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
