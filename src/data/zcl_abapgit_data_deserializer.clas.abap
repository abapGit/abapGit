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
        !is_file TYPE zif_abapgit_git_definitions=>ty_file
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
        VALUE(rr_data) TYPE REF TO data
      RAISING
        zcx_abapgit_exception .

    METHODS is_table_allowed_to_edit
      IMPORTING
                !table_name               TYPE tabname
                !is_checks                TYPE zif_abapgit_definitions=>ty_deserialize_checks
      RETURNING VALUE(is_allowed_to_edit) TYPE abap_bool.
ENDCLASS.



CLASS ZCL_ABAPGIT_DATA_DESERIALIZER IMPLEMENTATION.


  METHOD convert_json_to_itab.

    DATA lo_ajson TYPE REF TO zcl_abapgit_ajson.
    DATA lx_ajson TYPE REF TO zcx_abapgit_ajson_error.

    FIELD-SYMBOLS <lg_tab> TYPE ANY TABLE.

    ASSIGN ir_data->* TO <lg_tab>.

    TRY.
        lo_ajson = zcl_abapgit_ajson=>parse( zcl_abapgit_convert=>xstring_to_string_utf8( is_file-data ) ).
        lo_ajson->zif_abapgit_ajson~to_abap( IMPORTING ev_container = <lg_tab> ).
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
    rs_result-updates = zcl_abapgit_data_utils=>build_table_itab( iv_name ).
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

    DATA result         TYPE zif_abapgit_data_deserializer=>ty_results.
    DATA tables         TYPE tredt_objects.
    DATA table_keys     TYPE STANDARD TABLE OF e071k.
    DATA db_table_name  TYPE tabname.

    LOOP AT ii_config->get_configs( ) REFERENCE INTO DATA(config).
      db_table_name = config->name.

      CHECK is_table_allowed_to_edit( table_name = db_table_name is_checks = is_checks ).

      DATA(lr_data) = zcl_abapgit_data_utils=>build_table_itab( config->name ).

      READ TABLE it_files REFERENCE INTO DATA(ls_file)
        WITH KEY file_path
        COMPONENTS path     = zif_abapgit_data_config=>c_default_path
                   filename = zcl_abapgit_data_utils=>build_filename( config->* ).
      CHECK sy-subrc = 0.

      convert_json_to_itab(
        ir_data = lr_data
        is_file = ls_file->* ).

      DATA(table_changes) = preview_database_changes(
        iv_name  = config->name
        it_where = config->where
        ir_data  = lr_data ).

      cl_table_utilities_brf=>create_transport_entries(
        EXPORTING
          it_table_ins = table_changes-inserts->*
          it_table_upd = table_changes-updates->*
          it_table_del = table_changes-deletes->*
          iv_tabname   = db_table_name
        CHANGING
          ct_e071      = tables
          ct_e071k     = table_keys ).

      write_database_table(
        iv_name = table_changes-table
        ir_del  = table_changes-deletes
        ir_ins  = table_changes-inserts ).

    ENDLOOP.

    DATA tadir_entries TYPE scts_tadir.
    cl_table_utilities_brf=>write_transport_entries(
      CHANGING
        ct_e071  = tables
        ct_e071k = table_keys
        ct_tadir = tadir_entries ).

  ENDMETHOD.


  METHOD is_table_allowed_to_edit.

    "Did the user flagged this table for update?
    CHECK line_exists( is_checks-overwrite[ obj_name = table_name decision = zif_abapgit_definitions=>c_yes ] ).

    "For safety reasons only customer dependend customizing tables are allowed to update
    SELECT SINGLE @abap_true
      FROM dd09l
      INTO @is_allowed_to_edit
      WHERE tabname = @table_name
        AND tabart = 'APPL2'
        AND as4user <> 'SAP'
        AND as4local = @cl_wer_const=>c_db_table_version_active.

  ENDMETHOD.
ENDCLASS.
