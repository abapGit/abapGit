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
    METHODS determine_transport_request
      IMPORTING
        io_repo                     TYPE REF TO zcl_abapgit_repo
        iv_transport_type           TYPE zif_abapgit_definitions=>ty_transport_type
      RETURNING
        VALUE(rv_transport_request) TYPE trkorr.
    METHODS is_table_allowed_to_edit
      IMPORTING
        !is_result                TYPE zif_abapgit_data_deserializer=>ty_result
      RETURNING
        VALUE(rv_allowed_to_edit) TYPE abap_bool .

ENDCLASS.



CLASS zcl_abapgit_data_deserializer IMPLEMENTATION.


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


  METHOD determine_transport_request.

    DATA li_exit TYPE REF TO zif_abapgit_exit.

    li_exit = zcl_abapgit_exit=>get_instance( ).

    " Use transport from repo settings if maintained, or determine via user exit.
    " If transport keeps empty here, it'll requested later via popup.
    rv_transport_request = io_repo->get_local_settings( )-customizing_request.

    li_exit->determine_transport_request(
      EXPORTING
        io_repo              = io_repo
        iv_transport_type    = iv_transport_type
      CHANGING
        cv_transport_request = rv_transport_request ).

  ENDMETHOD.


  METHOD is_table_allowed_to_edit.

    " Is the object supported (by default or based on exit)?
    rv_allowed_to_edit = zcl_abapgit_data_factory=>get_supporter( )->is_object_supported(
      iv_type = is_result-type
      iv_name = is_result-name ).

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

    rs_result-type = zif_abapgit_data_config=>c_data_type-tabu.
    rs_result-name = iv_name.
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

    IF zcl_abapgit_data_utils=>does_table_exist( iv_name ) = abap_false.
      zcx_abapgit_exception=>raise( |Table { iv_name } not found for data deserialization| ).
    ENDIF.

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

    DATA ls_result  LIKE LINE OF it_result.
    DATA li_cts_api TYPE REF TO zif_abapgit_cts_api.
    DATA ls_file    LIKE LINE OF rt_accessed_files.

    FIELD-SYMBOLS:
      <lt_ins> TYPE ANY TABLE,
      <lt_del> TYPE ANY TABLE,
      <lt_upd> TYPE ANY TABLE.

    LOOP AT it_result INTO ls_result.
      ASSERT ls_result-type = zif_abapgit_data_config=>c_data_type-tabu. " todo
      ASSERT ls_result-name IS NOT INITIAL.

      " Did the user flagged this object for update?
      READ TABLE is_checks-overwrite TRANSPORTING NO FIELDS
        WITH KEY object_type_and_name
        COMPONENTS
          obj_type = ls_result-type
          obj_name = ls_result-name
          decision = zif_abapgit_definitions=>c_yes.
      IF sy-subrc <>  0.
        RETURN.
      ENDIF.

      IF is_table_allowed_to_edit( ls_result ) = abap_false.
        zcx_abapgit_exception=>raise( |Table { ls_result-name } not supported for updating data| ).
      ENDIF.

      write_database_table(
        iv_name = ls_result-name
        ir_del  = ls_result-deletes
        ir_ins  = ls_result-inserts ).

      ASSIGN ls_result-inserts->* TO <lt_ins>.
      ASSIGN ls_result-deletes->* TO <lt_del>.
      ASSIGN ls_result-updates->* TO <lt_upd>. " not used

      IF zcl_abapgit_data_utils=>is_customizing_table( ls_result-name ) = abap_true.
        IF li_cts_api IS INITIAL.
          li_cts_api = zcl_abapgit_factory=>get_cts_api( ).
        ENDIF.

        li_cts_api->create_transport_entries(
          iv_transport = is_checks-customizing-transport
          it_table_ins = <lt_ins>
          it_table_upd = <lt_upd>
          it_table_del = <lt_del>
          iv_tabname   = |{ ls_result-name }| ).
      ENDIF.

      INSERT ls_result-file INTO TABLE rt_accessed_files. " data file
      INSERT ls_result-config INTO TABLE rt_accessed_files. " config file
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
      ASSERT ls_config-type = zif_abapgit_data_config=>c_data_type-tabu. " todo
      ASSERT ls_config-name IS NOT INITIAL.

      lr_data = zcl_abapgit_data_utils=>build_table_itab( ls_config-name ).

      READ TABLE it_files INTO ls_file
        WITH KEY file_path
        COMPONENTS path     = zif_abapgit_data_config=>c_default_path
                   filename = zcl_abapgit_data_utils=>build_data_filename( ls_config ).
      IF sy-subrc = 0.
        convert_json_to_itab(
          ir_data = lr_data
          is_file = ls_file ).

        ls_result = preview_database_changes(
          iv_name  = ls_config-name
          it_where = ls_config-where
          ir_data  = lr_data ).

        MOVE-CORRESPONDING ls_file TO ls_result-file. " data file

        READ TABLE it_files INTO ls_file
          WITH KEY file_path
          COMPONENTS path     = zif_abapgit_data_config=>c_default_path
                     filename = zcl_abapgit_data_utils=>build_config_filename( ls_config ).
        ASSERT sy-subrc = 0.

        MOVE-CORRESPONDING ls_file TO ls_result-config. " config file

        INSERT ls_result INTO TABLE rt_result.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_data_deserializer~deserialize_check.

    DATA lt_configs TYPE zif_abapgit_data_config=>ty_config_tt.

    lt_configs = ii_config->get_configs( ).

    IF lt_configs IS NOT INITIAL.
      rs_checks-required     = abap_true.
      rs_checks-type-request = zif_abapgit_cts_api=>c_transport_type-cust_request.
      rs_checks-type-task    = zif_abapgit_cts_api=>c_transport_type-cust_task.
      rs_checks-transport    = determine_transport_request(
                                 io_repo           = io_repo
                                 iv_transport_type = rs_checks-type ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
