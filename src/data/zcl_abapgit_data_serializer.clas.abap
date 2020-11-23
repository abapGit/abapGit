CLASS zcl_abapgit_data_serializer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_data_serializer .
  PROTECTED SECTION.

    METHODS dump_itab
      IMPORTING
        !ir_data       TYPE REF TO data
      RETURNING
        VALUE(rv_data) TYPE xstring .
    METHODS build_table_itab
      IMPORTING
        !iv_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rr_data) TYPE REF TO data .
    METHODS read_database_table
      IMPORTING
        !iv_name       TYPE tadir-obj_name
        !it_where      TYPE string_table
      RETURNING
        VALUE(rr_data) TYPE REF TO data .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_DATA_SERIALIZER IMPLEMENTATION.


  METHOD build_table_itab.

    DATA lo_structure TYPE REF TO cl_abap_structdescr.
    DATA lo_table TYPE REF TO cl_abap_tabledescr.

    lo_structure ?= cl_abap_structdescr=>describe_by_name( iv_name ).
* todo, also add unique key corresponding to the db table, so duplicates cannot be returned
    lo_table = cl_abap_tabledescr=>create( lo_structure ).
    CREATE DATA rr_data TYPE HANDLE lo_table.

  ENDMETHOD.


  METHOD dump_itab.

* quick and dirty, will be json instead

    DATA lt_data TYPE string_table.
    DATA lv_str TYPE string.
    FIELD-SYMBOLS <lg_tab> TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_row> TYPE any.


    ASSIGN ir_data->* TO <lg_tab>.
    LOOP AT <lg_tab> ASSIGNING <ls_row>.
      cl_abap_container_utilities=>fill_container_c(
        EXPORTING
          im_value     = <ls_row>
        IMPORTING
          ex_container = lv_str ).
      APPEND lv_str TO lt_data.
    ENDLOOP.

    rv_data = zcl_abapgit_convert=>string_to_xstring_utf8( concat_lines_of( table = lt_data
                                                                            sep   = |\n| ) ).

  ENDMETHOD.


  METHOD read_database_table.

    DATA lv_where LIKE LINE OF it_where.
    FIELD-SYMBOLS: <lg_tab> TYPE ANY TABLE.

    rr_data = build_table_itab( iv_name ).
    ASSIGN rr_data->* TO <lg_tab>.

    LOOP AT it_where INTO lv_where.
      SELECT * FROM (iv_name) INTO TABLE <lg_tab> WHERE (lv_where).
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


    ls_file-path = ii_config->get_path( ).
    lt_configs = ii_config->get_configs( ).

    LOOP AT lt_configs INTO ls_config.
      ASSERT ls_config-type = zif_abapgit_data_config=>c_data_type-tabu. " todo
      ASSERT NOT ls_config-name IS INITIAL.

      lr_data = read_database_table(
        iv_name  = ls_config-name
        it_where = ls_config-where ).

      ls_file-filename = to_lower( |{ ls_config-name }.{ ls_config-type }.todo| ).
      ls_file-data = dump_itab( lr_data ).
      ls_file-sha1 = zcl_abapgit_hash=>sha1_blob( ls_file-data ).
      APPEND ls_file TO rt_files.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
