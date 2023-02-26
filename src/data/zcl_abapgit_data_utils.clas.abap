CLASS zcl_abapgit_data_utils DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS build_table_itab
      IMPORTING
        !iv_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rr_data) TYPE REF TO data
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS build_filename
      IMPORTING
        !is_config         TYPE zif_abapgit_data_config=>ty_config
      RETURNING
        VALUE(rv_filename) TYPE string.
    CLASS-METHODS jump
      IMPORTING
        !is_item       TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_exit) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES ty_names TYPE STANDARD TABLE OF abap_compname WITH DEFAULT KEY .
    CLASS-METHODS list_key_fields
      IMPORTING
        !iv_name TYPE tadir-obj_name
      RETURNING
        VALUE(rt_names) TYPE ty_names .
ENDCLASS.



CLASS zcl_abapgit_data_utils IMPLEMENTATION.


  METHOD build_filename.

    rv_filename = to_lower( |{ is_config-name }.{ is_config-type }.{ zif_abapgit_data_config=>c_default_format }| ).

    REPLACE ALL OCCURRENCES OF '/' IN rv_filename WITH '#'.

  ENDMETHOD.

  METHOD list_key_fields.
    DATA lo_obj        TYPE REF TO object.
    DATA lv_tabname    TYPE c LENGTH 16.
    DATA lr_ddfields   TYPE REF TO data.
    DATA lv_workaround TYPE c LENGTH 20.
    DATA lr_struct     TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS <lg_any> TYPE any.
    FIELD-SYMBOLS <lv_field> TYPE simple.
    FIELD-SYMBOLS <lt_ddfields> TYPE ANY TABLE.

* convert to correct type,
    lv_tabname = iv_name.

    TRY.
        CALL METHOD ('XCO_CP_ABAP_DICTIONARY')=>database_table
          EXPORTING
            iv_name           = lv_tabname
          RECEIVING
            ro_database_table = lo_obj.
        ASSIGN lo_obj->('IF_XCO_DATABASE_TABLE~FIELDS->IF_XCO_DBT_FIELDS_FACTORY~KEY') TO <lg_any>.
        ASSERT sy-subrc = 0.
        lo_obj = <lg_any>.
        CALL METHOD lo_obj->('IF_XCO_DBT_FIELDS~GET_NAMES')
          RECEIVING
            rt_names = rt_names.
      CATCH cx_sy_dyn_call_illegal_class.
        lv_workaround = 'DDFIELDS'.
        CREATE DATA lr_ddfields TYPE (lv_workaround).
        ASSIGN lr_ddfields->* TO <lt_ddfields>.
        ASSERT sy-subrc = 0.
        lr_struct ?= cl_abap_typedescr=>describe_by_name( lv_tabname ).
        <lt_ddfields> = lr_struct->get_ddic_field_list( ).
        LOOP AT <lt_ddfields> ASSIGNING <lg_any>.
          ASSIGN COMPONENT 'KEYFLAG' OF STRUCTURE <lg_any> TO <lv_field>.
          IF sy-subrc <> 0 OR <lv_field> <> abap_true.
            CONTINUE.
          ENDIF.
          ASSIGN COMPONENT 'FIELDNAME' OF STRUCTURE <lg_any> TO <lv_field>.
          ASSERT sy-subrc = 0.
          APPEND <lv_field> TO rt_names.
        ENDLOOP.
    ENDTRY.

  ENDMETHOD.

  METHOD build_table_itab.

    DATA lo_type   TYPE REF TO cl_abap_typedescr.
    DATA lo_data   TYPE REF TO cl_abap_structdescr.
    DATA lo_table  TYPE REF TO cl_abap_tabledescr.
    DATA lt_fields TYPE ddfields.
    DATA lt_keys   TYPE abap_table_keydescr_tab.
    DATA lt_names  TYPE ty_names.

    FIELD-SYMBOLS <lv_name>      LIKE LINE OF lt_names.
    FIELD-SYMBOLS <ls_key>       LIKE LINE OF lt_keys.
    FIELD-SYMBOLS <ls_component> LIKE LINE OF <ls_key>-components.

    cl_abap_structdescr=>describe_by_name(
      EXPORTING
        p_name         = iv_name
      RECEIVING
        p_descr_ref    = lo_type
      EXCEPTIONS
        type_not_found = 1 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Table { iv_name } not found for data serialization| ).
    ENDIF.

    TRY.
        lo_data ?= lo_type.

        " Get primary key to ensure unique entries
        IF lo_data->is_ddic_type( ) = abap_true.
          lt_names = list_key_fields( iv_name ).

          APPEND INITIAL LINE TO lt_keys ASSIGNING <ls_key>.
          <ls_key>-access_kind = cl_abap_tabledescr=>tablekind_sorted.
          <ls_key>-key_kind    = cl_abap_tabledescr=>keydefkind_user.
          <ls_key>-is_primary  = abap_true.
          <ls_key>-is_unique   = abap_true.

          LOOP AT lt_names ASSIGNING <lv_name>.
            APPEND INITIAL LINE TO <ls_key>-components ASSIGNING <ls_component>.
            <ls_component>-name = <lv_name>.
          ENDLOOP.
        ENDIF.

        IF lines( lt_names ) = 0.
          lo_table = cl_abap_tabledescr=>get( lo_data ).
        ELSE.
          lo_table = cl_abap_tabledescr=>get_with_keys(
            p_line_type = lo_data
            p_keys      = lt_keys ).
        ENDIF.

        CREATE DATA rr_data TYPE HANDLE lo_table.

      CATCH cx_root.
        zcx_abapgit_exception=>raise( |Error creating internal table for data serialization| ).
    ENDTRY.

  ENDMETHOD.


  METHOD jump.

    " Run SE16 with authorization check
    CALL FUNCTION 'RS_TABLE_LIST_CREATE'
      EXPORTING
        table_name         = is_item-obj_name
      EXCEPTIONS
        table_is_structure = 1
        table_not_exists   = 2
        db_not_exists      = 3
        no_permission      = 4
        no_change_allowed  = 5
        table_is_gtt       = 6
        OTHERS             = 7.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Table { is_item-obj_name } cannot be displayed| ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
