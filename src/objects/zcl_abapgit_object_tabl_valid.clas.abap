CLASS zcl_abapgit_object_tabl_valid DEFINITION PUBLIC FINAL.
  PUBLIC SECTION.
    METHODS validate
      IMPORTING
        io_remote_version TYPE REF TO zcl_abapgit_xml_input
        io_local_version  TYPE REF TO zcl_abapgit_xml_input
      RETURNING
        VALUE(rv_message) TYPE string
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
    TYPES:
      tty_founds  TYPE STANDARD TABLE OF rsfindlst
                       WITH NON-UNIQUE DEFAULT KEY,
      tty_seu_obj TYPE STANDARD TABLE OF seu_obj
                       WITH NON-UNIQUE DEFAULT KEY.

    METHODS:
      get_where_used_recursive
        IMPORTING
          iv_object_name       TYPE csequence
          iv_depth             TYPE i
          iv_object_type       TYPE euobj-id
          it_scope             TYPE tty_seu_obj
        RETURNING
          VALUE(rt_founds_all) TYPE tty_founds
        RAISING
          zcx_abapgit_exception,

      is_structure_used_in_db_table
        IMPORTING
          iv_object_name                        TYPE dd02v-tabname
        RETURNING
          VALUE(rv_is_structure_used_in_db_tab) TYPE abap_bool
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_object_tabl_valid IMPLEMENTATION.


  METHOD get_where_used_recursive.

    DATA: lt_findstrings TYPE stringtab,
          lt_founds      TYPE STANDARD TABLE OF rsfindlst,
          lt_scope       TYPE tty_seu_obj,
          lv_findstring  LIKE LINE OF lt_findstrings.

    FIELD-SYMBOLS: <ls_found> TYPE rsfindlst.

    lt_scope = it_scope.

    lv_findstring = iv_object_name.
    INSERT lv_findstring INTO TABLE lt_findstrings.

    DO iv_depth TIMES.

      CLEAR: lt_founds.

      CALL FUNCTION 'RS_EU_CROSSREF'
        EXPORTING
          i_find_obj_cls           = iv_object_type
          no_dialog                = 'X'
        TABLES
          i_findstrings            = lt_findstrings
          o_founds                 = lt_founds
          i_scope_object_cls       = lt_scope
        EXCEPTIONS
          not_executed             = 1
          not_found                = 2
          illegal_object           = 3
          no_cross_for_this_object = 4
          batch                    = 5
          batchjob_error           = 6
          wrong_type               = 7
          object_not_exist         = 8
          OTHERS                   = 9.

      IF sy-subrc = 1
      OR sy-subrc = 2
      OR lines( lt_founds ) = 0.
        EXIT.
      ELSEIF sy-subrc > 2.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      INSERT LINES OF lt_founds INTO TABLE rt_founds_all.

      CLEAR: lt_findstrings.

      LOOP AT lt_founds ASSIGNING <ls_found>.

        lv_findstring = <ls_found>-object.
        INSERT lv_findstring INTO TABLE lt_findstrings.

      ENDLOOP.

    ENDDO.

  ENDMETHOD.


  METHOD validate.

    DATA: lt_previous_table_fields TYPE TABLE OF dd03p,
          ls_previous_table_field  LIKE LINE OF lt_previous_table_fields,
          lt_current_table_fields  TYPE TABLE OF dd03p,
          ls_current_table_field   LIKE LINE OF lt_current_table_fields,
          ls_dd02v                 TYPE dd02v.

    io_remote_version->read(
      EXPORTING
        iv_name = 'DD02V'
      CHANGING
        cg_data = ls_dd02v ).

    " We only want to compare transparent tables, or structures used in transparent tables
    IF ls_dd02v-tabclass <> 'TRANSP' AND is_structure_used_in_db_table( ls_dd02v-tabname ) = abap_false.
      RETURN.
    ENDIF.

    io_remote_version->read(
      EXPORTING
        iv_name       = 'DD03P_TABLE'
      CHANGING
        cg_data       = lt_previous_table_fields ).

    io_local_version->read(
      EXPORTING
        iv_name       = 'DD03P_TABLE'
      CHANGING
        cg_data       = lt_current_table_fields ).

    LOOP AT lt_previous_table_fields INTO ls_previous_table_field.
      READ TABLE lt_current_table_fields WITH KEY fieldname = ls_previous_table_field-fieldname
        INTO ls_current_table_field.
      IF sy-subrc = 0.
        IF ls_current_table_field-rollname <> ls_previous_table_field-rollname.
          rv_message = 'Fields were changed. This may lead to inconsistencies.'.
        ENDIF.
      ELSE.
        rv_message = 'Fields were changed. This may lead to inconsistencies.'.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD is_structure_used_in_db_table.

    DATA: lt_scope  TYPE tty_seu_obj,
          lt_founds TYPE tty_founds.

    APPEND 'TABL' TO lt_scope.
    APPEND 'STRU' TO lt_scope.

    lt_founds = get_where_used_recursive( iv_object_name = iv_object_name
                                          iv_object_type = 'STRU'
                                          it_scope       = lt_scope
                                          iv_depth       = 5 ).

    DELETE lt_founds WHERE object_cls <> 'DT'.

    rv_is_structure_used_in_db_tab = boolc( lines( lt_founds ) > 0 ).

  ENDMETHOD.

ENDCLASS.
