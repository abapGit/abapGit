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
ENDCLASS.

CLASS zcl_abapgit_object_tabl_valid IMPLEMENTATION.

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

    IF ls_dd02v-tabclass <> 'TRANSP'.
      " We only want to compare transparent tables. Otherwise we
      " get false positives for structures
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

ENDCLASS.
