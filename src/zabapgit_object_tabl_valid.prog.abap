*&---------------------------------------------------------------------*
*&  Include  zabapgit_object_tabl_valid
*&---------------------------------------------------------------------*

CLASS lcl_object_tabl_validation DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS validate
      IMPORTING
        io_remote_version TYPE REF TO lcl_xml_input
        io_local_version  TYPE REF TO lcl_xml_input
      RETURNING
        VALUE(rv_message) TYPE string
      RAISING
        lcx_exception.
ENDCLASS.

CLASS lcl_tabl_validation_dialog DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_message TYPE string.
    INTERFACES: lif_object_comparison_result.
  PRIVATE SECTION.
    DATA mv_message TYPE string.
    DATA mv_halt TYPE string.

ENDCLASS.

CLASS lcl_object_tabl_validation IMPLEMENTATION.

  METHOD validate.
    DATA: lt_previous_table_fields TYPE TABLE OF dd03p,
          ls_previous_table_field  LIKE LINE OF lt_previous_table_fields,
          lt_current_table_fields  TYPE TABLE OF dd03p,
          ls_current_table_field   LIKE LINE OF lt_current_table_fields.
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

CLASS lcl_tabl_validation_dialog IMPLEMENTATION.
  METHOD constructor.
    mv_message = iv_message.
  ENDMETHOD.
  METHOD lif_object_comparison_result~is_result_complete_halt.
    rv_response = mv_halt.
  ENDMETHOD.

  METHOD lif_object_comparison_result~show_confirmation_dialog.
    DATA lv_answer TYPE string.
    TRY.
        lv_answer = lcl_popups=>popup_to_confirm(
          titlebar              = 'Warning'
          text_question         = mv_message
          text_button_1         = 'Abort'
          icon_button_1         = 'ICON_CANCEL'
          text_button_2         = 'Pull anyway'
          icon_button_2         = 'ICON_OKAY'
          default_button        = '2'
          display_cancel_button = abap_false ).
      CATCH lcx_exception.
        mv_halt = abap_true.
    ENDTRY.

    IF lv_answer = 1.
      mv_halt = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lct_table_validation DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
  PRIVATE SECTION.
    METHODS:
      setup,
      type_changed         FOR TESTING RAISING lcx_exception,
      no_type_changes      FOR TESTING RAISING lcx_exception,
      field_not_found      FOR TESTING RAISING lcx_exception,
      no_fields_no_message FOR TESTING RAISING lcx_exception,
      create_xmls
        RAISING
          lcx_exception.
    DATA: mo_table_validator            TYPE REF TO lcl_object_tabl_validation,
          mo_previous_version_out_xml   TYPE REF TO lcl_xml_output,
          mo_previous_version_input_xml TYPE REF TO lcl_xml_input,
          mo_current_version_out_xml    TYPE REF TO lcl_xml_output,
          mo_current_version_input_xml  TYPE REF TO lcl_xml_input,
          mt_previous_table_fields      TYPE TABLE OF dd03p,
          mt_current_table_fields       TYPE TABLE OF dd03p,
          mv_validation_message         TYPE string.

ENDCLASS.

CLASS lct_table_validation IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT mo_table_validator.
  ENDMETHOD.
  METHOD type_changed.
    DATA:
      ls_previous_table_field LIKE LINE OF mt_previous_table_fields,
      ls_current_table_field  LIKE LINE OF mt_current_table_fields.

    ls_previous_table_field-fieldname = 'FIELD1'.
    ls_previous_table_field-rollname  = 'INT4'.
    APPEND ls_previous_table_field TO mt_previous_table_fields.

    ls_current_table_field-fieldname = 'FIELD1'.
    ls_current_table_field-rollname  = 'CHAR30'.
    APPEND ls_current_table_field TO mt_current_table_fields.

    create_xmls( ).

    mv_validation_message = mo_table_validator->validate(
      io_remote_version = mo_previous_version_input_xml
      io_local_version  = mo_current_version_input_xml ).

    cl_abap_unit_assert=>assert_equals(
      act = mv_validation_message
      exp = 'Fields were changed. This may lead to inconsistencies.' ).
  ENDMETHOD.

  METHOD no_type_changes.
    DATA:
      ls_previous_table_field LIKE LINE OF mt_previous_table_fields.

    ls_previous_table_field-fieldname = 'FIELD1'.
    ls_previous_table_field-rollname  = 'INT4'.
    APPEND ls_previous_table_field TO mt_previous_table_fields.

    mt_current_table_fields = mt_previous_table_fields.

    create_xmls( ).

    mv_validation_message = mo_table_validator->validate(
      io_remote_version = mo_previous_version_input_xml
      io_local_version  = mo_current_version_input_xml ).

    cl_abap_unit_assert=>assert_equals(
      act = mv_validation_message
      exp = '' ).
  ENDMETHOD.

  METHOD field_not_found.
    DATA:
      ls_previous_table_field LIKE LINE OF mt_previous_table_fields,
      ls_current_table_field  LIKE LINE OF mt_current_table_fields.

    ls_previous_table_field-fieldname = 'FIELD1'.
    ls_previous_table_field-rollname  = 'INT4'.
    APPEND ls_previous_table_field TO mt_previous_table_fields.

    ls_current_table_field-fieldname = 'ANOTHER_FIELD'.
    ls_current_table_field-rollname  = 'CHAR30'.
    APPEND ls_current_table_field TO mt_current_table_fields.

    create_xmls( ).

    mv_validation_message = mo_table_validator->validate(
      io_remote_version = mo_previous_version_input_xml
      io_local_version  = mo_current_version_input_xml ).

    cl_abap_unit_assert=>assert_equals(
      act = mv_validation_message
      exp = 'Fields were changed. This may lead to inconsistencies.' ).
  ENDMETHOD.

  METHOD no_fields_no_message.
    create_xmls( ).

    mv_validation_message = mo_table_validator->validate(
      io_remote_version = mo_previous_version_input_xml
      io_local_version  = mo_current_version_input_xml ).

    cl_abap_unit_assert=>assert_initial( mv_validation_message ).
  ENDMETHOD.

  METHOD create_xmls.
    CREATE OBJECT mo_previous_version_out_xml.
    mo_previous_version_out_xml->add(
      iv_name       = 'DD03P_TABLE'
      ig_data       = mt_previous_table_fields ).

    CREATE OBJECT mo_previous_version_input_xml
      EXPORTING
        iv_xml = mo_previous_version_out_xml->render( ).

    CREATE OBJECT mo_current_version_out_xml.
    mo_current_version_out_xml->add(
      iv_name       = 'DD03P_TABLE'
      ig_data       = mt_current_table_fields ).

    CREATE OBJECT mo_current_version_input_xml
      EXPORTING
        iv_xml = mo_current_version_out_xml->render( ).
  ENDMETHOD.

ENDCLASS.
