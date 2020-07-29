CLASS test_objects_files DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: gt_files TYPE zif_abapgit_definitions=>ty_files_tt.

    METHODS setup.

    METHODS get_program_data
      IMPORTING
        iv_line_break TYPE clike
      RETURNING VALUE(rv_result) TYPE xstring.

    METHODS get_expected_abap_source
      RETURNING VALUE(rt_result) TYPE abaptxt255_tab.

    METHODS read_abap_with_crlf FOR TESTING
      RAISING
        cx_static_check.

    METHODS read_abap_with_lf FOR TESTING
      RAISING
        cx_static_check.

ENDCLASS.

CLASS test_objects_files IMPLEMENTATION.

  METHOD setup.
    FIELD-SYMBOLS: <ls_files> LIKE LINE OF gt_files.

    APPEND INITIAL LINE TO gt_files ASSIGNING <ls_files>.
    <ls_files>-filename = 'zcrlf.prog.abap'.
    <ls_files>-data = get_program_data( cl_abap_char_utilities=>cr_lf  ).
    APPEND INITIAL LINE TO gt_files ASSIGNING <ls_files>.
    <ls_files>-filename = 'zlf.prog.abap'.
    <ls_files>-data = get_program_data( zif_abapgit_definitions=>c_newline ).

  ENDMETHOD.

  METHOD get_program_data.
    DATA: char_source TYPE string.

    CONCATENATE 'My first program' iv_line_break 'has syntax errors' INTO char_source.
    rv_result = zcl_abapgit_convert=>string_to_xstring_utf8( char_source ).

  ENDMETHOD.

  METHOD get_expected_abap_source.

    APPEND 'My first program' TO rt_result.
    APPEND 'has syntax errors' TO rt_result.

  ENDMETHOD.

  METHOD read_abap_with_crlf.
    DATA: lt_exp_source TYPE abaptxt255_tab,
          lt_act_source TYPE abaptxt255_tab,
          ls_item TYPE zif_abapgit_definitions=>ty_item,
          lo_cut TYPE REF TO zcl_abapgit_objects_files.

    ls_item-obj_type = 'prog'.
    ls_item-obj_name = 'zcrlf'.
    CREATE OBJECT lo_cut
      EXPORTING
        is_item = ls_item.
    lo_cut->set_files( gt_files ).

    cl_abap_unit_assert=>assert_equals( exp = get_expected_abap_source( )
      act = lo_cut->read_abap( ) ).

  ENDMETHOD.

  METHOD read_abap_with_lf.
    DATA: lt_exp_source TYPE abaptxt255_tab,
          lt_act_source TYPE abaptxt255_tab,
          ls_item TYPE zif_abapgit_definitions=>ty_item,
          lo_cut TYPE REF TO zcl_abapgit_objects_files.

    ls_item-obj_type = 'prog'.
    ls_item-obj_name = 'zlf'.
    CREATE OBJECT lo_cut
      EXPORTING
        is_item = ls_item.
    lo_cut->set_files( gt_files ).

    cl_abap_unit_assert=>assert_equals( exp = get_expected_abap_source( )
      act = lo_cut->read_abap( ) ).

  ENDMETHOD.

ENDCLASS.
