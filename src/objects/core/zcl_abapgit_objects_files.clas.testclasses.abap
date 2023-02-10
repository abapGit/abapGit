CLASS ltcl_objects_files DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO zcl_abapgit_objects_files.

    METHODS setup
      RAISING
        zcx_abapgit_exception .

    METHODS get_program_data
      IMPORTING
        iv_line_break    TYPE clike
      RETURNING
        VALUE(rv_result) TYPE xstring
      RAISING
        zcx_abapgit_exception .

    METHODS get_xml_data
      RETURNING
        VALUE(rv_result) TYPE xstring
      RAISING
        zcx_abapgit_exception .

    METHODS get_expected_abap_source
      RETURNING
        VALUE(rt_result) TYPE abaptxt255_tab.

    METHODS read_abap FOR TESTING
      RAISING
        cx_static_check.

    METHODS read_xml FOR TESTING
      RAISING
        cx_static_check.

    METHODS get_file_pattern FOR TESTING.

    METHODS is_json_metadata FOR TESTING.
    METHODS is_not_json_metadata FOR TESTING.
ENDCLASS.

CLASS ltcl_objects_files IMPLEMENTATION.

  METHOD setup.
    DATA: lt_files TYPE zif_abapgit_git_definitions=>ty_files_tt,
          ls_item  TYPE zif_abapgit_definitions=>ty_item.
    FIELD-SYMBOLS: <ls_files> LIKE LINE OF lt_files.

    " filenames are lower case
    APPEND INITIAL LINE TO lt_files ASSIGNING <ls_files>.
    <ls_files>-filename = 'zlf.prog.abap'.
    <ls_files>-data = get_program_data( cl_abap_char_utilities=>newline ).
    APPEND INITIAL LINE TO lt_files ASSIGNING <ls_files>.
    <ls_files>-filename = 'zlf.prog.xml'.
    <ls_files>-data = get_xml_data( ).

    " object type and name are upper case
    ls_item-obj_type = 'PROG'.
    ls_item-obj_name = 'ZLF'.
    CREATE OBJECT mo_cut
      EXPORTING
        is_item = ls_item.
    mo_cut->set_files( lt_files ).

  ENDMETHOD.

  METHOD get_program_data.
    DATA: lv_abap_source TYPE string.

    CONCATENATE 'My first program' iv_line_break 'has syntax errors' INTO lv_abap_source.
    rv_result = zcl_abapgit_convert=>string_to_xstring_utf8( lv_abap_source ).

  ENDMETHOD.

  METHOD get_xml_data.
    DATA: lv_xml_source TYPE string.

    lv_xml_source = '<?xml version="1.0" encoding="utf-8"?>' &&
      '<abapGit version="v1.0.0" serializer="LCL_OBJECT_PROG" serializer_version="v1.0.0">' &&
        '<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">' &&
          '<asx:values>' &&
            '<PROGDIR>' &&
              '<NAME>ZLF</NAME>' &&
              '<SUBC>1</SUBC>' &&
              '<RLOAD>E</RLOAD>' &&
              '<FIXPT>X</FIXPT>' &&
              '<UCCHECK>X</UCCHECK>' &&
            '</PROGDIR>' &&
          '</asx:values>' &&
        '</asx:abap>' &&
      '</abapGit>'.

    rv_result = zcl_abapgit_convert=>string_to_xstring_utf8( lv_xml_source ).

  ENDMETHOD.

  METHOD get_expected_abap_source.

    APPEND 'My first program' TO rt_result.
    APPEND 'has syntax errors' TO rt_result.

  ENDMETHOD.

  METHOD read_abap.

    cl_abap_unit_assert=>assert_equals( exp = get_expected_abap_source( )
      act = mo_cut->read_abap( ) ).

  ENDMETHOD.

  METHOD read_xml.
    DATA: BEGIN OF ls_exp_prog_metadata,
            name       TYPE progname VALUE 'ZLF',
            subc(1)    TYPE c VALUE '1',
            rload(1)   TYPE c VALUE 'E',
            fixpt(1)   TYPE c VALUE 'X',
            uccheck(1) TYPE c VALUE 'X',
          END OF ls_exp_prog_metadata,
          ls_act_prog_metadata LIKE ls_exp_prog_metadata,
          ls_exp_metadata      TYPE zif_abapgit_definitions=>ty_metadata,
          li_xml               TYPE REF TO zif_abapgit_xml_input.

    ls_exp_metadata-class = 'LCL_OBJECT_PROG'.
    ls_exp_metadata-version = 'v1.0.0'.

    li_xml = mo_cut->read_xml( ).
    li_xml->read( EXPORTING iv_name = 'PROGDIR' CHANGING cg_data = ls_act_prog_metadata ).

    cl_abap_unit_assert=>assert_equals( exp = ls_exp_prog_metadata
      act = ls_act_prog_metadata ).
    cl_abap_unit_assert=>assert_equals( exp = ls_exp_metadata
      act = mo_cut->read_xml( )->get_metadata( ) ).

  ENDMETHOD.

  METHOD get_file_pattern.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.

    " filenames are lower case
    cl_abap_unit_assert=>assert_equals(
      exp = 'zlf.prog.*'
      act = mo_cut->get_file_pattern( ) ).

    " object type and name are upper case
    ls_item-obj_type = 'PROG'.
    ls_item-obj_name = '/TEST/ZLF'.

    CREATE OBJECT mo_cut
      EXPORTING
        is_item = ls_item.

    " filenames are lower case
    cl_abap_unit_assert=>assert_equals(
      exp = '##test##zlf.prog.*'
      act = mo_cut->get_file_pattern( ) ).

  ENDMETHOD.

  METHOD is_json_metadata.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_data TYPE xstring.

    " object type and name are upper case
    ls_item-obj_type = 'CHKO'.
    ls_item-obj_name = 'Z_AFF_EXAMPLE_CHKO'.

    CREATE OBJECT mo_cut
      EXPORTING
        is_item = ls_item.

    mo_cut->add_raw( iv_data = lv_data
                     iv_ext  = 'json' ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = mo_cut->is_json_metadata( ) ).

  ENDMETHOD.

  METHOD is_not_json_metadata.

    " checks PROG ZLF (see setup)
    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = mo_cut->is_json_metadata( ) ).

  ENDMETHOD.

ENDCLASS.
