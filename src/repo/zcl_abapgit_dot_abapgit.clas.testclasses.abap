CLASS ltcl_dot_abapgit DEFINITION DEFERRED.
CLASS zcl_abapgit_dot_abapgit DEFINITION LOCAL FRIENDS ltcl_dot_abapgit.

CLASS ltcl_dot_abapgit DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      identity FOR TESTING
        RAISING zcx_abapgit_exception,
      ignore FOR TESTING.

ENDCLASS.

CLASS ltcl_dot_abapgit IMPLEMENTATION.

  METHOD identity.

    DATA: lo_dot    TYPE REF TO zcl_abapgit_dot_abapgit,
          ls_before TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit,
          ls_after  TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit.


    lo_dot = zcl_abapgit_dot_abapgit=>build_default( ).
    ls_before = lo_dot->ms_data.

    lo_dot = zcl_abapgit_dot_abapgit=>deserialize( lo_dot->serialize( ) ).
    ls_after = lo_dot->ms_data.

    cl_abap_unit_assert=>assert_equals(
      act = ls_after
      exp = ls_before ).

  ENDMETHOD.

  METHOD ignore.

    CONSTANTS: lc_path     TYPE string VALUE '/src/',
               lc_root     TYPE string VALUE '/',
               lc_filename TYPE string VALUE 'foobar.txt'.

    DATA: lv_ignored TYPE abap_bool,
          lo_dot     TYPE REF TO zcl_abapgit_dot_abapgit.


    lo_dot = zcl_abapgit_dot_abapgit=>build_default( ).

    " Any file in default starting folder /src/ should not be ignored
    lv_ignored = lo_dot->is_ignored( iv_path = lc_path
                                     iv_filename = lc_filename ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_ignored
      exp = abap_false ).

    " Add file to ignore list -> expect to be ignored
    lo_dot->add_ignore( iv_path = lc_path
                        iv_filename = lc_filename ).

    lv_ignored = lo_dot->is_ignored( iv_path = lc_path
                                     iv_filename = lc_filename ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_ignored
      exp = abap_true ).

    " Remove file from ignore list -> expect to be allowed
    lo_dot->remove_ignore( iv_path = lc_path
                           iv_filename = lc_filename ).

    lv_ignored = lo_dot->is_ignored( iv_path = lc_path
                                     iv_filename = lc_filename ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_ignored
      exp = abap_false ).

    " .abapgit.xml and .apack-manifest.xml must always be allowed
    lv_ignored = lo_dot->is_ignored( iv_path = lc_root
                                     iv_filename = zif_abapgit_definitions=>c_dot_abapgit ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_ignored
      exp = abap_false ).

    lv_ignored = lo_dot->is_ignored( iv_path = lc_root
                                     iv_filename = zif_abapgit_apack_definitions=>c_dot_apack_manifest ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_ignored
      exp = abap_false ).

    " File in root must be ignored since it's not under starting folder
    lv_ignored = lo_dot->is_ignored( iv_path = lc_root
                                     iv_filename = 'abaplint.json' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_ignored
      exp = abap_true ).

    " File under starting folder must not be ignored
    lv_ignored = lo_dot->is_ignored( iv_path = lc_path
                                     iv_filename = 'ztest.prog.abap' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_ignored
      exp = abap_false ).

  ENDMETHOD.

ENDCLASS.
