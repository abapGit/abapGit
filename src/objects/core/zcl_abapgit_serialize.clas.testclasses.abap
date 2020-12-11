CLASS ltcl_determine_max_threads DEFINITION DEFERRED.
CLASS zcl_abapgit_serialize DEFINITION LOCAL FRIENDS ltcl_determine_max_threads.

CLASS ltcl_determine_max_threads DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abapgit_serialize.

    METHODS:
      setup,
      determine_max_threads FOR TESTING RAISING zcx_abapgit_exception,
      force FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_determine_max_threads IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD determine_max_threads.

    DATA: lv_threads TYPE i.

    lv_threads = mo_cut->determine_max_threads( ).

    cl_abap_unit_assert=>assert_differs(
      act = lv_threads
      exp = 0 ).

  ENDMETHOD.

  METHOD force.

    DATA: lv_threads TYPE i.

    lv_threads = mo_cut->determine_max_threads( abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_threads
      exp = 1 ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_serialize DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abapgit_serialize.

    METHODS:
      setup,
      test FOR TESTING RAISING zcx_abapgit_exception,
      unsupported FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_serialize IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD test.

    DATA: lt_tadir      TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lt_sequential TYPE zif_abapgit_definitions=>ty_files_item_tt,
          lt_parallel   TYPE zif_abapgit_definitions=>ty_files_item_tt.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF lt_tadir.


    APPEND INITIAL LINE TO lt_tadir ASSIGNING <ls_tadir>.
    <ls_tadir>-object   = 'PROG'.
    <ls_tadir>-obj_name = 'RSABAPPROGRAM'.
    <ls_tadir>-devclass = 'PACKAGE'.
    <ls_tadir>-path     = 'foobar'.

    lt_sequential = mo_cut->serialize(
      it_tadir            = lt_tadir
      iv_force_sequential = abap_true ).

    lt_parallel = mo_cut->serialize(
      it_tadir            = lt_tadir
      iv_force_sequential = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_sequential
      exp = lt_parallel ).

  ENDMETHOD.

  METHOD unsupported.

    DATA: lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt,
          li_log1  TYPE REF TO zif_abapgit_log,
          li_log2  TYPE REF TO zif_abapgit_log.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF lt_tadir.


    APPEND INITIAL LINE TO lt_tadir ASSIGNING <ls_tadir>.
    <ls_tadir>-object   = 'ABCD'.
    <ls_tadir>-obj_name = 'OBJECT'.

    CREATE OBJECT li_log1 TYPE zcl_abapgit_log.
    mo_cut->serialize(
      it_tadir            = lt_tadir
      ii_log              = li_log1
      iv_force_sequential = abap_true ).

    CREATE OBJECT li_log2 TYPE zcl_abapgit_log.
    mo_cut->serialize(
      it_tadir            = lt_tadir
      ii_log              = li_log2
      iv_force_sequential = abap_false ).

    cl_abap_unit_assert=>assert_char_cp(
      act = zcl_abapgit_log_viewer=>to_html( li_log1 )->render( )
      exp = '*Object type ignored, not supported*' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = zcl_abapgit_log_viewer=>to_html( li_log2 )->render( )
      exp = '*Object type ignored, not supported*' ).

  ENDMETHOD.

ENDCLASS.
