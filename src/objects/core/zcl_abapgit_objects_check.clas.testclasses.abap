CLASS ltcl_warning_overwrite_find DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_objects   TYPE REF TO zcl_abapgit_objects_check,
      mt_result    TYPE zif_abapgit_definitions=>ty_results_tt,
      ms_overwrite TYPE zif_abapgit_definitions=>ty_overwrite,
      mt_overwrite TYPE zif_abapgit_definitions=>ty_overwrite_tt.

    METHODS:
      setup,
      warning_overwrite_find_00 FOR TESTING RAISING cx_static_check,
      warning_overwrite_find_01 FOR TESTING RAISING cx_static_check,
      warning_overwrite_find_02 FOR TESTING RAISING cx_static_check,
      warning_overwrite_find_03 FOR TESTING RAISING cx_static_check,
      warning_overwrite_find_04 FOR TESTING RAISING cx_static_check,
      warning_overwrite_find_05 FOR TESTING RAISING cx_static_check,
      warning_overwrite_find_06 FOR TESTING RAISING cx_static_check,
      check_multiple_files_01 FOR TESTING RAISING cx_static_check,
      check_multiple_files_02 FOR TESTING RAISING cx_static_check,
      check_multiple_files_03 FOR TESTING RAISING cx_static_check,

      given_result
        IMPORTING
          iv_result_line TYPE string,


      when_warning_overwrite_find.

ENDCLASS.

CLASS zcl_abapgit_objects_check DEFINITION LOCAL FRIENDS ltcl_warning_overwrite_find.

CLASS ltcl_warning_overwrite_find IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_objects.

  ENDMETHOD.

  METHOD warning_overwrite_find_00.

    " no changes -> nothing to do
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.abap;;;;| ).

    when_warning_overwrite_find( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lines( mt_overwrite ) ).

  ENDMETHOD.

  METHOD warning_overwrite_find_01.

    " change remote but not local -> update
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.abap;;;;M| ).

    when_warning_overwrite_find( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( mt_overwrite ) ).

    READ TABLE mt_overwrite INTO ms_overwrite INDEX 1.
    ASSERT sy-subrc = 0.

    cl_abap_unit_assert=>assert_equals(
      exp = zif_abapgit_objects=>c_deserialize_action-update
      act = ms_overwrite-action ).

  ENDMETHOD.

  METHOD warning_overwrite_find_02.

    " change remote and local -> overwrite
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.abap;;;M;M| ).

    when_warning_overwrite_find( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( mt_overwrite ) ).

    READ TABLE mt_overwrite INTO ms_overwrite INDEX 1.
    ASSERT sy-subrc = 0.

    cl_abap_unit_assert=>assert_equals(
      exp = zif_abapgit_objects=>c_deserialize_action-overwrite
      act = ms_overwrite-action ).

  ENDMETHOD.

  METHOD warning_overwrite_find_03.

    " delete local -> add (since object will be created again from remote)
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.abap;;;D;| ).

    when_warning_overwrite_find( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( mt_overwrite ) ).

    READ TABLE mt_overwrite INTO ms_overwrite INDEX 1.
    ASSERT sy-subrc = 0.

    cl_abap_unit_assert=>assert_equals(
      exp = zif_abapgit_objects=>c_deserialize_action-add
      act = ms_overwrite-action ).

  ENDMETHOD.

  METHOD warning_overwrite_find_04.

    " exists local but not remote -> delete
    " (object will be in confirmation popup: see zcl_abapgit_services_repo=>gui_deserialize)
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.abap;;;A;| ).

    when_warning_overwrite_find( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( mt_overwrite ) ).

    READ TABLE mt_overwrite INTO ms_overwrite INDEX 1.
    ASSERT sy-subrc = 0.

    cl_abap_unit_assert=>assert_equals(
      exp = zif_abapgit_objects=>c_deserialize_action-delete
      act = ms_overwrite-action ).

  ENDMETHOD.

  METHOD warning_overwrite_find_05.

    " some part of object deleted remotely -> delete and recreate
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.abap;;;;| ).
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.testclass.clas.abap;;;;D| ).

    when_warning_overwrite_find( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( mt_overwrite ) ).

    READ TABLE mt_overwrite INTO ms_overwrite INDEX 1.
    ASSERT sy-subrc = 0.

    cl_abap_unit_assert=>assert_equals(
      exp = zif_abapgit_objects=>c_deserialize_action-delete_add
      act = ms_overwrite-action ).

  ENDMETHOD.

  METHOD warning_overwrite_find_06.

    " changed package assignment
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.abap;;;D;;X| ).
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/sub;zag_unit_test.clas.abap;;;A;;X| ).

    when_warning_overwrite_find( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( mt_overwrite ) ).

    READ TABLE mt_overwrite INTO ms_overwrite INDEX 1.
    ASSERT sy-subrc = 0.

    cl_abap_unit_assert=>assert_equals(
      exp = zif_abapgit_objects=>c_deserialize_action-packmove
      act = ms_overwrite-action ).

  ENDMETHOD.

  METHOD check_multiple_files_01.

    " same filename but different packages (paths)
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.abap;;;A;;| ).
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/sub;zag_unit_test.clas.abap;;;A;;| ).

    TRY.
        mo_objects->check_multiple_files( mt_result ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD check_multiple_files_02.

    " same filename but change of package and object (local add + delete)
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.abap;;;A;;| ).
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/sub;zag_unit_test.clas.abap;;;D;;| ).

    TRY.
        mo_objects->check_multiple_files( mt_result ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

  METHOD check_multiple_files_03.

    " same filename but change of package and object (remote add + delete)
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.abap;;;;A;| ).
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/sub;zag_unit_test.clas.abap;;;;D;| ).

    TRY.
        mo_objects->check_multiple_files( mt_result ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

  METHOD given_result.

    DATA: ls_result LIKE LINE OF mt_result.

    SPLIT iv_result_line
      AT ';'
      INTO ls_result-obj_type
           ls_result-obj_name
           ls_result-inactive
           ls_result-path
           ls_result-filename
           ls_result-package
           ls_result-match
           ls_result-lstate
           ls_result-rstate
           ls_result-packmove.

    INSERT ls_result INTO TABLE mt_result.

  ENDMETHOD.


  METHOD when_warning_overwrite_find.

    mt_overwrite = mo_objects->warning_overwrite_find( mt_result ).

  ENDMETHOD.

ENDCLASS.
