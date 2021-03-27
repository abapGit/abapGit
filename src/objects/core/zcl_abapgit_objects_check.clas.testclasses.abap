CLASS ltcl_warning_overwrite_find DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_objects   TYPE REF TO zcl_abapgit_objects_check,
      mt_result    TYPE zif_abapgit_definitions=>ty_results_tt,
      mt_overwrite TYPE zif_abapgit_definitions=>ty_overwrite_tt.

    METHODS:
      setup,
      warning_overwrite_find_01 FOR TESTING RAISING cx_static_check,
      warning_overwrite_find_02 FOR TESTING RAISING cx_static_check,
      warning_overwrite_find_03 FOR TESTING RAISING cx_static_check,
      warning_overwrite_find_04 FOR TESTING RAISING cx_static_check,

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

  METHOD warning_overwrite_find_01.

    " change remote but not local -> no overwrite
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.abap;;;;M| ).

    when_warning_overwrite_find( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lines( mt_overwrite ) ).

  ENDMETHOD.

  METHOD warning_overwrite_find_02.

    " change remote and local -> overwrite
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.abap;;;M;M| ).

    when_warning_overwrite_find( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( mt_overwrite ) ).

  ENDMETHOD.

  METHOD warning_overwrite_find_03.

    " delete local -> overwrite (since object will be created again from remote)
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.abap;;;D;| ).

    when_warning_overwrite_find( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( mt_overwrite ) ).

  ENDMETHOD.

  METHOD warning_overwrite_find_04.

    " exists local but not remote -> no overwrite
    " (object will be in delete confirmation popup: see ZCL_ABAPGIT_SERVICES_GIT->GET_UNNECESSARY_LOCAL_OBJS)
    given_result( |CLAS;ZAG_UNIT_TEST;;/src/;zag_unit_test.clas.abap;;;A;| ).

    when_warning_overwrite_find( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lines( mt_overwrite ) ).

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
           ls_result-rstate.

    INSERT ls_result INTO TABLE mt_result.

  ENDMETHOD.


  METHOD when_warning_overwrite_find.

    mt_overwrite = mo_objects->warning_overwrite_find( mt_result ).

  ENDMETHOD.

ENDCLASS.
