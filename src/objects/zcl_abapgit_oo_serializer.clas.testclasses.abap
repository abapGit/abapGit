CLASS ltcl_oo_serialize DEFINITION DEFERRED.
CLASS zcl_abapgit_oo_serializer DEFINITION LOCAL FRIENDS ltcl_oo_serialize.

CLASS ltcl_oo_serialize DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      empty_include FOR TESTING RAISING cx_static_check,
      one_line_include FOR TESTING RAISING cx_static_check,
      one_line_include_2 FOR TESTING RAISING cx_static_check,
      one_line_include_3 FOR TESTING RAISING cx_static_check,
      two_line_include FOR TESTING RAISING cx_static_check,
      two_line_include_2 FOR TESTING RAISING cx_static_check,
      two_line_include_3 FOR TESTING RAISING cx_static_check,
      more_than_two_lines FOR TESTING RAISING cx_static_check,

      _given_source_is
        IMPORTING
          iv_source TYPE LINE OF zif_abapgit_definitions=>ty_string_tt,
      _given_empty_test_include,
      _when_skip_is_calculated,
      _then_should_be_skipped,
      _then_should_not_be_skipped.

    DATA: mo_oo_serializer  TYPE REF TO zcl_abapgit_oo_serializer,
          mt_source         TYPE zif_abapgit_definitions=>ty_string_tt,
          mv_skip_testclass TYPE abap_bool.

ENDCLASS.


CLASS ltcl_oo_serialize IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_oo_serializer.

  ENDMETHOD.

  METHOD empty_include.

    _given_empty_test_include( ).

    _when_skip_is_calculated( ).

    _then_should_be_skipped( ).

  ENDMETHOD.

  METHOD one_line_include.

    _given_source_is( `*"* use this source file for your ABAP unit test classes` ).

    _when_skip_is_calculated( ).

    _then_should_be_skipped( ).

  ENDMETHOD.

  METHOD one_line_include_2.

    _given_source_is( `*` ).

    _when_skip_is_calculated( ).

    _then_should_be_skipped( ).

  ENDMETHOD.

  METHOD one_line_include_3.

    _given_source_is( `write: 'This is ABAP'.` ).

    _when_skip_is_calculated( ).

    _then_should_not_be_skipped( ).

  ENDMETHOD.

  METHOD two_line_include.

    _given_source_is( `*"* use this source file for your ABAP unit test classes` ).
    _given_source_is( ``                                                         ).

    _when_skip_is_calculated( ).

    _then_should_be_skipped( ).

  ENDMETHOD.

  METHOD two_line_include_2.

    _given_source_is( `*"* use this source file for your ABAP unit test classes` ).
    _given_source_is( `write: 'This is ABAP'.`                                   ).

    _when_skip_is_calculated( ).

    _then_should_not_be_skipped( ).

  ENDMETHOD.

  METHOD two_line_include_3.

    _given_source_is( ` `                                                        ).
    _given_source_is( `*"* use this source file for your ABAP unit test classes` ).

    _when_skip_is_calculated( ).

    _then_should_not_be_skipped( ).

  ENDMETHOD.

  METHOD more_than_two_lines.

    _given_source_is( `*"* use this source file for your ABAP unit test classes` ).
    _given_source_is( `CLASS ltcl_test DEFINITION FINAL FOR TESTING`             ).
    _given_source_is( `  DURATION SHORT`                                         ).
    _given_source_is( `  RISK LEVEL HARMLESS.`                                   ).
    _given_source_is( ` `                                                        ).
    _given_source_is( `  PRIVATE SECTION.`                                       ).
    _given_source_is( `    METHODS:`                                             ).
    _given_source_is( `      first_test FOR TESTING RAISING cx_static_check.`    ).
    _given_source_is( `ENDCLASS.`                                                ).
    _given_source_is( ` `                                                        ).
    _given_source_is( `CLASS ltcl_test IMPLEMENTATION.`                          ).
    _given_source_is( ` `                                                        ).
    _given_source_is( `  METHOD first_test.`                                     ).
    _given_source_is( `    cl_abap_unit_assert=>fail( 'This is a real test' ).`  ).
    _given_source_is( `  ENDMETHOD.`                                             ).
    _given_source_is( ` `                                                        ).
    _given_source_is( `ENDCLASS.`                                                ).

    _when_skip_is_calculated( ).

    _then_should_not_be_skipped( ).

  ENDMETHOD.

  METHOD _given_source_is.

    INSERT iv_source INTO TABLE mt_source.

  ENDMETHOD.

  METHOD _given_empty_test_include.

  ENDMETHOD.

  METHOD _when_skip_is_calculated.

    mv_skip_testclass = mo_oo_serializer->calculate_skip_testclass( mt_source ).

  ENDMETHOD.

  METHOD _then_should_be_skipped.

    cl_abap_unit_assert=>assert_equals(
      act = mv_skip_testclass
      exp = abap_true
      msg = |Testclass should be skipped| ).

  ENDMETHOD.


  METHOD _then_should_not_be_skipped.

    cl_abap_unit_assert=>assert_equals(
      act = mv_skip_testclass
      exp = abap_false
      msg = |Testclass should not be skipped| ).

  ENDMETHOD.

ENDCLASS.
