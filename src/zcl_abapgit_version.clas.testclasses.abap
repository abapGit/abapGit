CLASS ltcl_version DEFINITION DEFERRED.
CLASS zcl_abapgit_version DEFINITION LOCAL FRIENDS ltcl_version.

CLASS ltcl_version DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS:
      version_to_numeric FOR TESTING,
      compare            FOR TESTING,
      normalize          FOR TESTING.

ENDCLASS.

CLASS ltcl_version IMPLEMENTATION.

  METHOD version_to_numeric.

    DATA: lv_version_exp TYPE i VALUE 1023010,
          lv_version_act TYPE i.

    lv_version_act = zcl_abapgit_version=>version_to_numeric( '1.23.10' ).

    cl_abap_unit_assert=>assert_equals( exp = lv_version_exp
                                        act = lv_version_act
                                        msg = ' Error during conversion of version to numeric value' ).

  ENDMETHOD.

  METHOD compare.

    DATA lv_result TYPE i.

    " Case 1: version A > version B
    lv_result = zcl_abapgit_version=>compare( iv_a = '1.28.10'
                                              iv_b = '1.23.10' ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lv_result
                                        msg = ' Error during comparison of versions. Case: A > B' ).

    CLEAR: lv_result.

    " Case 2: version A < version B
    lv_result = zcl_abapgit_version=>compare( iv_a = '1.28.10'
                                              iv_b = '2.23.10' ).

    cl_abap_unit_assert=>assert_equals( exp = -1
                                        act = lv_result
                                        msg = ' Error during comparison of versions. Case: A < B' ).

    CLEAR: lv_result.

    " Case 3: version A = version B
    lv_result = zcl_abapgit_version=>compare( iv_a = '1.28.10'
                                              iv_b = '1.28.10' ).

    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = lv_result
                                        msg = ' Error during comparison of versions. Case: A = B' ).

  ENDMETHOD.

  METHOD normalize.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_version=>normalize( '1.28.10' )
      exp = '1.28.10' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_version=>normalize( 'v1.28.10' )
      exp = '1.28.10' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_version=>normalize( 'b1.28.10' )
      exp = '' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_version=>normalize( 'x.y.z' )
      exp = '' ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_version_parse DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    METHODS:
      abapgit_version FOR TESTING,
      interface1 FOR TESTING,
      interface2 FOR TESTING,
      class1 FOR TESTING,
      class_private FOR TESTING,
      structured_constant FOR TESTING,
      int4 FOR TESTING,
      decfloat34 FOR TESTING,
      syntax_error FOR TESTING,
      class_data FOR TESTING,
      ampersand FOR TESTING,
      constant_reference FOR TESTING,
      missing_constant FOR TESTING.
  PRIVATE SECTION.
    DATA:
      mt_given_source         TYPE string_table,
      mv_given_component_name TYPE string,
      mv_parsed_version       TYPE string,
      mo_raised_exception     TYPE REF TO zcx_abapgit_exception.
    METHODS:
      given_the_source IMPORTING it_source TYPE string_table,
      given_the_component_name IMPORTING iv_component_name TYPE string,
      when_parse_is_called,
      then_should_raise_exception,
      then_version_should_equal IMPORTING iv_version TYPE string,
      teardown.
ENDCLASS.

CLASS ltcl_version_parse IMPLEMENTATION.
  METHOD abapgit_version.
    DATA: lt_source TYPE string_table.

    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      TRY.
          CALL METHOD cl_abap_unit_assert=>('SKIP')
            EXPORTING
              msg = 'Test method not supported in standalone version'.
        CATCH cx_sy_dyn_call_illegal_method. " NW <= 752
          RETURN.
      ENDTRY.
    ENDIF.

    READ REPORT 'ZIF_ABAPGIT_VERSION===========IU' INTO lt_source STATE 'A'.
    cl_abap_unit_assert=>assert_subrc( ).

    given_the_source( lt_source ).
    given_the_component_name( 'C_ABAP_VERSION' ).
    when_parse_is_called( ).
    then_version_should_equal( zif_abapgit_version=>c_abap_version ).
  ENDMETHOD.

  METHOD interface1.
    DATA: lt_source TYPE string_table.

    APPEND 'INTERFACE version.' TO lt_source.
    APPEND '  CONSTANTS:' TO lt_source.
    APPEND '    version TYPE string VALUE `1.2.3`.' TO lt_source.
    APPEND 'ENDINTERFACE.' TO lt_source.

    given_the_source( lt_source ).
    given_the_component_name( 'VERSION' ).
    when_parse_is_called( ).
    then_version_should_equal( '1.2.3' ).
  ENDMETHOD.

  METHOD interface2.
    DATA: lt_source TYPE string_table.

    APPEND 'INTERFACE version.' TO lt_source.
    APPEND '  CONSTANTS version TYPE string VALUE `1.2.3`.' TO lt_source.
    APPEND 'ENDINTERFACE.' TO lt_source.

    given_the_source( lt_source ).
    given_the_component_name( 'VERSION' ).
    when_parse_is_called( ).
    then_version_should_equal( '1.2.3' ).
  ENDMETHOD.

  METHOD class1.
    DATA: lt_source TYPE string_table.

    APPEND 'CLASS version DEFINITION CREATE PRIVATE PUBLIC.' TO lt_source.
    APPEND '  PUBLIC SECTION.' TO lt_source.
    APPEND '    CONSTANTS:' TO lt_source.
    APPEND '      version TYPE string VALUE `1.2.3`.' TO lt_source.
    APPEND 'ENDCLASS.' TO lt_source.

    given_the_source( lt_source ).
    given_the_component_name( 'VERSION' ).
    when_parse_is_called( ).
    then_version_should_equal( '1.2.3' ).
  ENDMETHOD.

  METHOD class_private.
    DATA: lt_source TYPE string_table.

    APPEND 'CLASS version DEFINITION CREATE PRIVATE PUBLIC.' TO lt_source.
    APPEND '  PRIVATE SECTION.' TO lt_source.
    APPEND '    CONSTANTS:' TO lt_source.
    APPEND '      version TYPE string VALUE `1.2.3`.' TO lt_source.
    APPEND 'ENDCLASS.' TO lt_source.

    given_the_source( lt_source ).
    given_the_component_name( 'VERSION' ).
    when_parse_is_called( ).
    then_version_should_equal( '1.2.3' ).
  ENDMETHOD.

  METHOD structured_constant.
    DATA: lt_source TYPE string_table.

    APPEND 'INTERFACE version.' TO lt_source.
    APPEND '  CONSTANTS:' TO lt_source.
    APPEND '    BEGIN OF structure,' TO lt_source.
    APPEND '      BEGIN OF inner_structure,' TO lt_source.
    APPEND '        version TYPE string VALUE `1.0.0`,' TO lt_source.
    APPEND '        text    TYPE string VALUE `inner`,' TO lt_source.
    APPEND '      END OF inner_structre,' TO lt_source.
    APPEND '      outer_text TYPE string VALUE `outer`,' TO lt_source.
    APPEND '    END OF structure.' TO lt_source.
    APPEND 'ENDINTERFACE.' TO lt_source.

    given_the_source( lt_source ).
    given_the_component_name( 'STRUCTURE-INNER_STRUCTURE-VERSION' ).
    when_parse_is_called( ).
    then_should_raise_exception( ).
  ENDMETHOD.

  METHOD int4.
    DATA: lt_source TYPE string_table.

    APPEND 'INTERFACE version DEFINITION CREATE PRIVATE PUBLIC.' TO lt_source.
    APPEND '  CONSTANTS:' TO lt_source.
    APPEND '    version TYPE i VALUE 6.' TO lt_source.
    APPEND 'ENDINTERFACE.' TO lt_source.

    given_the_source( lt_source ).
    given_the_component_name( 'VERSION' ).
    when_parse_is_called( ).
    then_version_should_equal( '6' ).
  ENDMETHOD.

  METHOD decfloat34.
    DATA: lt_source TYPE string_table.

    APPEND 'INTERFACE version DEFINITION CREATE PRIVATE PUBLIC.' TO lt_source.
    APPEND '  CONSTANTS:' TO lt_source.
    APPEND `    version TYPE decfloat34 VALUE '3.14'.` TO lt_source.
    APPEND 'ENDINTERFACE.' TO lt_source.

    given_the_source( lt_source ).
    given_the_component_name( 'VERSION' ).
    when_parse_is_called( ).
    then_version_should_equal( '3.14' ).
  ENDMETHOD.

  METHOD syntax_error.
    DATA: lt_source TYPE string_table.

    APPEND 'INTERFACE version.' TO lt_source.
    APPEND '  CONSTANTS:' TO lt_source.
    APPEND '    version TYPE string VAL `1.2.3`.' TO lt_source.
    APPEND 'ENDINTERFACE.' TO lt_source.

    given_the_source( lt_source ).
    given_the_component_name( 'VERSION' ).
    when_parse_is_called( ).
    then_should_raise_exception( ).
  ENDMETHOD.

  METHOD class_data.
    DATA: lt_source TYPE string_table.

    APPEND 'INTERFACE version.' TO lt_source.
    APPEND '  CLASS-DATA:' TO lt_source.
    APPEND '    version TYPE string VALUE `1.2.3`.' TO lt_source.
    APPEND 'ENDINTERFACE.' TO lt_source.

    given_the_source( lt_source ).
    given_the_component_name( 'VERSION' ).
    when_parse_is_called( ).
    then_should_raise_exception( ).
  ENDMETHOD.

  METHOD ampersand.
    DATA: lt_source TYPE string_table.

    APPEND 'INTERFACE version.' TO lt_source.
    APPEND '  CONSTANTS:' TO lt_source.
    APPEND '    version TYPE string VALUE `1` &' TO lt_source.
    APPEND '    `.2` & `.3`.' TO lt_source.
    APPEND 'ENDINTERFACE.' TO lt_source.

    given_the_source( lt_source ).
    given_the_component_name( 'VERSION' ).
    when_parse_is_called( ).
    then_version_should_equal( '1.2.3' ).
  ENDMETHOD.

  METHOD constant_reference.
    DATA: lt_source TYPE string_table.

    APPEND 'INTERFACE version.' TO lt_source.
    APPEND '  CONSTANTS:' TO lt_source.
    APPEND '    actual_version TYPE string VALUE `1.2.3`,' TO lt_source.
    APPEND '    version        TYPE string VALUE actual_version.' TO lt_source.
    APPEND 'ENDINTERFACE.' TO lt_source.

    given_the_source( lt_source ).
    given_the_component_name( 'VERSION' ).
    when_parse_is_called( ).
    then_should_raise_exception( ).
  ENDMETHOD.

  METHOD missing_constant.
    DATA: lt_source TYPE string_table.

    APPEND 'INTERFACE version.' TO lt_source.
    APPEND '  CONSTANTS:' TO lt_source.
    APPEND '    version TYPE string VALUE `1.2.3`.' TO lt_source.
    APPEND 'ENDINTERFACE.' TO lt_source.

    given_the_source( lt_source ).
    given_the_component_name( 'SOME_MISSING_CONSTANT' ).
    when_parse_is_called( ).
    then_should_raise_exception( ).
  ENDMETHOD.

  METHOD given_the_source.
    mt_given_source = it_source.
  ENDMETHOD.

  METHOD given_the_component_name.
    mv_given_component_name = iv_component_name.
  ENDMETHOD.

  METHOD when_parse_is_called.
    TRY.
        mv_parsed_version = zcl_abapgit_version=>parse_version_from_source(
          it_source         = mt_given_source
          iv_component_name = mv_given_component_name ).
      CATCH zcx_abapgit_exception INTO mo_raised_exception ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD then_should_raise_exception.
    cl_abap_unit_assert=>assert_bound( mo_raised_exception ).
  ENDMETHOD.

  METHOD then_version_should_equal.
    cl_abap_unit_assert=>assert_equals(
      exp = iv_version
      act = mv_parsed_version ).
  ENDMETHOD.

  METHOD teardown.
    CLEAR mt_given_source.
    CLEAR mv_given_component_name.
    CLEAR mv_parsed_version.
    FREE mo_raised_exception.
  ENDMETHOD.
ENDCLASS.
