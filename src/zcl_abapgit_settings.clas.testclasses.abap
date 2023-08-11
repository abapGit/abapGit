CLASS ltcl_settings DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abapgit_settings.

    METHODS:
      setup,
      feature_disabled FOR TESTING,
      feature_enabled FOR TESTING.

ENDCLASS.

CLASS ltcl_settings IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD feature_disabled.

    " Only run for dev version
    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      RETURN.
    ENDIF.

    " All off
    mo_cut->set_experimental_features( '' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_experimental_features( )
      exp = '' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_feature_enabled( 'TEST' )
      exp = abap_false ).

  ENDMETHOD.

  METHOD feature_enabled.

    " Only run for dev version
    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      RETURN.
    ENDIF.

    " All on
    mo_cut->set_experimental_features( 'X' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_experimental_features( )
      exp = 'X' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_feature_enabled( 'TEST' )
      exp = abap_true ).

    " Just one feature on
    mo_cut->set_experimental_features( 'TEST' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_experimental_features( )
      exp = 'TEST' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_feature_enabled( 'TEST' )
      exp = abap_true ).

    " Several features on
    mo_cut->set_experimental_features( 'AFF,LXE' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_feature_enabled( 'TEST' )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_feature_enabled( 'AFF' )
      exp = abap_true ).

  ENDMETHOD.

ENDCLASS.
