CLASS ltcl_feature DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    DATA mo_settings TYPE REF TO zcl_abapgit_settings.
    DATA mo_cut TYPE REF TO zcl_abapgit_feature.

    METHODS:
      setup,
      merged_disabled FOR TESTING,
      feature_disabled FOR TESTING,
      feature_enabled FOR TESTING.

ENDCLASS.

CLASS ltcl_feature IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
    mo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).
  ENDMETHOD.

  METHOD merged_disabled.

    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.

      cl_abap_unit_assert=>assert_equals(
        act = mo_cut->is_enabled( )
        exp = abap_false ).

      cl_abap_unit_assert=>assert_equals(
        act = mo_cut->is_enabled( 'TEST' )
        exp = abap_false ).

    ENDIF.

  ENDMETHOD.

  METHOD feature_disabled.

    " Only run for dev version
    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      RETURN.
    ENDIF.

    " All off
    mo_settings->set_experimental_features( '' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_enabled( 'TEST' )
      exp = abap_false ).

  ENDMETHOD.

  METHOD feature_enabled.

    " Only run for dev version
    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      RETURN.
    ENDIF.

    " All on
    mo_settings->set_experimental_features( 'X' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_enabled( 'TEST' )
      exp = abap_true ).

    " Just one feature on
    mo_settings->set_experimental_features( 'TEST' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_enabled( 'TEST' )
      exp = abap_true ).

    " Several features on
    mo_settings->set_experimental_features( 'AFF,LXE' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_enabled( 'TEST' )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_enabled( 'AFF' )
      exp = abap_true ).

    " Several features on with whitespace
    mo_settings->set_experimental_features( ' AFF , FLOW   ,LXE ' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_enabled( 'TEST' )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_enabled( 'FLOW' )
      exp = abap_true ).

  ENDMETHOD.

ENDCLASS.
