CLASS ltc_ci DEFINITION FINAL FOR TESTING
  DURATION MEDIUM
  RISK LEVEL CRITICAL.

  PRIVATE SECTION.
    METHODS run_ci FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_ci IMPLEMENTATION.

  METHOD run_ci.

    DATA lv_repo_url TYPE string.

    IF zcl_abapgit_persist_factory=>get_settings( )->read( )->get_experimental_features( ) = abap_false.
      RETURN.
    ENDIF.

    "Use STVARV to optionally override repo in local system
    SELECT SINGLE low
      INTO lv_repo_url
      FROM tvarvc
      WHERE name = 'ABAPGIT_TEST_URL_PDTS'  ##WARN_OK.

    zcl_abapgit_objects_ci_tests=>run(
        iv_object = 'PDTS'
        iv_url  = lv_repo_url ).

  ENDMETHOD.

ENDCLASS.



CLASS ltc_smoke_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA mo_cut TYPE REF TO zif_abapgit_object.

    METHODS setup.
    METHODS run_simple_methods FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_smoke_test IMPLEMENTATION.

  METHOD setup.

    DATA  ls_item   TYPE zif_abapgit_definitions=>ty_item.

    IF zcl_abapgit_persist_factory=>get_settings( )->read( )->get_experimental_features( ) = abap_false.
      RETURN.
    ENDIF.

    ls_item-obj_type = 'PDTS'.
    ls_item-obj_name = '99999999'.

    TRY.
        CREATE OBJECT mo_cut TYPE zcl_abapgit_object_pdts
          EXPORTING
            is_item     = ls_item
            iv_language = sy-langu.
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

  METHOD run_simple_methods.

    IF zcl_abapgit_persist_factory=>get_settings( )->read( )->get_experimental_features( ) = abap_false.
      RETURN.
    ENDIF.

    mo_cut->get_comparator( ).
    mo_cut->get_deserialize_steps( ).
    mo_cut->get_metadata( ).
    mo_cut->is_active( ).
  ENDMETHOD.

ENDCLASS.
