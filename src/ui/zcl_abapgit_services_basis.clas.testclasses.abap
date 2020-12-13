*"* use this source file for your ABAP unit test classes
CLASS ltcl_sap_package_mock DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_sap_package.

    CONSTANTS:
      BEGIN OF c_package,
        existing     TYPE devclass VALUE '$EXISTING',
        non_existing TYPE devclass VALUE '$NON_EXISTING',
      END OF c_package.

    METHODS:
      constructor
        IMPORTING
          iv_package TYPE devclass,
      get_created
        RETURNING
          VALUE(rv_created) TYPE abap_bool.

  PRIVATE SECTION.
    DATA:
      mv_package TYPE devclass,
      mv_created TYPE abap_bool.

ENDCLASS.


CLASS ltcl_popups_mock DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    TYPES:
      ty_user_decision TYPE c LENGTH 1.

    CONSTANTS:
      BEGIN OF c_user_decision,
        cancel TYPE ty_user_decision VALUE 'C',
      END OF c_user_decision.

    INTERFACES:
      zif_abapgit_popups.

    METHODS:
      was_create_package_popup_shown
        RETURNING
          VALUE(rv_popup_shown) TYPE abap_bool,

      set_user_decision
        IMPORTING
          iv_user_decision TYPE ty_user_decision.

  PRIVATE SECTION.
    DATA:
      BEGIN OF ms_called,
        popup_to_create_package TYPE abap_bool,
      END OF ms_called,
      mv_user_decision TYPE ltcl_popups_mock=>ty_user_decision.

ENDCLASS.


CLASS ltcl_create_package DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PUBLIC SECTION.
  PRIVATE SECTION.
    DATA:
      mv_package          TYPE devclass,
      mx_error            TYPE REF TO zcx_abapgit_exception,
      mo_popups_mock      TYPE REF TO ltcl_popups_mock,
      mo_sap_package_mock TYPE REF TO ltcl_sap_package_mock.

    METHODS:
      setup,

      raise_error_if_package_exists FOR TESTING RAISING cx_static_check,
      popup_if_package_doesnt_exist FOR TESTING RAISING cx_static_check,
      package_not_created_when_canc FOR TESTING RAISING cx_static_check,

      given_existing_package,
      given_non_existing_package,

      when_create_package,

      then_error_should_be_raised,
      then_no_error_should_be_raised,
      then_popup_is_shown,
      given_user_cancels_popup,
      then_no_package_is_created.

ENDCLASS.


CLASS ltcl_create_package IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_popups_mock TYPE ltcl_popups_mock.
    zcl_abapgit_ui_injector=>set_popups( mo_popups_mock ).

  ENDMETHOD.


  METHOD raise_error_if_package_exists.

    given_existing_package( ).
    when_create_package( ).
    then_error_should_be_raised( ).

  ENDMETHOD.


  METHOD popup_if_package_doesnt_exist.

    given_non_existing_package( ).
    when_create_package( ).
    then_popup_is_shown( ).

  ENDMETHOD.


  METHOD package_not_created_when_canc.

    given_non_existing_package( ).
    given_user_cancels_popup( ).
    when_create_package( ).
    then_no_package_is_created( ).

  ENDMETHOD.


  METHOD when_create_package.

    CREATE OBJECT mo_sap_package_mock
      EXPORTING
        iv_package = mv_package.

    zcl_abapgit_injector=>set_sap_package(
        iv_package     = mv_package
        ii_sap_package = mo_sap_package_mock ).

    TRY.
        zcl_abapgit_services_basis=>create_package( mv_package ).
      CATCH zcx_abapgit_exception INTO mx_error.
    ENDTRY.

  ENDMETHOD.


  METHOD given_existing_package.

    mv_package = ltcl_sap_package_mock=>c_package-existing.

  ENDMETHOD.


  METHOD given_non_existing_package.

    mv_package = ltcl_sap_package_mock=>c_package-non_existing.

  ENDMETHOD.


  METHOD then_error_should_be_raised.

    cl_abap_unit_assert=>assert_bound( mx_error ).

  ENDMETHOD.


  METHOD then_no_error_should_be_raised.

    cl_abap_unit_assert=>assert_not_bound( mx_error ).

  ENDMETHOD.


  METHOD then_popup_is_shown.

    cl_abap_unit_assert=>assert_true( mo_popups_mock->was_create_package_popup_shown( )  ).

  ENDMETHOD.


  METHOD given_user_cancels_popup.

    mo_popups_mock->set_user_decision( ltcl_popups_mock=>c_user_decision-cancel ).

  ENDMETHOD.


  METHOD then_no_package_is_created.

    cl_abap_unit_assert=>assert_false( mo_sap_package_mock->get_created( ) ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_sap_package_mock IMPLEMENTATION.

  METHOD constructor.

    mv_package = iv_package.

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~are_changes_recorded_in_tr_req.

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~create.

    mv_created = abap_true.

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~create_child.

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~create_local.

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~exists.

    rv_bool = boolc( c_package-existing = mv_package ).

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~get_transport_type.

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~list_subpackages.

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~list_superpackages.

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~read_parent.

  ENDMETHOD.


  METHOD get_created.

    rv_created = mv_created.

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_popups_mock IMPLEMENTATION.

  METHOD zif_abapgit_popups~branch_list_popup.

  ENDMETHOD.

  METHOD zif_abapgit_popups~branch_popup_callback.

  ENDMETHOD.

  METHOD zif_abapgit_popups~choose_pr_popup.

  ENDMETHOD.

  METHOD zif_abapgit_popups~create_branch_popup.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_folder_logic.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_object.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_package_export.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_perf_test_parameters.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_proxy_bypass.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_search_help.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_confirm.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_create_package.

    ms_called-popup_to_create_package = abap_true.

    IF mv_user_decision = c_user_decision-cancel.
      ev_create = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_create_transp_branch.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_inform.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_select_from_list.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_select_transports.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_transport_request.

  ENDMETHOD.

  METHOD zif_abapgit_popups~repo_popup.

  ENDMETHOD.

  METHOD was_create_package_popup_shown.

    rv_popup_shown = ms_called-popup_to_create_package.

  ENDMETHOD.

  METHOD set_user_decision.

    mv_user_decision = iv_user_decision.

  ENDMETHOD.

ENDCLASS.
