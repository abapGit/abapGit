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
        user         TYPE devclass VALUE '$USER',
      END OF c_package.

    METHODS:
      constructor
        IMPORTING
          iv_package TYPE devclass,

      was_create_called
        RETURNING
          VALUE(rv_create_called) TYPE abap_bool.

  PRIVATE SECTION.
    DATA:
      mv_package TYPE devclass,
      BEGIN OF ms_called,
        create TYPE abap_bool,
      END OF ms_called.

ENDCLASS.


CLASS ltcl_popups_mock DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    TYPES:
      ty_user_decision TYPE string.

    CONSTANTS:
      BEGIN OF c_user_decision,
        cancel  TYPE ty_user_decision VALUE 'cancel',
        confirm TYPE ty_user_decision VALUE 'confirm',
      END OF c_user_decision.

    INTERFACES:
      zif_abapgit_popups.

    METHODS:
      was_create_package_popup_shown
        RETURNING
          VALUE(rv_popup_shown) TYPE abap_bool,

      set_user_decision
        IMPORTING
          iv_user_decision TYPE ty_user_decision,

      set_package
        IMPORTING
          iv_package TYPE devclass.

  PRIVATE SECTION.
    DATA:
      BEGIN OF ms_called,
        popup_to_create_package TYPE abap_bool,
      END OF ms_called,
      mv_user_decision TYPE ty_user_decision,
      mv_package       TYPE devclass.

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
      mo_sap_package_mock TYPE REF TO ltcl_sap_package_mock,
      mv_created_package  TYPE devclass.

    METHODS:
      setup,

      raise_error_if_package_exists FOR TESTING RAISING cx_static_check,
      package_given_in_popup      FOR TESTING RAISING cx_static_check,
      package_not_created_when_canc FOR TESTING RAISING cx_static_check,
      package_created_when_confirm  FOR TESTING RAISING cx_static_check,

      given_existing_package,
      given_non_existing_package,
      given_user_cancels_popup,
      given_user_confirms_popup,
      given_no_package,
      given_package_by_user,

      when_create_package,

      then_error_is_raised,
      then_popup_is_shown,
      then_no_package_is_created,
      then_package_is_created,
      then_no_error_is_raised.

ENDCLASS.


CLASS ltcl_create_package IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_popups_mock TYPE ltcl_popups_mock.
    zcl_abapgit_ui_injector=>set_popups( mo_popups_mock ).

  ENDMETHOD.


  METHOD raise_error_if_package_exists.

    given_existing_package( ).
    when_create_package( ).
    then_error_is_raised( ).

  ENDMETHOD.


  METHOD package_given_in_popup.

    given_no_package( ).
    given_package_by_user( ).
    given_user_confirms_popup( ).
    when_create_package( ).
    then_no_error_is_raised( ).
    then_popup_is_shown( ).
    then_package_is_created( ).

  ENDMETHOD.


  METHOD package_not_created_when_canc.

    given_non_existing_package( ).
    given_user_cancels_popup( ).
    when_create_package( ).
    then_popup_is_shown( ).
    then_no_package_is_created( ).

  ENDMETHOD.


  METHOD package_created_when_confirm.

    given_non_existing_package( ).
    given_user_confirms_popup( ).
    when_create_package( ).
    then_popup_is_shown( ).
    then_package_is_created( ).

  ENDMETHOD.


  METHOD when_create_package.

    CREATE OBJECT mo_sap_package_mock
      EXPORTING
        iv_package = mv_package.

    zcl_abapgit_injector=>set_sap_package(
        iv_package     = mv_package
        ii_sap_package = mo_sap_package_mock ).

    TRY.
        mv_created_package = zcl_abapgit_services_basis=>create_package( mv_package ).
      CATCH zcx_abapgit_exception INTO mx_error.
    ENDTRY.

  ENDMETHOD.


  METHOD given_existing_package.

    mv_package = ltcl_sap_package_mock=>c_package-existing.

  ENDMETHOD.


  METHOD given_non_existing_package.

    mv_package = ltcl_sap_package_mock=>c_package-non_existing.

  ENDMETHOD.


  METHOD then_error_is_raised.

    cl_abap_unit_assert=>assert_bound( mx_error ).

  ENDMETHOD.


  METHOD then_no_error_is_raised.

    cl_abap_unit_assert=>assert_not_bound( mx_error ).

  ENDMETHOD.


  METHOD then_popup_is_shown.

    cl_abap_unit_assert=>assert_true( mo_popups_mock->was_create_package_popup_shown( )  ).

  ENDMETHOD.


  METHOD given_user_cancels_popup.

    mo_popups_mock->set_user_decision( ltcl_popups_mock=>c_user_decision-cancel ).

  ENDMETHOD.


  METHOD then_no_package_is_created.

    cl_abap_unit_assert=>assert_false( mo_sap_package_mock->was_create_called( ) ).

  ENDMETHOD.


  METHOD given_user_confirms_popup.

    mo_popups_mock->set_user_decision( ltcl_popups_mock=>c_user_decision-confirm ).

  ENDMETHOD.


  METHOD then_package_is_created.

    cl_abap_unit_assert=>assert_true( mo_sap_package_mock->was_create_called( ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = mv_package
        act = mv_created_package ).

  ENDMETHOD.


  METHOD given_no_package.

    CLEAR: mv_package.

  ENDMETHOD.


  METHOD given_package_by_user.

    mo_popups_mock->set_package( ltcl_sap_package_mock=>c_package-user ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_sap_package_mock IMPLEMENTATION.

  METHOD constructor.

    mv_package = iv_package.

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~read_responsible.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~validate_name.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~read_description.

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~are_changes_recorded_in_tr_req.

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~create.

    ms_called-create = abap_true.

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

  METHOD zif_abapgit_sap_package~get_transport_layer.

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~list_subpackages.

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~list_superpackages.

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~read_parent.

  ENDMETHOD.


  METHOD was_create_called.

    rv_create_called = ms_called-create.

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_popups_mock IMPLEMENTATION.

  METHOD zif_abapgit_popups~branch_list_popup.

  ENDMETHOD.

  METHOD zif_abapgit_popups~choose_pr_popup.

  ENDMETHOD.

  METHOD zif_abapgit_popups~commit_list_popup.

  ENDMETHOD.

  METHOD zif_abapgit_popups~create_branch_popup.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_folder_logic.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_search_help.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_confirm.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_create_package.

    ms_called-popup_to_create_package = abap_true.

    CASE mv_user_decision.
      WHEN c_user_decision-cancel.
        ev_create = abap_false.
      WHEN c_user_decision-confirm.
        ev_create = abap_true.
      WHEN OTHERS.
        cl_abap_unit_assert=>fail( ).
    ENDCASE.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_create_transp_branch.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_select_from_list.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_select_transports.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_transport_request.

  ENDMETHOD.

  METHOD was_create_package_popup_shown.

    rv_popup_shown = ms_called-popup_to_create_package.

  ENDMETHOD.

  METHOD set_user_decision.

    mv_user_decision = iv_user_decision.

  ENDMETHOD.

  METHOD set_package.

    mv_package = iv_package.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_select_tr_requests.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_select_wb_tc_tr_and_tsk.

  ENDMETHOD.

  METHOD zif_abapgit_popups~tag_list_popup.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_select_labels.

  ENDMETHOD.

  METHOD zif_abapgit_popups~choose_code_insp_check_variant.

  ENDMETHOD.

ENDCLASS.
