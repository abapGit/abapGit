CLASS lcl_helper DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS get_sap_basis_component
      RETURNING
        VALUE(rs_result) TYPE cvers_sdu
      RAISING
        zcx_abapgit_exception.

ENDCLASS.


CLASS lcl_helper IMPLEMENTATION.


  METHOD get_sap_basis_component.

    DATA:
      lt_installed TYPE STANDARD TABLE OF cvers_sdu.

    CALL FUNCTION 'DELIVERY_GET_INSTALLED_COMPS'
      TABLES
        tt_comptab       = lt_installed
      EXCEPTIONS
        no_release_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from DELIVERY_GET_INSTALLED_COMPS { sy-subrc }| ).
    ENDIF.

    READ TABLE lt_installed INTO rs_result
      WITH KEY component = `SAP_BASIS`.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Component SAP_BASIS not found| ).
    ENDIF.

  ENDMETHOD.


ENDCLASS.



CLASS ltcl_lower_release DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PUBLIC SECTION.

  PRIVATE SECTION.
    METHODS empty_patch FOR TESTING
      RAISING
        zcx_abapgit_exception.

    METHODS lower_patch FOR TESTING
      RAISING
        zcx_abapgit_exception.

    METHODS same_patch FOR TESTING
      RAISING
        zcx_abapgit_exception.

    METHODS higher_patch FOR TESTING
      RAISING
        zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_lower_release IMPLEMENTATION.


  METHOD empty_patch.

    DATA:
      ls_component    TYPE cvers_sdu,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release - 1.
    ls_requirement-min_patch   = 0.
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_requirement_helper=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_yes ).

  ENDMETHOD.


  METHOD lower_patch.

    DATA:
      ls_component    TYPE cvers_sdu,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release - 1.
    ls_requirement-min_patch   = nmax(
                                   val1 = ls_component-extrelease - 1
                                   val2 = 0 ).
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_requirement_helper=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_yes ).

  ENDMETHOD.


  METHOD same_patch.

    DATA:
      ls_component    TYPE cvers_sdu,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release - 1.
    ls_requirement-min_patch   = ls_component-extrelease.
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_requirement_helper=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_yes ).

  ENDMETHOD.


  METHOD higher_patch.

    DATA:
      ls_component    TYPE cvers_sdu,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release - 1.
    ls_requirement-min_patch   = ls_component-extrelease + 1.
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_requirement_helper=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_yes ).

  ENDMETHOD.


ENDCLASS.



CLASS ltcl_same_release DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PUBLIC SECTION.

  PRIVATE SECTION.
    METHODS empty_patch FOR TESTING
      RAISING
        zcx_abapgit_exception.

    METHODS lower_patch FOR TESTING
      RAISING
        zcx_abapgit_exception.

    METHODS same_patch FOR TESTING
      RAISING
        zcx_abapgit_exception.

    METHODS higher_patch FOR TESTING
      RAISING
        zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_same_release IMPLEMENTATION.


  METHOD empty_patch.

    DATA:
      ls_component    TYPE cvers_sdu,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release.
    ls_requirement-min_patch   = 0.
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_requirement_helper=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_yes ).

  ENDMETHOD.


  METHOD lower_patch.

    DATA:
      ls_component    TYPE cvers_sdu,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release.
    ls_requirement-min_patch   = nmax(
                                   val1 = ls_component-extrelease - 1
                                   val2 = 0 ).
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_requirement_helper=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_yes ).

  ENDMETHOD.


  METHOD same_patch.

    DATA:
      ls_component    TYPE cvers_sdu,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release.
    ls_requirement-min_patch   = ls_component-extrelease.
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_requirement_helper=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_yes ).

  ENDMETHOD.


  METHOD higher_patch.

    DATA:
      ls_component    TYPE cvers_sdu,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release.
    ls_requirement-min_patch   = ls_component-extrelease + 1.
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_requirement_helper=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_no ).

  ENDMETHOD.


ENDCLASS.



CLASS ltcl_higher_release DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PUBLIC SECTION.

  PRIVATE SECTION.
    METHODS empty_patch FOR TESTING
      RAISING
        zcx_abapgit_exception.

    METHODS lower_patch FOR TESTING
      RAISING
        zcx_abapgit_exception.

    METHODS same_patch FOR TESTING
      RAISING
        zcx_abapgit_exception.

    METHODS higher_patch FOR TESTING
      RAISING
        zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_higher_release IMPLEMENTATION.


  METHOD empty_patch.

    DATA:
      ls_component    TYPE cvers_sdu,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release + 1.
    ls_requirement-min_patch   = 0.
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_requirement_helper=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_no ).

  ENDMETHOD.


  METHOD lower_patch.

    DATA:
      ls_component    TYPE cvers_sdu,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release + 1.
    ls_requirement-min_patch   = ls_component-extrelease - 1.
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_requirement_helper=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_no ).

  ENDMETHOD.


  METHOD same_patch.

    DATA:
      ls_component    TYPE cvers_sdu,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release + 1.
    ls_requirement-min_patch   = ls_component-extrelease.
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_requirement_helper=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_no ).

  ENDMETHOD.


  METHOD higher_patch.

    DATA:
      ls_component    TYPE cvers_sdu,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release + 1.
    ls_requirement-min_patch   = ls_component-extrelease + 1.
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_requirement_helper=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_no ).

  ENDMETHOD.


ENDCLASS.



CLASS ltcl_formats DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PUBLIC SECTION.

  PRIVATE SECTION.
    METHODS shorter_patch FOR TESTING
      RAISING
        zcx_abapgit_exception.

    METHODS higher_patch FOR TESTING
      RAISING
        zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_formats IMPLEMENTATION.


  METHOD shorter_patch.

    DATA:
      ls_component    TYPE cvers_sdu,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = ls_component-extrelease
      IMPORTING
        output = ls_requirement-min_patch.

    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_requirement_helper=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_yes ).

  ENDMETHOD.


  METHOD higher_patch.

    DATA:
      ls_component    TYPE cvers_sdu,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_component-extrelease
      IMPORTING
        output = ls_requirement-min_patch.

    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_requirement_helper=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_yes ).

  ENDMETHOD.


ENDCLASS.
