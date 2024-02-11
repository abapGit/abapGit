CLASS lcl_helper DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS get_sap_basis_component
      RETURNING
        VALUE(rs_result) TYPE cvers
      RAISING
        zcx_abapgit_exception.

ENDCLASS.


CLASS lcl_helper IMPLEMENTATION.


  METHOD get_sap_basis_component.

    DATA lt_cvers TYPE zcl_abapgit_repo_requirements=>ty_cvers.

    " mock SAP_BASIS
    rs_result-component  = 'SAP_BASIS'.
    rs_result-release    = '754'.
    rs_result-extrelease = '0007'.
    rs_result-comp_type  = 'S'.

    INSERT rs_result INTO TABLE lt_cvers.

    zcl_abapgit_repo_requirements=>inject_cvers( lt_cvers ).

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
      ls_component    TYPE cvers,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release - 1.
    ls_requirement-min_patch   = 0.
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_repo_requirements=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_yes ).

  ENDMETHOD.


  METHOD lower_patch.

    DATA:
      ls_component    TYPE cvers,
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
        act = zcl_abapgit_repo_requirements=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_yes ).

  ENDMETHOD.


  METHOD same_patch.

    DATA:
      ls_component    TYPE cvers,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release - 1.
    ls_requirement-min_patch   = ls_component-extrelease.
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_repo_requirements=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_yes ).

  ENDMETHOD.


  METHOD higher_patch.

    DATA:
      ls_component    TYPE cvers,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release - 1.
    ls_requirement-min_patch   = ls_component-extrelease + 1.
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_repo_requirements=>is_requirements_met( lt_requirements )
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
      ls_component    TYPE cvers,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release.
    ls_requirement-min_patch   = 0.
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_repo_requirements=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_yes ).

  ENDMETHOD.


  METHOD lower_patch.

    DATA:
      ls_component    TYPE cvers,
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
        act = zcl_abapgit_repo_requirements=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_yes ).

  ENDMETHOD.


  METHOD same_patch.

    DATA:
      ls_component    TYPE cvers,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release.
    ls_requirement-min_patch   = ls_component-extrelease.
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_repo_requirements=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_yes ).

  ENDMETHOD.


  METHOD higher_patch.

    DATA:
      ls_component    TYPE cvers,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release.
    ls_requirement-min_patch   = ls_component-extrelease + 1.
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_repo_requirements=>is_requirements_met( lt_requirements )
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
      ls_component    TYPE cvers,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release + 1.
    ls_requirement-min_patch   = 0.
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_repo_requirements=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_no ).

  ENDMETHOD.


  METHOD lower_patch.

    DATA:
      ls_component    TYPE cvers,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release + 1.
    ls_requirement-min_patch   = ls_component-extrelease - 1.
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_repo_requirements=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_no ).

  ENDMETHOD.


  METHOD same_patch.

    DATA:
      ls_component    TYPE cvers,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release + 1.
    ls_requirement-min_patch   = ls_component-extrelease.
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_repo_requirements=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_no ).

  ENDMETHOD.


  METHOD higher_patch.

    DATA:
      ls_component    TYPE cvers,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release + 1.
    ls_requirement-min_patch   = ls_component-extrelease + 1.
    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_repo_requirements=>is_requirements_met( lt_requirements )
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

    METHODS longer_patch FOR TESTING
      RAISING
        zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_formats IMPLEMENTATION.


  METHOD shorter_patch.

    DATA:
      ls_component    TYPE cvers,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release.
    ls_requirement-min_patch   = ls_component-extrelease. " len 4 to len 10

    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_repo_requirements=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_yes ).

  ENDMETHOD.


  METHOD longer_patch.

    DATA:
      ls_component    TYPE cvers,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      ls_requirement  LIKE LINE OF lt_requirements.

    ls_component = lcl_helper=>get_sap_basis_component( ).

    ls_requirement-component   = ls_component-component.
    ls_requirement-min_release = ls_component-release.
    ls_requirement-min_patch   = '0000000007'. " len 10 instead of len 4

    APPEND ls_requirement TO lt_requirements.

    cl_abap_unit_assert=>assert_equals(
        act = zcl_abapgit_repo_requirements=>is_requirements_met( lt_requirements )
        exp = zif_abapgit_definitions=>c_yes ).

  ENDMETHOD.


ENDCLASS.
