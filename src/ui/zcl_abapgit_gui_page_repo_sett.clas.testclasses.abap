CLASS ltcl_requirements DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_requirements.

    METHODS setup.
    METHODS different_fields_are_one_group FOR TESTING RAISING cx_static_check.
    METHODS repeat_field_adds_new_group    FOR TESTING RAISING cx_static_check.
    METHODS check_field_mapping FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_requirements IMPLEMENTATION.

  METHOD setup.
    mo_cut = lcl_requirements=>new( ).
  ENDMETHOD.

  METHOD check_field_mapping.

    DATA ls_actual       TYPE zif_abapgit_dot_abapgit=>ty_requirement.
    DATA ls_expected     TYPE zif_abapgit_dot_abapgit=>ty_requirement.
    DATA lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt.

    mo_cut->set_component( '2' ).
    mo_cut->set_min_release( '3' ).
    mo_cut->set_min_patch( '4' ).

    lt_requirements = mo_cut->get_as_table( ).
    READ TABLE lt_requirements INDEX 1 INTO ls_actual.

    ls_expected-component   = '2'.
    ls_expected-min_release = '3'.
    ls_expected-min_patch   = '4'.

    cl_abap_unit_assert=>assert_equals( act = ls_actual
                                        exp = ls_expected ).
  ENDMETHOD.

  METHOD different_fields_are_one_group.
    DATA lv_lines TYPE i.
    DATA lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt.

    mo_cut->set_component( '1' ).
    mo_cut->set_min_release( '1' ).
    mo_cut->set_min_patch( '1' ).

    lt_requirements = mo_cut->get_as_table( ).
    DESCRIBE TABLE lt_requirements LINES lv_lines.

    cl_abap_unit_assert=>assert_equals( act = lv_lines
                                        exp = 1 ).
  ENDMETHOD.

  METHOD repeat_field_adds_new_group.
    DATA lv_lines TYPE i.
    DATA lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt.

    mo_cut->set_component( '1' ).
    mo_cut->set_min_release( '1' ).
    mo_cut->set_component( '1' ).

    lt_requirements = mo_cut->get_as_table( ).
    DESCRIBE TABLE lt_requirements LINES lv_lines.

    cl_abap_unit_assert=>assert_equals( act = lv_lines
                                        exp = 2 ).
  ENDMETHOD.

ENDCLASS.
