CLASS lcl_requirements DEFINITION CREATE PRIVATE.

  "This assumes grouping, any duplicate field will trigger a new record.
  "Not perfect, but a little better than assuming a sequence

  PUBLIC SECTION.
    CLASS-METHODS new
      RETURNING VALUE(ro_result) TYPE REF TO lcl_requirements.

    METHODS set_component   IMPORTING iv_component   TYPE string.
    METHODS set_min_release IMPORTING iv_min_release TYPE string.
    METHODS set_min_patch   IMPORTING iv_min_patch   TYPE string.

    METHODS get_as_table
      RETURNING VALUE(rt_requirements) TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt.

  PRIVATE SECTION.
    DATA: ms_requirement  TYPE zif_abapgit_dot_abapgit=>ty_requirement,
          mt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt.

ENDCLASS.

CLASS lcl_requirements IMPLEMENTATION.

  METHOD new.
    CREATE OBJECT ro_result.
  ENDMETHOD.

  METHOD set_component.

    IF ms_requirement-component IS NOT INITIAL.
      APPEND ms_requirement TO mt_requirements.
      CLEAR ms_requirement.
    ENDIF.
    ms_requirement-component = iv_component.

  ENDMETHOD.

  METHOD set_min_patch.

    IF ms_requirement-min_patch IS NOT INITIAL.
      APPEND ms_requirement TO mt_requirements.
      CLEAR ms_requirement.
    ENDIF.
    ms_requirement-min_patch = iv_min_patch.

  ENDMETHOD.

  METHOD set_min_release.

    IF ms_requirement-min_release IS NOT INITIAL.
      APPEND ms_requirement TO mt_requirements.
      CLEAR ms_requirement.
    ENDIF.
    ms_requirement-min_release = iv_min_release.

  ENDMETHOD.

  METHOD get_as_table.

    IF ms_requirement IS NOT INITIAL.
      APPEND ms_requirement TO mt_requirements.
    ENDIF.

    SORT mt_requirements BY component min_release min_patch.
    DELETE mt_requirements WHERE component IS INITIAL.
    DELETE ADJACENT DUPLICATES FROM mt_requirements COMPARING ALL FIELDS.

    rt_requirements = mt_requirements.

  ENDMETHOD.

ENDCLASS.
