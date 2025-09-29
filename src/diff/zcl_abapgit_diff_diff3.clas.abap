CLASS zcl_abapgit_diff_diff3 DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS compute
      IMPORTING
        iv_new                TYPE xstring
        iv_old                TYPE xstring
        iv_ignore_indentation TYPE abap_bool DEFAULT abap_false
        iv_ignore_comments    TYPE abap_bool DEFAULT abap_false
        iv_ignore_case        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_diff)        TYPE zif_abapgit_definitions=>ty_diffs_tt
      RAISING
        zcx_abapgit_exception.
ENDCLASS.

CLASS zcl_abapgit_diff_diff3 IMPLEMENTATION.
  METHOD compute.
    CLEAR rt_diff.
  ENDMETHOD.

ENDCLASS.
