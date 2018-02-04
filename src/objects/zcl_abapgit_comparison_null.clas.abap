CLASS zcl_abapgit_comparison_null DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_comparison_result .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_COMPARISON_NULL IMPLEMENTATION.


  METHOD zif_abapgit_comparison_result~is_result_complete_halt.
    rv_response = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_comparison_result~show_confirmation_dialog.
    RETURN.
  ENDMETHOD.
ENDCLASS.
