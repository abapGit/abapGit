class ZCL_ABAPGIT_COMPARISON_NULL definition
  public
  final
  create public .

public section.

  interfaces ZIF_ABAPGIT_COMPARISON_RESULT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ABAPGIT_COMPARISON_NULL IMPLEMENTATION.


  METHOD zif_abapgit_comparison_result~is_result_complete_halt.
    rv_response = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_comparison_result~show_confirmation_dialog.
    RETURN.
  ENDMETHOD.
ENDCLASS.
