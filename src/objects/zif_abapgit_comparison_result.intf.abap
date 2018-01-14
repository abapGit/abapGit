INTERFACE zif_abapgit_comparison_result PUBLIC.

  METHODS:
    show_confirmation_dialog,
    is_result_complete_halt
      RETURNING VALUE(rv_response) TYPE abap_bool.

ENDINTERFACE.
