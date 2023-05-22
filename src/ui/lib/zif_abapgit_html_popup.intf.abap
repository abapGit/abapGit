INTERFACE zif_abapgit_html_popup
  PUBLIC .

  METHODS create_picklist
    RETURNING
      VALUE(ro_picklist) TYPE REF TO zcl_abapgit_gui_picklist
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
