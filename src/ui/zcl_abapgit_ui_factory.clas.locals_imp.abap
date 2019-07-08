CLASS lcl_gui_error_handler DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_gui_error_handler.
ENDCLASS.

CLASS lcl_gui_error_handler IMPLEMENTATION.

  METHOD zif_abapgit_gui_error_handler~handle_error.
    DATA: lv_handled TYPE abap_bool.
    ROLLBACK WORK.
    lv_handled = ii_page->show_error( ix_error ).

    IF lv_handled = abap_false.
      MESSAGE ix_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
