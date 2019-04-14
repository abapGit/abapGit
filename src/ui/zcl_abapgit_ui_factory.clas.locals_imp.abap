CLASS lcl_gui_error_handler DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_gui_error_handler.
ENDCLASS.

CLASS lcl_gui_error_handler IMPLEMENTATION.

  METHOD zif_abapgit_gui_error_handler~handle_error.
    ROLLBACK WORK.
    MESSAGE ix_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDMETHOD.

ENDCLASS.
