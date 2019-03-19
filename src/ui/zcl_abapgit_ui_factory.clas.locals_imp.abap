CLASS lcl_gui_error_handler DEFINITION.
  PUBLIC SECTION.
    METHODS on_gui_error FOR EVENT on_handle_error OF zcl_abapgit_gui
      IMPORTING
        io_exception.
ENDCLASS.

CLASS lcl_gui_error_handler IMPLEMENTATION.
  METHOD on_gui_error.
    ROLLBACK WORK.
    MESSAGE io_exception TYPE 'S' DISPLAY LIKE 'E'.
  ENDMETHOD.
ENDCLASS.
