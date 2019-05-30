CLASS zcl_abapgit_gui_functions DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_gui_functions.

ENDCLASS.



CLASS zcl_abapgit_gui_functions IMPLEMENTATION.

  METHOD zif_abapgit_gui_functions~gui_is_available.

    CALL FUNCTION 'GUI_IS_AVAILABLE'
      IMPORTING
        return = rv_gui_is_available.

  ENDMETHOD.

ENDCLASS.
