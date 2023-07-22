CLASS zcl_abapgit_objects_injector DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS set_gui_jumper
      IMPORTING
        !ii_gui_jumper TYPE REF TO zif_abapgit_gui_jumper .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_objects_injector IMPLEMENTATION.

  METHOD set_gui_jumper.

    zcl_abapgit_ui_factory=>gi_gui_jumper = ii_gui_jumper.

  ENDMETHOD.

ENDCLASS.
