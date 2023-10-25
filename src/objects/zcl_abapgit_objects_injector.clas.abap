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



CLASS ZCL_ABAPGIT_OBJECTS_INJECTOR IMPLEMENTATION.


  METHOD set_gui_jumper.

    zcl_abapgit_objects_factory=>gi_gui_jumper = ii_gui_jumper.

  ENDMETHOD.
ENDCLASS.
