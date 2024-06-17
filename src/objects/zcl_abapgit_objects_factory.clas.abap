CLASS zcl_abapgit_objects_factory DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_objects_injector .

  PUBLIC SECTION.
    CLASS-METHODS get_gui_jumper
      RETURNING
        VALUE(ri_gui_jumper) TYPE REF TO zif_abapgit_gui_jumper .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gi_gui_jumper TYPE REF TO zif_abapgit_gui_jumper .
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECTS_FACTORY IMPLEMENTATION.


  METHOD get_gui_jumper.

    IF gi_gui_jumper IS INITIAL.
      CREATE OBJECT gi_gui_jumper TYPE zcl_abapgit_gui_jumper.
    ENDIF.

    ri_gui_jumper = gi_gui_jumper.

  ENDMETHOD.
ENDCLASS.
