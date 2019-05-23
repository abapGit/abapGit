CLASS lcl_renderable DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_gui_renderable.
ENDCLASS.
CLASS lcl_renderable IMPLEMENTATION.
  METHOD zif_abapgit_gui_renderable~render.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_handler DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_gui_event_handler.
ENDCLASS.
CLASS lcl_handler IMPLEMENTATION.
  METHOD zif_abapgit_gui_event_handler~on_event.
  ENDMETHOD.
ENDCLASS.


CLASS ltcl_gui_utils DEFINITION FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS is_renderable FOR TESTING.
    METHODS is_event_handler FOR TESTING.

ENDCLASS.

CLASS ltcl_gui_utils IMPLEMENTATION.

  METHOD is_renderable.

    DATA lo_handler TYPE REF TO lcl_handler.
    DATA lo_renderable TYPE REF TO lcl_renderable.

    CREATE OBJECT lo_handler.
    CREATE OBJECT lo_renderable.

    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = zcl_abapgit_gui_utils=>is_renderable( lo_renderable ) ).
    cl_abap_unit_assert=>assert_equals( exp = abap_false
                                        act = zcl_abapgit_gui_utils=>is_renderable( lo_handler ) ).

  ENDMETHOD.

  METHOD is_event_handler.

    DATA lo_handler TYPE REF TO lcl_handler.
    DATA lo_renderable TYPE REF TO lcl_renderable.

    CREATE OBJECT lo_handler.
    CREATE OBJECT lo_renderable.

    cl_abap_unit_assert=>assert_equals( exp = abap_false
                                        act = zcl_abapgit_gui_utils=>is_event_handler( lo_renderable ) ).
    cl_abap_unit_assert=>assert_equals( exp = abap_true
                                        act = zcl_abapgit_gui_utils=>is_event_handler( lo_handler ) ).

  ENDMETHOD.

ENDCLASS.
