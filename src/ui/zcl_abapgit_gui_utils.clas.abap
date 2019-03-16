class ZCL_ABAPGIT_GUI_UTILS definition
  public
  final
  create public .

public section.

  class-methods IS_RENDERABLE
    importing
      !IO_OBJ type ref to OBJECT
    returning
      value(RV_YES) type ABAP_BOOL .
  class-methods IS_EVENT_HANDLER
    importing
      !IO_OBJ type ref to OBJECT
    returning
      value(RV_YES) type ABAP_BOOL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_UTILS IMPLEMENTATION.


  METHOD is_event_handler.
    DATA li_event_handler TYPE REF TO zif_abapgit_gui_event_handler.
    TRY.
        li_event_handler ?= io_obj.
        rv_yes = abap_true.
      CATCH cx_sy_move_cast_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD is_renderable.
    DATA li_renderable TYPE REF TO zif_abapgit_gui_renderable.
    TRY.
        li_renderable ?= io_obj.
        rv_yes = abap_true.
      CATCH cx_sy_move_cast_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
