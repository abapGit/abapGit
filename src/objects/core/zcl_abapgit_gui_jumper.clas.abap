CLASS zcl_abapgit_gui_jumper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_jumper.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS jump_tr
      IMPORTING
        !is_item       TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.

    METHODS jump_wb
      IMPORTING
        !is_item       TYPE zif_abapgit_definitions=>ty_item
        !iv_new_window TYPE abap_bool
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.

    METHODS jump_wb_line
      IMPORTING
        !is_item         TYPE zif_abapgit_definitions=>ty_item
        !iv_sub_obj_name TYPE zif_abapgit_definitions=>ty_item-obj_name
        !iv_sub_obj_type TYPE zif_abapgit_definitions=>ty_item-obj_type
        !iv_line_number  TYPE i
        !iv_new_window   TYPE abap_bool
      RETURNING
        VALUE(rv_exit)   TYPE abap_bool.

ENDCLASS.



CLASS zcl_abapgit_gui_jumper IMPLEMENTATION.


  METHOD jump_tr.

    DATA:
      lv_e071_object   TYPE e071-object,
      lv_e071_obj_name TYPE e071-obj_name.

    lv_e071_object   = is_item-obj_type.
    lv_e071_obj_name = is_item-obj_name.

    CALL FUNCTION 'TR_OBJECT_JUMP_TO_TOOL'
      EXPORTING
        iv_action         = 'SHOW'
        iv_pgmid          = 'R3TR'
        iv_object         = lv_e071_object
        iv_obj_name       = lv_e071_obj_name
      EXCEPTIONS
        jump_not_possible = 1
        OTHERS            = 2.

    rv_exit = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD jump_wb.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = is_item-obj_name
        object_type         = is_item-obj_type
        devclass            = is_item-devclass
        in_new_window       = iv_new_window
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.

    rv_exit = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD jump_wb_line.

    IF iv_line_number IS NOT INITIAL AND iv_sub_obj_type IS NOT INITIAL AND iv_sub_obj_name IS NOT INITIAL.

      " For the line navigation we have to supply the sub object type (iv_sub_obj_type).
      " If we use is_item-obj_type it navigates only to the object.
      CALL FUNCTION 'RS_TOOL_ACCESS'
        EXPORTING
          operation           = 'SHOW'
          object_name         = is_item-obj_name
          object_type         = iv_sub_obj_type
          devclass            = is_item-devclass
          include             = iv_sub_obj_name
          position            = iv_line_number
          in_new_window       = iv_new_window
        EXCEPTIONS
          not_executed        = 1
          invalid_object_type = 2
          OTHERS              = 3.

      rv_exit = boolc( sy-subrc = 0 ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_jumper~jump.

    " Try all generic jump options

    " 1) ADT Jump
    rv_exit = zif_abapgit_gui_jumper~jump_adt(
      is_item         = is_item
      iv_sub_obj_name = iv_sub_obj_name
      iv_line_number  = iv_line_number ).

    IF rv_exit = abap_true.
      RETURN.
    ENDIF.

    " 2) WB Jump with Line Number
    rv_exit = jump_wb_line(
      is_item         = is_item
      iv_sub_obj_name = iv_sub_obj_name
      iv_sub_obj_type = iv_sub_obj_type
      iv_line_number  = iv_line_number
      iv_new_window   = iv_new_window ).

    IF rv_exit = abap_true.
      RETURN.
    ENDIF.

    " 3) WB Jump without Line Number
    rv_exit = jump_wb(
      is_item       = is_item
      iv_new_window = iv_new_window ).

    IF rv_exit = abap_true.
      RETURN.
    ENDIF.

    " 4) Transport Tool Jump
    rv_exit = jump_tr( is_item ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_jumper~jump_adt.

    " Open object in ADT (if enabled)

    DATA lv_adt_jump_enabled TYPE abap_bool.

    lv_adt_jump_enabled = zcl_abapgit_persist_factory=>get_settings( )->read( )->get_adt_jump_enabled( ).

    IF lv_adt_jump_enabled = abap_true.
      zcl_abapgit_adt_link=>jump(
        iv_obj_name     = is_item-obj_name
        iv_obj_type     = is_item-obj_type
        iv_sub_obj_name = iv_sub_obj_name
        iv_line_number  = iv_line_number ).

      rv_exit = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
