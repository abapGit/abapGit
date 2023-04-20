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

    METHODS jump_bw
      IMPORTING
        !is_item       TYPE zif_abapgit_definitions=>ty_item
        !iv_new_window TYPE abap_bool
      RETURNING
        VALUE(rv_exit) TYPE abap_bool.

ENDCLASS.



CLASS zcl_abapgit_gui_jumper IMPLEMENTATION.


  METHOD jump_bw.

    DATA:
      lv_exit  TYPE abap_bool,
      lv_tlogo TYPE c LENGTH 4, "rstlogo
      lv_objnm TYPE c LENGTH 40. "rsawbnobjnm

    lv_tlogo = is_item-obj_type.
    lv_objnm = is_item-obj_name.

    TRY.
        CALL METHOD ('CL_RSAWBN_AWB')=>('IS_SUPPORTED_NAVIGATION')
          EXPORTING
            i_tlogo               = lv_tlogo
            i_fcode               = 'DISPLAY'
          IMPORTING
            re_is_supported_fcode = lv_exit.

        IF lv_exit = abap_false.
          RETURN.
        ENDIF.
      CATCH cx_root.
        " Not a BW system
        RETURN.
    ENDTRY.

    TRY.
        CALL METHOD ('CL_RSAWBN_AWB')=>('NAVIGATE_FROM_APPLICATION')
          EXPORTING
            i_tlogo                = lv_tlogo
            i_objnm                = lv_objnm
            i_new_mode             = iv_new_window
          IMPORTING
            e_exit_own_application = lv_exit.

      CATCH cx_root.
        " Older release without i_new_mode
        CALL METHOD ('CL_RSAWBN_AWB')=>('NAVIGATE_FROM_APPLICATION')
          EXPORTING
            i_tlogo                = lv_tlogo
            i_objnm                = lv_objnm
          IMPORTING
            e_exit_own_application = lv_exit.
    ENDTRY.

    rv_exit = lv_exit.

  ENDMETHOD.


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

    " WebGUI cannot open windows or ADT
    IF zcl_abapgit_ui_factory=>get_frontend_services( )->is_webgui( ) = abap_true.
      zcx_abapgit_exception=>raise( |Jump not possible in WebGUI| ).
    ENDIF.

    " Try all generic jump options

    " 1) ADT Jump
    rv_exit = zif_abapgit_gui_jumper~jump_adt(
      is_item         = is_item
      iv_sub_obj_name = is_sub_item-obj_name
      iv_line_number  = iv_line_number ).

    IF rv_exit = abap_true.
      RETURN.
    ENDIF.

    " 2) WB Jump with Line Number
    rv_exit = jump_wb_line(
      is_item         = is_item
      iv_sub_obj_name = is_sub_item-obj_name
      iv_sub_obj_type = is_sub_item-obj_type
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

    IF rv_exit = abap_true.
      RETURN.
    ENDIF.

    " 5) BW Jump
    rv_exit = jump_bw(
      is_item       = is_item
      iv_new_window = iv_new_window ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_jumper~jump_abapgit.

    DATA lt_spagpa        TYPE STANDARD TABLE OF rfc_spagpa.
    DATA ls_spagpa        LIKE LINE OF lt_spagpa.
    DATA lv_save_sy_langu TYPE sy-langu.
    DATA lv_subrc         TYPE syst-subrc.
    DATA lv_tcode         TYPE tcode.

    " https://blogs.sap.com/2017/01/13/logon-language-sy-langu-and-rfc/

    lv_tcode = zcl_abapgit_services_abapgit=>get_abapgit_tcode( ).

    lv_save_sy_langu = sy-langu.
    SET LOCALE LANGUAGE iv_language.

    ls_spagpa-parid  = zif_abapgit_definitions=>c_spagpa_param_repo_key.
    ls_spagpa-parval = iv_key.
    INSERT ls_spagpa INTO TABLE lt_spagpa.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      DESTINATION 'NONE'
      STARTING NEW TASK 'ABAPGIT'
      EXPORTING
        tcode                   = lv_tcode
      TABLES
        spagpa_tab              = lt_spagpa
      EXCEPTIONS
        call_transaction_denied = 1
        tcode_invalid           = 2
        communication_failure   = 3
        system_failure          = 4
        OTHERS                  = 5.

    lv_subrc = sy-subrc.

    SET LOCALE LANGUAGE lv_save_sy_langu.

    IF lv_subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from ABAP4_CALL_TRANSACTION. Subrc = { lv_subrc }| ).
    ENDIF.

    MESSAGE 'Repository opened in a new window' TYPE 'S'.

  ENDMETHOD.


  METHOD zif_abapgit_gui_jumper~jump_adt.

    " Open object in ADT (if enabled)

    DATA lv_adt_jump_enabled TYPE abap_bool.

    lv_adt_jump_enabled = zcl_abapgit_persist_factory=>get_settings( )->read( )->get_adt_jump_enabled( ).

    IF lv_adt_jump_enabled = abap_true.
      TRY.
          zcl_abapgit_adt_link=>jump(
            iv_obj_name     = is_item-obj_name
            iv_obj_type     = is_item-obj_type
            iv_sub_obj_name = iv_sub_obj_name
            iv_line_number  = iv_line_number ).

          rv_exit = abap_true.
        CATCH zcx_abapgit_exception ##NO_HANDLER.
          " Use fallback
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_jumper~jump_batch_input.

    DATA lv_msg TYPE c LENGTH 80.

    IF iv_new_window = abap_true.
      CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
        STARTING NEW TASK 'GIT'
        EXPORTING
          tcode                 = iv_tcode
          mode_val              = 'E'
        TABLES
          using_tab             = it_bdcdata
        EXCEPTIONS
          system_failure        = 1 MESSAGE lv_msg
          communication_failure = 2 MESSAGE lv_msg
          resource_failure      = 3
          OTHERS                = 4.
    ELSE.
      CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
        EXPORTING
          tcode     = iv_tcode
          mode_val  = 'E'
        TABLES
          using_tab = it_bdcdata
        EXCEPTIONS
          OTHERS    = 4.
    ENDIF.

    CASE sy-subrc.
      WHEN 1 OR 2.
        zcx_abapgit_exception=>raise( |Batch input error for transaction { iv_tcode }: { lv_msg }| ).
      WHEN 3 OR 4.
        zcx_abapgit_exception=>raise( |Batch input error for transaction { iv_tcode }| ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
