CLASS zcl_abapgit_log_viewer DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS show_log
      IMPORTING
        !ii_log TYPE REF TO zif_abapgit_log .
    CLASS-METHODS to_html
      IMPORTING
        !ii_log        TYPE REF TO zif_abapgit_log
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    CLASS-METHODS write_log
      IMPORTING
        !ii_log TYPE REF TO zif_abapgit_log .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_log_out,
        type      TYPE icon_d,
        msg       TYPE string,
        obj_type  TYPE trobjtype,
        obj_name  TYPE sobj_name,
        exception TYPE REF TO cx_root,
        longtext  TYPE icon_d,
        t100      TYPE icon_d,
        source    TYPE icon_d,
        callstack TYPE icon_d,
        cell_type TYPE salv_t_int4_column,
      END OF ty_log_out.
    TYPES:
      ty_log_outs TYPE STANDARD TABLE OF ty_log_out
                                WITH NON-UNIQUE DEFAULT KEY.

    CLASS-METHODS:
      prepare_log_for_display
        IMPORTING
          ii_log            TYPE REF TO zif_abapgit_log
        RETURNING
          VALUE(rt_log_out) TYPE ty_log_outs,

      show_longtext
        IMPORTING
          is_log TYPE ty_log_out
        RAISING
          zcx_abapgit_exception,

      goto_source
        IMPORTING
          is_log TYPE ty_log_out
        RAISING
          zcx_abapgit_exception,

      goto_callstack
        IMPORTING
          is_log TYPE ty_log_out
        RAISING
          zcx_abapgit_exception,

      goto_t100_message
        IMPORTING
          is_log TYPE ty_log_out
        RAISING
          zcx_abapgit_exception,

      on_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column,

      dispatch
        IMPORTING
          is_log    TYPE ty_log_out
          iv_column TYPE salv_de_column
        RAISING
          zcx_abapgit_exception,

      calculate_cell_type,

      get_exception_viewer
        IMPORTING
          is_log                     TYPE ty_log_out
        RETURNING
          VALUE(ro_exception_viewer) TYPE REF TO zcl_abapgit_exception_viewer.

    CLASS-DATA:
      gt_log TYPE ty_log_outs.

ENDCLASS.



CLASS ZCL_ABAPGIT_LOG_VIEWER IMPLEMENTATION.


  METHOD calculate_cell_type.

    FIELD-SYMBOLS: <ls_log> LIKE LINE OF gt_log.
    DATA: ls_cell_type LIKE LINE OF <ls_log>-cell_type.

    LOOP AT gt_log ASSIGNING <ls_log>.

      IF <ls_log>-longtext IS NOT INITIAL.
        ls_cell_type-columnname = `LONGTEXT`.
        ls_cell_type-value      = if_salv_c_cell_type=>hotspot.
        INSERT ls_cell_type INTO TABLE <ls_log>-cell_type.
      ENDIF.

      IF <ls_log>-t100 IS NOT INITIAL.
        ls_cell_type-columnname = `T100`.
        ls_cell_type-value      = if_salv_c_cell_type=>hotspot.
        INSERT ls_cell_type INTO TABLE <ls_log>-cell_type.
      ENDIF.

      IF <ls_log>-source IS NOT INITIAL.
        ls_cell_type-columnname = `SOURCE`.
        ls_cell_type-value      = if_salv_c_cell_type=>hotspot.
        INSERT ls_cell_type INTO TABLE <ls_log>-cell_type.
      ENDIF.

      IF <ls_log>-callstack IS NOT INITIAL.
        ls_cell_type-columnname = `CALLSTACK`.
        ls_cell_type-value      = if_salv_c_cell_type=>hotspot.
        INSERT ls_cell_type INTO TABLE <ls_log>-cell_type.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD dispatch.

    CASE iv_column.
      WHEN `LONGTEXT`.

        show_longtext( is_log ).

      WHEN `T100`.

        goto_t100_message( is_log ).

      WHEN `SOURCE`.

        goto_source( is_log ).

      WHEN `CALLSTACK`.

        goto_callstack( is_log ).

    ENDCASE.

  ENDMETHOD.


  METHOD get_exception_viewer.

    DATA:
      lx_abapgit TYPE REF TO zcx_abapgit_exception.

    ASSERT is_log-exception IS BOUND.
    lx_abapgit ?= is_log-exception.

    CREATE OBJECT ro_exception_viewer
      EXPORTING
        ix_error = lx_abapgit.

  ENDMETHOD.


  METHOD goto_callstack.

    get_exception_viewer( is_log )->show_callstack( ).

  ENDMETHOD.


  METHOD goto_source.

    get_exception_viewer( is_log )->goto_source( ).

  ENDMETHOD.


  METHOD goto_t100_message.

    get_exception_viewer( is_log )->goto_message( ).

  ENDMETHOD.


  METHOD on_link_click.

    DATA: lx_error TYPE REF TO zcx_abapgit_exception.
    FIELD-SYMBOLS: <ls_log> TYPE ty_log_out.

    IF row IS INITIAL
    OR column IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE gt_log ASSIGNING <ls_log>
                      INDEX row.
    ASSERT sy-subrc = 0.

    TRY.
        dispatch(
            is_log    = <ls_log>
            iv_column = column ).

      CATCH zcx_abapgit_exception INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD prepare_log_for_display.

    DATA: lt_message      TYPE zif_abapgit_log=>ty_log_outs,
          lr_message      TYPE REF TO zif_abapgit_log=>ty_log_out,
          ls_log          TYPE ty_log_out,
          li_t100_message TYPE REF TO if_t100_message,
          lx_abapgit      TYPE REF TO zcx_abapgit_exception.

    lt_message = ii_log->get_messages( ).

    LOOP AT lt_message REFERENCE INTO lr_message.

      CLEAR: ls_log.

      ls_log-msg = lr_message->text.
      ls_log-exception = lr_message->exception.

      CASE lr_message->type.
        WHEN 'E' OR 'A' OR 'X'.
          ls_log-type = icon_led_red.
        WHEN 'W'.
          ls_log-type = icon_led_yellow.
        WHEN 'I' OR 'S'.
          ls_log-type = icon_led_green.
        WHEN OTHERS.
          ls_log-type = icon_led_inactive.
      ENDCASE.

      IF lr_message->exception IS BOUND.

        TRY.
            li_t100_message ?= lr_message->exception.

            IF li_t100_message->t100key IS NOT INITIAL.
              ls_log-t100 = icon_message_information.
            ENDIF.

          CATCH cx_sy_move_cast_error ##NO_HANDLER.
        ENDTRY.

        TRY.
            lx_abapgit ?= lr_message->exception.

            IF lx_abapgit->mt_callstack IS NOT INITIAL.
              ls_log-longtext  = icon_system_help.
              ls_log-callstack = icon_stack.
              ls_log-source    = icon_abap.
            ENDIF.

          CATCH cx_sy_move_cast_error ##NO_HANDLER.
        ENDTRY.

      ENDIF.

      ls_log-obj_type = lr_message->obj_type.
      ls_log-obj_name = lr_message->obj_name.

      INSERT ls_log INTO TABLE rt_log_out.

    ENDLOOP.

  ENDMETHOD.


  METHOD show_log.

    DATA: lr_log         TYPE REF TO ty_log_out,
          lo_alv         TYPE REF TO cl_salv_table,
          lx_error       TYPE REF TO cx_salv_error,
          lo_form_header TYPE REF TO cl_salv_form_header_info,
          lo_columns     TYPE REF TO cl_salv_columns_table,
          lo_column      TYPE REF TO cl_salv_column,
          lo_functions   TYPE REF TO cl_salv_functions_list,
          ls_position    TYPE zif_abapgit_popups=>ty_popup_position,
          lv_add_obj_col TYPE abap_bool,
          lo_event       TYPE REF TO cl_salv_events_table.

    gt_log = prepare_log_for_display( ii_log ).

    "check if log contains any object info
    LOOP AT gt_log REFERENCE INTO lr_log.
      IF lr_log->obj_type IS NOT INITIAL OR lr_log->obj_name IS NOT INITIAL.
        lv_add_obj_col = abap_true.
      ENDIF.
    ENDLOOP.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = gt_log ).

        lo_functions = lo_alv->get_functions( ).
        lo_functions->set_all( ).

        lo_alv->get_display_settings( )->set_list_header( |abapGit Log Viewer| ).

        lo_columns = lo_alv->get_columns( ).

        lo_columns->set_optimize( ).
        lo_columns->set_cell_type_column( |CELL_TYPE| ).

        calculate_cell_type( ).

        lo_column = lo_columns->get_column( |TYPE| ).
        lo_column->set_medium_text( |Type| ).

        lo_column = lo_columns->get_column( |MSG| ).
        lo_column->set_medium_text( |Message| ).

        lo_column = lo_columns->get_column( |LONGTEXT| ).
        lo_column->set_medium_text( |Longtext| ).

        lo_column = lo_columns->get_column( |T100| ).
        lo_column->set_medium_text( |Goto message| ).

        lo_column = lo_columns->get_column( |SOURCE| ).
        lo_column->set_medium_text( |Goto source| ).

        lo_column = lo_columns->get_column( |CALLSTACK| ).
        lo_column->set_medium_text( |Show callstack| ).

        IF lv_add_obj_col = abap_true.
          lo_column = lo_columns->get_column( |OBJ_TYPE| ).
          lo_column->set_medium_text( |Object Type| ).

          lo_column = lo_columns->get_column( |OBJ_NAME| ).
          lo_column->set_medium_text( |Object Name| ).
        ELSE.
          "hide object columns
          lo_column = lo_columns->get_column( |OBJ_TYPE| ).
          lo_column->set_technical( abap_true ).

          lo_column = lo_columns->get_column( |OBJ_NAME| ).
          lo_column->set_technical( abap_true ).
        ENDIF.

        "hide empty columns
        LOOP AT gt_log TRANSPORTING NO FIELDS WHERE t100 IS NOT INITIAL.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          lo_column = lo_columns->get_column( |T100| ).
          lo_column->set_technical( abap_true ).
        ENDIF.

        LOOP AT gt_log TRANSPORTING NO FIELDS WHERE source IS NOT INITIAL.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          lo_column = lo_columns->get_column( |SOURCE| ).
          lo_column->set_technical( abap_true ).
        ENDIF.

        LOOP AT gt_log TRANSPORTING NO FIELDS WHERE longtext IS NOT INITIAL.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          lo_column = lo_columns->get_column( |LONGTEXT| ).
          lo_column->set_technical( abap_true ).
        ENDIF.

        LOOP AT gt_log TRANSPORTING NO FIELDS WHERE callstack IS NOT INITIAL.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          lo_column = lo_columns->get_column( |CALLSTACK| ).
          lo_column->set_technical( abap_true ).
        ENDIF.

        ls_position = zcl_abapgit_popups=>center(
          iv_width  = 125
          iv_height = 20 ).

        lo_alv->set_screen_popup( start_column = ls_position-start_column
                                  end_column   = ls_position-end_column
                                  start_line   = ls_position-start_row
                                  end_line     = ls_position-end_row ).

        CREATE OBJECT lo_form_header
          EXPORTING
            text = ii_log->get_title( ).

        lo_alv->set_top_of_list( lo_form_header ).

        lo_event = lo_alv->get_event( ).

        SET HANDLER on_link_click FOR lo_event.

        lo_alv->display( ).

      CATCH cx_salv_error INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD show_longtext.

    DATA: lx_abapgit TYPE REF TO zcx_abapgit_exception.

    DATA: lv_docu_object TYPE dokhl-object,
          lt_dummy1      TYPE TABLE OF dselc,
          lt_dummy2      TYPE TABLE OF dval,
          ls_help_info   TYPE help_info.

    IF is_log-exception IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        lx_abapgit ?= is_log-exception.
      CATCH cx_sy_move_cast_error.
        RETURN.
    ENDTRY.

    lv_docu_object   = lx_abapgit->if_t100_message~t100key-msgid.
    lv_docu_object+2 = lx_abapgit->if_t100_message~t100key-msgno.

    ls_help_info-call       = 'D'.
    ls_help_info-spras      = sy-langu.
    ls_help_info-messageid  = lx_abapgit->if_t100_message~t100key-msgid.
    ls_help_info-messagenr  = lx_abapgit->if_t100_message~t100key-msgno.
    ls_help_info-message    = is_log-msg.
    ls_help_info-title      = 'Longtext'.
    ls_help_info-docuid     = 'NA'.
    ls_help_info-docuobject = lv_docu_object.
    ls_help_info-msgv1      = lx_abapgit->msgv1.
    ls_help_info-msgv2      = lx_abapgit->msgv2.
    ls_help_info-msgv3      = lx_abapgit->msgv3.
    ls_help_info-msgv4      = lx_abapgit->msgv4.

    CALL FUNCTION 'HELP_START'
      EXPORTING
        help_infos   = ls_help_info
      TABLES
        dynpselect   = lt_dummy1
        dynpvaluetab = lt_dummy2
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc IS NOT INITIAL.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD to_html.

    DATA: lt_message TYPE zif_abapgit_log=>ty_log_outs,
          lr_message TYPE REF TO zif_abapgit_log=>ty_log_out,
          lv_class   TYPE string,
          lv_icon    TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF ii_log->count( ) = 0.
      RETURN.
    ENDIF.

    lt_message = ii_log->get_messages( ).

    LOOP AT lt_message REFERENCE INTO lr_message.
      CASE lr_message->type.
        WHEN 'W'.
          lv_icon  = 'attention'.
          lv_class = 'warning'.
        WHEN 'E'.
          lv_icon  = 'error'.
          lv_class = 'error'.
        WHEN OTHERS. " ??? unexpected
          lv_icon  = 'error'.
          lv_class = 'error'.
      ENDCASE.

      ri_html->add( |<span class="{ lv_class }">| ).
      ri_html->add_icon( lv_icon ).
      ri_html->add( lr_message->text ).
      ri_html->add( '</span>' ).
    ENDLOOP.

  ENDMETHOD.


  METHOD write_log.

    DATA: lt_message TYPE zif_abapgit_log=>ty_log_outs,
          lr_message TYPE REF TO zif_abapgit_log=>ty_log_out,
          lv_text    TYPE string.

    lt_message = ii_log->get_messages( ).

    LOOP AT lt_message REFERENCE INTO lr_message.
      IF lr_message->obj_name IS NOT INITIAL AND lr_message->obj_type IS NOT INITIAL.
        lv_text = |{ lr_message->type }: { lr_message->text } ({ lr_message->obj_type }/{ lr_message->obj_name })|.
      ELSE.
        lv_text = |{ lr_message->type }: { lr_message->text }|.
      ENDIF.
      WRITE: / lv_text.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
