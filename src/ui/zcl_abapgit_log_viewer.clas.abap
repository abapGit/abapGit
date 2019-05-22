CLASS zcl_abapgit_log_viewer DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS show_log
      IMPORTING
        iv_header_text TYPE csequence DEFAULT 'Log'
        ii_log         TYPE REF TO zif_abapgit_log.

    CLASS-METHODS to_html
      IMPORTING
        ii_log         TYPE REF TO zif_abapgit_log
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html.

    CLASS-METHODS write_log
      IMPORTING
        ii_log         TYPE REF TO zif_abapgit_log.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_log_out,
        type     TYPE icon_d,
        msg      TYPE string,
        obj_type TYPE trobjtype,
        obj_name TYPE sobj_name,
      END OF ty_log_out .
    TYPES:
      tty_log_out TYPE STANDARD TABLE OF ty_log_out
                                WITH NON-UNIQUE DEFAULT KEY .

    CLASS-METHODS prepare_log_for_display
      IMPORTING
        ii_log     TYPE REF TO zif_abapgit_log
      EXPORTING
        et_log_out TYPE tty_log_out .

ENDCLASS.



CLASS ZCL_ABAPGIT_LOG_VIEWER IMPLEMENTATION.


  METHOD prepare_log_for_display.

    DATA: lt_message TYPE zif_abapgit_log=>tty_log_out,
          lr_message TYPE REF TO zif_abapgit_log=>ty_log_out,
          ls_log     TYPE ty_log_out.

    lt_message = ii_log->get_messages( ).

    LOOP AT lt_message REFERENCE INTO lr_message.

      CLEAR: ls_log.

      ls_log-msg = lr_message->text.

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

      ls_log-obj_type = lr_message->obj_type.
      ls_log-obj_name = lr_message->obj_name.

      INSERT ls_log INTO TABLE et_log_out.

    ENDLOOP.

  ENDMETHOD.


  METHOD show_log.

    DATA: lt_log         TYPE tty_log_out,
          lr_log         TYPE REF TO ty_log_out,
          lo_alv         TYPE REF TO cl_salv_table,
          lx_error       TYPE REF TO cx_salv_error,
          lo_form_header TYPE REF TO cl_salv_form_header_info,
          lo_columns     TYPE REF TO cl_salv_columns_table,
          lo_column      TYPE REF TO cl_salv_column,
          lo_functions   TYPE REF TO cl_salv_functions_list,
          lv_add_obj_col TYPE abap_bool.

    prepare_log_for_display(
      EXPORTING
        ii_log     = ii_log
      IMPORTING
        et_log_out = lt_log ).

    "check if log contains any object info
    LOOP AT lt_log REFERENCE INTO lr_log.
      IF lr_log->obj_type IS NOT INITIAL OR lr_log->obj_name IS NOT INITIAL.
        lv_add_obj_col = abap_true.
      ENDIF.
    ENDLOOP.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = lt_log ).

        lo_functions = lo_alv->get_functions( ).
        lo_functions->set_all( ).

        lo_columns = lo_alv->get_columns( ).

        lo_columns->set_optimize( ).

        lo_column = lo_columns->get_column( |TYPE| ).
        lo_column->set_medium_text( |Type| ).

        lo_column = lo_columns->get_column( |MSG| ).
        lo_column->set_medium_text( |Message| ).

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

        lo_alv->set_screen_popup( start_column = 10
                                  end_column   = 120
                                  start_line   = 4
                                  end_line     = 20 ).

        CREATE OBJECT lo_form_header
          EXPORTING
            text = iv_header_text.

        lo_alv->set_top_of_list( lo_form_header ).

        lo_alv->display( ).

      CATCH cx_salv_error INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD to_html.

    DATA: lt_message TYPE zif_abapgit_log=>tty_log_out,
          lr_message TYPE REF TO zif_abapgit_log=>ty_log_out,
          lv_class   TYPE string,
          lv_icon    TYPE string.

    CREATE OBJECT ro_html.

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

      ro_html->add( |<span class="{ lv_class }">| ).
      ro_html->add_icon( lv_icon ).
      ro_html->add( lr_message->text ).
      ro_html->add( '</span>' ).
    ENDLOOP.

  ENDMETHOD.


  METHOD write_log.

    DATA: lt_message TYPE zif_abapgit_log=>tty_log_out,
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
