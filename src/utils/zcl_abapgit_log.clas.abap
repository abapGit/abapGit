CLASS zcl_abapgit_log DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS add
      IMPORTING
        !iv_msg  TYPE csequence
        !iv_type TYPE symsgty DEFAULT 'E'
        !iv_rc   TYPE balsort OPTIONAL .
    METHODS add_error
      IMPORTING
        !iv_msg TYPE csequence .
    METHODS add_info
      IMPORTING
        !iv_msg TYPE csequence .
    METHODS add_warning
      IMPORTING
        !iv_msg TYPE csequence .
    METHODS clear .
    METHODS count
      RETURNING
        VALUE(rv_count) TYPE i .
    METHODS has_rc
      IMPORTING
        !iv_rc        TYPE balsort
      RETURNING
        VALUE(rv_yes) TYPE abap_bool .
    METHODS show
      IMPORTING
        !iv_header_text TYPE csequence DEFAULT 'Log' .
    METHODS to_html
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS write .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_log,
        msg  TYPE string,
        type TYPE symsgty,
        rc   TYPE balsort,
      END OF ty_log .
    TYPES:
      BEGIN OF ty_log_out,
        type TYPE icon_d,
        msg  TYPE string,
      END OF ty_log_out .
    TYPES:
      tty_log_out TYPE STANDARD TABLE OF ty_log_out
                              WITH NON-UNIQUE DEFAULT KEY .

    DATA:
      mt_log TYPE STANDARD TABLE OF ty_log WITH DEFAULT KEY .

    METHODS prepare_log_for_display
      RETURNING
        VALUE(rt_log_out) TYPE zcl_abapgit_log=>tty_log_out .
ENDCLASS.



CLASS zcl_abapgit_log IMPLEMENTATION.


  METHOD add.

    FIELD-SYMBOLS: <ls_log> LIKE LINE OF mt_log.

    APPEND INITIAL LINE TO mt_log ASSIGNING <ls_log>.
    <ls_log>-msg  = iv_msg.
    <ls_log>-type = iv_type.
    <ls_log>-rc   = iv_rc.

  ENDMETHOD.


  METHOD add_error.

    add( iv_msg  = iv_msg
         iv_type = 'E' ).

  ENDMETHOD.


  METHOD add_info.

    add( iv_msg  = iv_msg
         iv_type = 'I' ).

  ENDMETHOD.


  METHOD add_warning.

    add( iv_msg  = iv_msg
         iv_type = 'W' ).

  ENDMETHOD.


  METHOD clear.
    CLEAR mt_log.
  ENDMETHOD.


  METHOD count.
    rv_count = lines( mt_log ).
  ENDMETHOD.


  METHOD has_rc.
* todo, this method is only used in unit tests

    READ TABLE mt_log WITH KEY rc = iv_rc TRANSPORTING NO FIELDS.
    rv_yes = boolc( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD prepare_log_for_display.

    DATA: ls_log TYPE ty_log_out.

    FIELD-SYMBOLS: <ls_log> TYPE ty_log.

    LOOP AT mt_log ASSIGNING <ls_log>.

      CLEAR: ls_log.

      ls_log-msg = <ls_log>-msg.

      CASE <ls_log>-type.
        WHEN 'E' OR 'A' OR 'X'.
          ls_log-type = icon_led_red.
        WHEN 'W'.
          ls_log-type = icon_led_yellow.
        WHEN 'I' OR 'S'.
          ls_log-type = icon_led_green.
        WHEN OTHERS.
          ls_log-type = icon_led_inactive.
      ENDCASE.

      INSERT ls_log INTO TABLE rt_log_out.

    ENDLOOP.

  ENDMETHOD.


  METHOD show.

    DATA: lt_log         TYPE tty_log_out,
          lo_alv         TYPE REF TO cl_salv_table,
          lx_error       TYPE REF TO cx_salv_error,
          lo_form_header TYPE REF TO cl_salv_form_header_info,
          lo_columns     TYPE REF TO cl_salv_columns_table,
          lo_column      TYPE REF TO cl_salv_column,
          lo_functions   TYPE REF TO cl_salv_functions_list.

    lt_log = prepare_log_for_display( ).

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

    DATA: lv_class TYPE string,
          lv_icon  TYPE string.

    FIELD-SYMBOLS: <ls_log> LIKE LINE OF mt_log.

    CREATE OBJECT ro_html.

    IF count( ) = 0.
      RETURN.
    ENDIF.

    LOOP AT mt_log ASSIGNING <ls_log>.
      CASE <ls_log>-type.
        WHEN 'W'.
          lv_icon  = 'alert'.
          lv_class = 'warning'.
        WHEN 'E'.
          lv_icon  = 'flame'.
          lv_class = 'error'.
        WHEN OTHERS. " ??? unexpected
          lv_icon  = 'flame'.
          lv_class = 'error'.
      ENDCASE.

      ro_html->add( |<span class="{ lv_class }">| ).
      ro_html->add_icon( lv_icon ).
      ro_html->add( <ls_log>-msg ).
      ro_html->add( '</span>' ).
    ENDLOOP.

  ENDMETHOD.


  METHOD write.

    DATA: ls_log  LIKE LINE OF mt_log,
          lv_text TYPE string.


    LOOP AT mt_log INTO ls_log.
      lv_text = |{ ls_log-type }: { ls_log-msg }|.
      WRITE: / lv_text.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
