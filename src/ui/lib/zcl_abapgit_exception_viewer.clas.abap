CLASS zcl_abapgit_exception_viewer DEFINITION
  PUBLIC
  CREATE PUBLIC.


  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          ix_error TYPE REF TO zcx_abapgit_exception,

      goto_source
        RAISING
          zcx_abapgit_exception,

      goto_message
        RAISING
          zcx_abapgit_exception,

      show_callstack
        RAISING
          zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mx_error     TYPE REF TO zcx_abapgit_exception,
      mt_callstack TYPE abap_callstack.

    METHODS:
      build_top_of_list
        IMPORTING
          is_top_of_stack TYPE abap_callstack_line
        RETURNING
          VALUE(ro_form)  TYPE REF TO cl_salv_form_element,

      add_row
        IMPORTING
          io_grid  TYPE REF TO cl_salv_form_layout_grid
          iv_col_1 TYPE csequence
          iv_col_2 TYPE csequence,

      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING
          row column,

      set_text
        IMPORTING
          io_columns TYPE REF TO cl_salv_columns_table
          iv_column  TYPE lvc_fname
          iv_text    TYPE string
        RAISING
          cx_static_check,

      goto_source_code
        IMPORTING
          is_callstack TYPE abap_callstack_line
        RAISING
          zcx_abapgit_exception,

      extract_classname
        IMPORTING
          iv_mainprogram      TYPE abap_callstack_line-mainprogram
        RETURNING
          VALUE(rv_classname) TYPE tadir-obj_name,

      get_top_of_callstack
        RETURNING
          VALUE(rs_top_of_callstack) TYPE abap_callstack_line
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_exception_viewer IMPLEMENTATION.


  METHOD add_row.

    DATA: lo_row TYPE REF TO cl_salv_form_layout_flow.

    lo_row = io_grid->add_row( ).

    lo_row->create_label( position = 1
                          text     = iv_col_1 ).

    lo_row->create_label( position = 2
                          text     = iv_col_2 ).

  ENDMETHOD.


  METHOD build_top_of_list.

    DATA: lo_grid TYPE REF TO cl_salv_form_layout_grid.

    CREATE OBJECT lo_grid
      EXPORTING
        columns = 2.

    add_row( io_grid  = lo_grid
             iv_col_1 = 'Main program:'
             iv_col_2 = is_top_of_stack-mainprogram ).

    add_row( io_grid  = lo_grid
             iv_col_1 = 'Include name:'
             iv_col_2 = is_top_of_stack-include ).

    add_row( io_grid  = lo_grid
             iv_col_1 = 'Source line'
             iv_col_2 = |{ is_top_of_stack-line }| ).

    ro_form = lo_grid.

  ENDMETHOD.


  METHOD constructor.

    mx_error = ix_error.
    mt_callstack = mx_error->mt_callstack.

  ENDMETHOD.


  METHOD extract_classname.

    rv_classname = substring_before( val   = iv_mainprogram
                                     regex = '=*CP$' ).

  ENDMETHOD.


  METHOD get_top_of_callstack.

    READ TABLE mt_callstack INDEX 1
                            INTO rs_top_of_callstack.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Callstack is empty| ).
    ENDIF.

  ENDMETHOD.


  METHOD goto_message.

    DATA: lt_bdcdata TYPE STANDARD TABLE OF bdcdata,
          ls_bdcdata LIKE LINE OF lt_bdcdata.

    ls_bdcdata-program  = 'SAPLWBMESSAGES'.
    ls_bdcdata-dynpro   = '0100'.
    ls_bdcdata-dynbegin = abap_true.
    INSERT ls_bdcdata INTO TABLE lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-fnam = 'RSDAG-ARBGB'.
    ls_bdcdata-fval = mx_error->if_t100_message~t100key-msgid.
    INSERT ls_bdcdata INTO TABLE lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-fnam = 'MSG_NUMMER'.
    ls_bdcdata-fval = mx_error->if_t100_message~t100key-msgno.
    INSERT ls_bdcdata INTO TABLE lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-fnam = 'RSDAG-MSGFLAG'.
    ls_bdcdata-fval = 'X'.
    INSERT ls_bdcdata INTO TABLE lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-fnam = 'BDC_OKCODE'.
    ls_bdcdata-fval = '=WB_DISPLAY'.
    INSERT ls_bdcdata INTO TABLE lt_bdcdata.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode                   = 'SE91'
        mode_val                = 'E'
      TABLES
        using_tab               = lt_bdcdata
      EXCEPTIONS
        call_transaction_denied = 1
        tcode_invalid           = 2
        OTHERS                  = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD goto_source.

    goto_source_code( get_top_of_callstack( ) ).

  ENDMETHOD.


  METHOD goto_source_code.

    CONSTANTS:
      BEGIN OF lc_obj_type,
        class   TYPE trobjtype VALUE `CLAS`,
        program TYPE trobjtype VALUE `PROG`,
      END OF lc_obj_type.

    DATA:
      ls_item      TYPE zif_abapgit_definitions=>ty_item,
      ls_sub_item  TYPE zif_abapgit_definitions=>ty_item,
      lv_classname LIKE ls_item-obj_name.

    " you should remember that we distinct two cases
    " 1) we navigate to a global class
    " 2) we navigate to a program
    " the latter one is the default case

    lv_classname = extract_classname( is_callstack-mainprogram ).

    IF lv_classname IS NOT INITIAL.
      ls_item-obj_name = lv_classname.
      ls_item-obj_type = lc_obj_type-class.
    ELSE.
      ls_item-obj_name = is_callstack-mainprogram.
      ls_item-obj_type = lc_obj_type-program.
    ENDIF.

    ls_sub_item-obj_name = is_callstack-include.
    ls_sub_item-obj_type = lc_obj_type-program.

    zcl_abapgit_objects=>jump(
      is_item        = ls_item
      is_sub_item    = ls_sub_item
      iv_line_number = is_callstack-line ).

  ENDMETHOD.


  METHOD on_double_click.

    DATA: lx_error TYPE REF TO zcx_abapgit_exception.
    FIELD-SYMBOLS: <ls_callstack> TYPE abap_callstack_line.

    READ TABLE mt_callstack ASSIGNING <ls_callstack>
                            INDEX row.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.
        goto_source_code( <ls_callstack> ).

      CATCH zcx_abapgit_exception INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD set_text.

    DATA: lo_column      TYPE REF TO cl_salv_column,
          lv_short_text  TYPE scrtext_s,
          lv_medium_text TYPE scrtext_m,
          lv_long_text   TYPE scrtext_l.

    lo_column = io_columns->get_column( iv_column ).

    lv_short_text  = iv_text.
    lv_medium_text = iv_text.
    lv_long_text   = iv_text.

    lo_column->set_short_text( lv_short_text ).
    lo_column->set_medium_text( lv_medium_text ).
    lo_column->set_long_text( lv_long_text ).

  ENDMETHOD.


  METHOD show_callstack.

    DATA: lx_error   TYPE REF TO cx_static_check,
          lo_event   TYPE REF TO cl_salv_events_table,
          lo_columns TYPE REF TO cl_salv_columns_table,
          lo_alv     TYPE REF TO cl_salv_table.
    DATA  ls_position TYPE zif_abapgit_popups=>ty_popup_position.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = mt_callstack ).

        lo_alv->get_columns( )->set_optimize( ).

        lo_alv->set_top_of_list( build_top_of_list( get_top_of_callstack( ) ) ).

        ls_position = zcl_abapgit_popups=>center(
          iv_width  = 150
          iv_height = 25 ).

        lo_alv->set_screen_popup( start_column = ls_position-start_column
                                  end_column   = ls_position-end_column
                                  start_line   = ls_position-start_row
                                  end_line     = ls_position-end_row ).

        lo_event = lo_alv->get_event( ).

        lo_columns = lo_alv->get_columns( ).

        set_text( io_columns = lo_columns
                  iv_column  = |LINE|
                  iv_text    = |Line| ).

        set_text( io_columns = lo_columns
                  iv_column  = |LINE|
                  iv_text    = |Line| ).

        set_text( io_columns = lo_columns
                  iv_column  = |BLOCKTYPE|
                  iv_text    = |Event Type| ).

        set_text( io_columns = lo_columns
                  iv_column  = |BLOCKNAME|
                  iv_text    = |Event| ).

        set_text( io_columns = lo_columns
                  iv_column  = |FLAG_SYSTEM|
                  iv_text    = |System| ).

        SET HANDLER on_double_click FOR lo_event.

        lo_alv->display( ).

      CATCH cx_static_check INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
