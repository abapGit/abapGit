CLASS zcl_abapgit_exception_viewer DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      run.

  PRIVATE SECTION.
    DATA:
      mt_callstack TYPE abap_callstack.

    METHODS:
      go_to_source_code
        IMPORTING
          is_callstack TYPE abap_callstack_line
        RAISING
          zcx_abapgit_exception,

      show_callstack
        IMPORTING
          is_top_of_stack TYPE abap_callstack_line,

      get_top_of_list
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
          cx_salv_not_found,

      process
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


  METHOD get_top_of_list.

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


  METHOD go_to_source_code.

    DATA: ls_item TYPE zif_abapgit_definitions=>ty_item.

    ls_item-obj_name =  substring_before( val   = is_callstack-mainprogram
                                          regex = '=+CP' ).

    IF ls_item-obj_name IS INITIAL.
      ls_item-obj_name = is_callstack-mainprogram.
      ls_item-obj_type = |PROG|.
    ELSE.
      ls_item-obj_type = |CLAS|.
    ENDIF.

    zcl_abapgit_objects=>jump( is_item         = ls_item
                               iv_line_number  = is_callstack-line
                               iv_sub_obj_name = is_callstack-include
                               iv_sub_obj_type = |PROG| ).

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
        go_to_source_code( <ls_callstack> ).

      CATCH zcx_abapgit_exception INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD run.

    DATA: lx_error TYPE REF TO zcx_abapgit_exception.

    TRY.
        process( ).

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

    DATA: lx_error   TYPE REF TO cx_salv_error,
          lo_event   TYPE REF TO cl_salv_events_table,
          lo_columns TYPE REF TO cl_salv_columns_table,
          lo_alv     TYPE REF TO cl_salv_table.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = mt_callstack ).

        lo_alv->get_columns( )->set_optimize( ).

        lo_alv->set_top_of_list( get_top_of_list( is_top_of_stack ) ).

        lo_alv->set_screen_popup( start_column = 10
                                  end_column   = 150
                                  start_line   = 3
                                  end_line     = 30 ).

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

      CATCH cx_salv_error INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD process.

    FIELD-SYMBOLS: <ls_top_of_stack> LIKE LINE OF mt_callstack.

    IMPORT callstack = mt_callstack
           FROM MEMORY ID zcx_abapgit_exception=>gc_memory_id.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |No callstack found in memory| ).
    ENDIF.

    " We mustn't delete the callstack from ABAP memory, because
    " the code can be executed multiple times for an exception.
    " DELETE FROM MEMORY ID zcx_abapgit_exception=>gc_memory_id.

    READ TABLE mt_callstack INDEX 1
                            ASSIGNING <ls_top_of_stack>.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Callstack is empty| ).
    ENDIF.

    CASE sy-tcode.
      WHEN 'ZABAPGIT_GOTO_SOURCE'.

        go_to_source_code( <ls_top_of_stack> ).

      WHEN 'ZABAPGIT_CALLSTACK'.

        show_callstack( <ls_top_of_stack> ).

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
