CLASS zcl_abapgit_tag_popups DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_ui_factory.

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_tag_popups.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_tag_out.
        INCLUDE TYPE zif_abapgit_definitions=>ty_git_tag.
    TYPES: body_icon TYPE icon_d,
           END OF ty_tag_out,
           tty_tag_out TYPE STANDARD TABLE OF ty_tag_out
                       WITH NON-UNIQUE DEFAULT KEY.

    DATA:
      mt_tags              TYPE tty_tag_out,
      mo_docking_container TYPE REF TO cl_gui_docking_container,
      mo_text_control      TYPE REF TO cl_gui_textedit.

    METHODS:
      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column,

      prepare_tags_for_display
        IMPORTING
          it_tags            TYPE zif_abapgit_definitions=>ty_git_tag_list_tt
        RETURNING
          VALUE(rt_tags_out) TYPE zcl_abapgit_tag_popups=>tty_tag_out,

      clean_up,

      show_docking_container_with
        IMPORTING
          iv_text TYPE string.

ENDCLASS.



CLASS ZCL_ABAPGIT_TAG_POPUPS IMPLEMENTATION.


  METHOD clean_up.

    IF mo_text_control IS BOUND.

      mo_text_control->finalize( ).
      mo_text_control->free(
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3 ).
      ASSERT sy-subrc = 0.

      CLEAR: mo_text_control.

    ENDIF.

    IF mo_docking_container IS BOUND.

      mo_docking_container->finalize( ).
      mo_docking_container->free(
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3 ).
      ASSERT sy-subrc = 0.

      CLEAR: mo_docking_container.

    ENDIF.

  ENDMETHOD.


  METHOD on_double_click.

    FIELD-SYMBOLS: <ls_tag> TYPE zcl_abapgit_tag_popups=>ty_tag_out.

    READ TABLE mt_tags ASSIGNING <ls_tag>
                       INDEX row.
    IF sy-subrc <> 0 OR <ls_tag>-body IS INITIAL.
      RETURN.
    ENDIF.

    show_docking_container_with( <ls_tag>-body ).

  ENDMETHOD.


  METHOD prepare_tags_for_display.

    DATA: ls_tag_out LIKE LINE OF rt_tags_out.

    FIELD-SYMBOLS: <ls_tag> TYPE zif_abapgit_definitions=>ty_git_tag.

    LOOP AT it_tags ASSIGNING <ls_tag>.

      CLEAR: ls_tag_out.

      MOVE-CORRESPONDING <ls_tag> TO ls_tag_out.

      ls_tag_out-name = zcl_abapgit_git_tag=>remove_tag_prefix( ls_tag_out-name ).

      IF ls_tag_out-body IS NOT INITIAL.
        ls_tag_out-body_icon = |{ icon_display_text }|.
      ENDIF.

      INSERT ls_tag_out INTO TABLE rt_tags_out.

    ENDLOOP.

  ENDMETHOD.


  METHOD show_docking_container_with.

    IF mo_docking_container IS NOT BOUND.

      CREATE OBJECT mo_docking_container
        EXPORTING
          side                        = cl_gui_docking_container=>dock_at_bottom
          extension                   = 120
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.
      ASSERT sy-subrc = 0.

    ENDIF.

    IF mo_text_control IS NOT BOUND.
      CREATE OBJECT mo_text_control
        EXPORTING
          parent                 = mo_docking_container
        EXCEPTIONS
          error_cntl_create      = 1
          error_cntl_init        = 2
          error_cntl_link        = 3
          error_dp_create        = 4
          gui_type_not_supported = 5
          OTHERS                 = 6.
      ASSERT sy-subrc = 0.

      mo_text_control->set_readonly_mode(
        EXCEPTIONS
          error_cntl_call_method = 1
          invalid_parameter      = 2
          OTHERS                 = 3 ).
      ASSERT sy-subrc = 0.

    ENDIF.

    mo_text_control->set_textstream(
      EXPORTING
        text                   = iv_text
      EXCEPTIONS
        error_cntl_call_method = 1
        not_supported_by_gui   = 2
        OTHERS                 = 3 ).
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD zif_abapgit_tag_popups~tag_list_popup.

    DATA: lo_alv          TYPE REF TO cl_salv_table,
          lo_table_header TYPE REF TO cl_salv_form_header_info,
          lo_columns      TYPE REF TO cl_salv_columns_table,
          lx_alv          TYPE REF TO cx_salv_error,
          lt_tags         TYPE zif_abapgit_definitions=>ty_git_tag_list_tt,
          lo_event        TYPE REF TO cl_salv_events_table.

    CLEAR: mt_tags.

    lt_tags = zcl_abapgit_factory=>get_branch_overview( io_repo )->get_tags( ).

    IF lines( lt_tags ) = 0.
      zcx_abapgit_exception=>raise( `There are no tags for this repository` ).
    ENDIF.

    mt_tags = prepare_tags_for_display( lt_tags ).

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = lo_alv
          CHANGING
            t_table        = mt_tags ).

        lo_columns = lo_alv->get_columns( ).

        lo_columns->get_column( `TYPE` )->set_technical( ).
        lo_columns->get_column( `DISPLAY_NAME` )->set_technical( ).
        lo_columns->get_column( `BODY` )->set_technical( ).

        lo_columns->get_column( `NAME` )->set_medium_text( 'Tag name' ).
        lo_columns->set_column_position( columnname = 'NAME'
                                         position   = 1 ).

        lo_columns->get_column( `TAGGER_NAME` )->set_medium_text( 'Tagger' ).
        lo_columns->set_column_position( columnname = 'TAGGER_NAME'
                                         position   = 2 ).

        lo_columns->get_column( `TAGGER_EMAIL` )->set_medium_text( 'Tagger E-Mail' ).
        lo_columns->set_column_position( columnname = 'TAGGER_EMAIL'
                                         position   = 3 ).

        lo_columns->get_column( `MESSAGE` )->set_medium_text( 'Tag message' ).
        lo_columns->set_column_position( columnname = 'MESSAGE'
                                         position   = 4 ).

        lo_columns->get_column( `BODY_ICON` )->set_medium_text( 'Body' ).
        lo_columns->get_column( `BODY_ICON` )->set_output_length( 4 ).
        lo_columns->set_column_position( columnname = 'BODY_ICON'
                                         position   = 5 ).

        lo_columns->get_column( `SHA1` )->set_output_length( 15 ).
        lo_columns->get_column( `SHA1` )->set_medium_text( 'SHA' ).
        lo_columns->set_column_position( columnname = 'SHA1'
                                         position   = 6 ).

        lo_columns->get_column( `OBJECT` )->set_output_length( 15 ).
        lo_columns->get_column( `OBJECT` )->set_medium_text( 'Object' ).
        lo_columns->set_column_position( columnname = 'OBJECT'
                                         position   = 7 ).

        lo_columns->set_optimize( ).

        lo_alv->set_screen_popup( start_column = 7
                                  end_column   = 200
                                  start_line   = 1
                                  end_line     = 25 ).

        CREATE OBJECT lo_table_header
          EXPORTING
            text = `Tags`.

        lo_alv->set_top_of_list( lo_table_header ).

        lo_event = lo_alv->get_event( ).

        SET HANDLER on_double_click FOR lo_event.

        lo_alv->display( ).

      CATCH cx_salv_error INTO lx_alv.
        zcx_abapgit_exception=>raise( lx_alv->get_text( ) ).
    ENDTRY.

    clean_up( ).

  ENDMETHOD.


  METHOD zif_abapgit_tag_popups~tag_select_popup.

    DATA: lt_tags             TYPE zif_abapgit_definitions=>ty_git_tag_list_tt,
          lv_answer           TYPE c LENGTH 1,
          lt_selection        TYPE TABLE OF spopli,
          lv_name_with_prefix TYPE string.

    FIELD-SYMBOLS: <ls_sel> LIKE LINE OF lt_selection,
                   <ls_tag> LIKE LINE OF lt_tags.

    lt_tags = zcl_abapgit_factory=>get_branch_overview( io_repo )->get_tags( ).

    IF lines( lt_tags ) = 0.
      zcx_abapgit_exception=>raise( `There are no tags for this repository` ).
    ENDIF.

    LOOP AT lt_tags ASSIGNING <ls_tag>.

      INSERT INITIAL LINE INTO lt_selection INDEX 1 ASSIGNING <ls_sel>.
      <ls_sel>-varoption = zcl_abapgit_git_tag=>remove_tag_prefix( <ls_tag>-name ).

    ENDLOOP.

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        textline1          = 'Select tag'
        titel              = 'Select tag'
        start_col          = 30
        start_row          = 5
      IMPORTING
        answer             = lv_answer
      TABLES
        t_spopli           = lt_selection
      EXCEPTIONS
        not_enough_answers = 1
        too_much_answers   = 2
        too_much_marks     = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error from POPUP_TO_DECIDE_LIST' ).
    ENDIF.

    IF lv_answer = 'A'.
      RETURN.
    ENDIF.

    READ TABLE lt_selection ASSIGNING <ls_sel> WITH KEY selflag = abap_true.
    ASSERT sy-subrc = 0.

    lv_name_with_prefix = zcl_abapgit_git_tag=>add_tag_prefix( <ls_sel>-varoption ).

    READ TABLE lt_tags ASSIGNING <ls_tag> WITH KEY name = lv_name_with_prefix.
    ASSERT sy-subrc = 0.

    rs_tag = <ls_tag>.

  ENDMETHOD.
ENDCLASS.
