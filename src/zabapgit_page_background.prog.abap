*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_BACKGROUND
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_background_run DEFINITION FINAL
    INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS:
      lif_gui_page~on_event REDEFINITION,
      lif_gui_page~render   REDEFINITION.

  PRIVATE SECTION.
    DATA: mt_text TYPE TABLE OF string.

    METHODS: run.

ENDCLASS.

CLASS lcl_gui_page_background_run IMPLEMENTATION.

  METHOD lif_gui_page~on_event.
    RETURN.
  ENDMETHOD.

  METHOD run.

    DATA: lx_error TYPE REF TO lcx_exception,
          lv_text  TYPE string,
          lv_line  TYPE i VALUE 1.


    TRY.
        lcl_background=>run( ).

        DO.
          READ LINE lv_line LINE VALUE INTO lv_text.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
          APPEND lv_text TO mt_text.
          lv_line = lv_line + 1.
        ENDDO.
      CATCH lcx_exception INTO lx_error.
        APPEND lx_error->mv_text TO mt_text.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_gui_page~render.

    DATA: lv_text LIKE LINE OF mt_text.


    run( ).

    CREATE OBJECT ro_html.

    ro_html->add( header( ) ).
    ro_html->add( title( 'BACKGROUND_RUN' ) ).
    ro_html->add( '<div id="toc">' ).
    LOOP AT mt_text INTO lv_text.
      ro_html->add( '<pre>' && lv_text && '</pre><br>' ).
    ENDLOOP.
    ro_html->add( '</div>' ).
    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_gui_page_background DEFINITION FINAL
    INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS:
      lif_gui_page~on_event REDEFINITION,
      lif_gui_page~render   REDEFINITION.

  PRIVATE SECTION.

    METHODS:
      parse_fields
        IMPORTING iv_getdata       TYPE clike
        RETURNING VALUE(rs_fields) TYPE lcl_persistence_background=>ty_background,
      render_data
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception,
      save
        IMPORTING iv_getdata TYPE clike
        RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_gui_page_background IMPLEMENTATION.

  METHOD parse_fields.

    DEFINE _field.
      READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = &1 ##NO_TEXT.
      IF sy-subrc = 0.
        rs_fields-&2 = <ls_field>-value.
      ENDIF.
    END-OF-DEFINITION.

    DATA: lt_fields TYPE tihttpnvp,
          lv_string TYPE string.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    lv_string = iv_getdata.     " type conversion
    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    _field 'key' key.
    _field 'method' method.
    _field 'username' username.
    _field 'password' password.

    ASSERT NOT rs_fields IS INITIAL.

  ENDMETHOD.

  METHOD lif_gui_page~on_event.

    CASE iv_action.
      WHEN 'save'.
        save( iv_getdata ).
        ev_state = gc_event_state-re_render.
    ENDCASE.

  ENDMETHOD.

  METHOD save.

    DATA: ls_fields      TYPE lcl_persistence_background=>ty_background,
          lo_persistence TYPE REF TO lcl_persistence_background.


    ls_fields = parse_fields( iv_getdata ).

    CREATE OBJECT lo_persistence.

    IF ls_fields-method = lcl_persistence_background=>c_method-nothing.
      lo_persistence->delete( ls_fields-key ).
    ELSE.
      lo_persistence->modify( ls_fields ).
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.

  METHOD render_data.

    DATA: lo_repo    TYPE REF TO lcl_repo,
          lo_online  TYPE REF TO lcl_repo_online,
          lo_per     TYPE REF TO lcl_persistence_background,
          lt_per     TYPE lcl_persistence_background=>tt_background,
          ls_per     LIKE LINE OF lt_per,
          lv_nothing TYPE string,
          lv_push    TYPE string,
          lv_pull    TYPE string,
          lt_list    TYPE lcl_repo_srv=>ty_repo_tt.


    CREATE OBJECT ro_html.

    ro_html->add( '<div id="toc">' ).
    ro_html->add( 'Listing online repositories' ) ##NO_TEXT.
    ro_html->add( '<br><br>' ).

    CREATE OBJECT lo_per.
    lt_per = lo_per->list( ).
    lt_list = lcl_app=>repo_srv( )->list( ).

    LOOP AT lt_list INTO lo_repo.
      IF lo_repo->is_offline( ) = abap_false.
        lo_online ?= lo_repo.

        READ TABLE lt_per INTO ls_per WITH KEY key = lo_online->get_key( ).
        IF sy-subrc <> 0.
          CLEAR ls_per.
        ENDIF.

        CLEAR lv_push.
        CLEAR lv_pull.
        CLEAR lv_nothing.
        CASE ls_per-method.
          WHEN lcl_persistence_background=>c_method-push.
            lv_push = ' checked' ##NO_TEXT.
          WHEN lcl_persistence_background=>c_method-pull.
            lv_pull = ' checked' ##NO_TEXT.
          WHEN OTHERS.
            lv_nothing = ' checked' ##NO_TEXT.
        ENDCASE.

        ro_html->add( '<h1>' && lo_online->get_name( ) && '</h1>' ).
        ro_html->add( '<form method="get" action="sapevent:save">' ).
        ro_html->add( '<input type="hidden" name="key" value="' &&
          lo_repo->get_key( ) && '">' ).
        ro_html->add( '<input type="radio" name="method" value="nothing"' &&
          lv_nothing && '>Do nothing<br>' )  ##NO_TEXT.
        ro_html->add( '<input type="radio" name="method" value="push"' &&
          lv_push && '>Automatic push<br>' )  ##NO_TEXT.
        ro_html->add( '<input type="radio" name="method" value="pull"' &&
          lv_pull && '>Automatic pull<br>' )  ##NO_TEXT.
        ro_html->add( '<br>' ).
        ro_html->add( 'Authentication, optional<br>' )  ##NO_TEXT.
        ro_html->add( '(password will be saved in clear text)<br>' )  ##NO_TEXT.
        ro_html->add( '<table>' ).
        ro_html->add( '<tr>' ).
        ro_html->add( '<td>Username:</td>' ).
        ro_html->add( '<td><input type="text" name="username" value="' &&
          ls_per-username && '"></td>' ).
        ro_html->add( '</tr>' ).
        ro_html->add( '<tr>' ).
        ro_html->add( '<td>Password:</td>' ).
        ro_html->add( '<td><input type="text" name="password" value="' &&
          ls_per-password && '"></td>' ).
        ro_html->add( '</tr>' ).
        ro_html->add( '<tr><td colspan="2" align="right">' ).
        ro_html->add( '<input type="submit" value="Save">' ).
        ro_html->add( '</td></tr>' ).
        ro_html->add( '</table>' ).
        ro_html->add( '</form>' ).
        ro_html->add( '<br>' ).
      ENDIF.
    ENDLOOP.

    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD lif_gui_page~render.

    DATA lo_toolbar TYPE REF TO lcl_html_toolbar.

    CREATE OBJECT lo_toolbar.
    CREATE OBJECT ro_html.

    lo_toolbar->add( iv_txt = 'Run background logic'
                     iv_act = 'background_run' ) ##NO_TEXT.

    ro_html->add( header( ) ).
    ro_html->add( title( iv_title = 'BACKGROUND' io_menu = lo_toolbar ) ).
    ro_html->add( render_data( ) ).
    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.