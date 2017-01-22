*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_BACKGROUND
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_background_run DEFINITION FINAL
    INHERITING FROM lcl_gui_page.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS lif_gui_page~on_event REDEFINITION.

  PROTECTED SECTION.
    METHODS render_content        REDEFINITION.

  PRIVATE SECTION.
    DATA: mt_text TYPE TABLE OF string.

    METHODS: run.

ENDCLASS.

CLASS lcl_gui_page_background_run IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'BACKGROUND_RUN'.
  ENDMETHOD.  " constructor.

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

  METHOD render_content.

    DATA: lv_text LIKE LINE OF mt_text.

    run( ).

    CREATE OBJECT ro_html.

    ro_html->add( '<div id="toc">' ).
    LOOP AT mt_text INTO lv_text.
      ro_html->add( '<pre>' && lv_text && '</pre><br>' ).
    ENDLOOP.
    ro_html->add( '</div>' ).

  ENDMETHOD.  "render_content

ENDCLASS.

CLASS lcl_gui_page_background DEFINITION FINAL
    INHERITING FROM lcl_gui_page.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING  iv_key TYPE lcl_persistence_repo=>ty_repo-key,
      lif_gui_page~on_event REDEFINITION.

  PROTECTED SECTION.
    METHODS render_content        REDEFINITION.

  PRIVATE SECTION.
    DATA:
      mv_key TYPE lcl_persistence_repo=>ty_repo-key.

    METHODS:
      build_menu
        RETURNING VALUE(ro_menu) TYPE REF TO lcl_html_toolbar,
      render_data
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html
        RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_gui_page_background IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    mv_key = iv_key.
    ms_control-page_title = 'BACKGROUND'.
    ms_control-page_menu  = build_menu( ).

  ENDMETHOD.

  METHOD build_menu.
    CREATE OBJECT ro_menu.
    ro_menu->add( iv_txt = 'Run background logic'
                  iv_act = gc_action-go_background_run ) ##NO_TEXT.
  ENDMETHOD. "build_menu

  METHOD lif_gui_page~on_event.

    DATA ls_bg_task     TYPE lcl_persistence_background=>ty_background.

    CASE iv_action.
      WHEN gc_action-bg_update.
        ls_bg_task     = lcl_html_action_utils=>decode_bg_update( iv_getdata ).
        ls_bg_task-key = mv_key.
        lcl_services_background=>update_task( ls_bg_task ).
        ev_state = gc_event_state-re_render.
    ENDCASE.

  ENDMETHOD.

  METHOD render_data.

    DATA: lo_repo    TYPE REF TO lcl_repo_online,
          lo_per     TYPE REF TO lcl_persistence_background,
          lt_per     TYPE lcl_persistence_background=>tt_background,
          ls_per     LIKE LINE OF lt_per,
          lv_nothing TYPE string,
          lv_push    TYPE string,
          lv_pull    TYPE string,
          lv_afixed  TYPE string,
          lv_aauto   TYPE string.


    CREATE OBJECT ro_html.

    ro_html->add( '<div id="toc">' ).

    CREATE OBJECT lo_per.
    lt_per = lo_per->list( ).

    lo_repo ?= lcl_app=>repo_srv( )->get( mv_key ).

    READ TABLE lt_per INTO ls_per WITH KEY key = lo_repo->get_key( ).
    IF sy-subrc <> 0.
      CLEAR ls_per.
    ENDIF.

    IF ls_per-aname IS INITIAL.
      ls_per-aname = 'foobar' ##NO_TEXT.
    ENDIF.
    IF ls_per-amail IS INITIAL.
      ls_per-amail = 'foo@bar.com' ##NO_TEXT.
    ENDIF.

    CASE ls_per-method.
      WHEN lcl_persistence_background=>c_method-push.
        lv_push = ' checked' ##NO_TEXT.
      WHEN lcl_persistence_background=>c_method-pull.
        lv_pull = ' checked' ##NO_TEXT.
      WHEN OTHERS.
        lv_nothing = ' checked' ##NO_TEXT.
    ENDCASE.

    CASE ls_per-amethod.
      WHEN lcl_persistence_background=>c_amethod-auto.
        lv_aauto = ' checked' ##NO_TEXT.
      WHEN OTHERS.
        lv_afixed = ' checked' ##NO_TEXT.
    ENDCASE.

    ro_html->add( lcl_gui_chunk_lib=>render_repo_top( lo_repo ) ).
    ro_html->add( '<br>' ).

    ro_html->add( '<u>Method</u><br>' )  ##NO_TEXT.
    ro_html->add( |<form method="get" action="sapevent:{ gc_action-bg_update }">| ).
    ro_html->add( '<input type="radio" name="method" value="nothing"' &&
      lv_nothing && '>Do nothing<br>' )  ##NO_TEXT.
    ro_html->add( '<input type="radio" name="method" value="push"' &&
      lv_push && '>Automatic push<br>' )  ##NO_TEXT.
    ro_html->add( '<input type="radio" name="method" value="pull"' &&
      lv_pull && '>Automatic pull<br>' )  ##NO_TEXT.
    ro_html->add( '<br>' ).

    ro_html->add( '<u>HTTP Authentication, optional</u><br>' )  ##NO_TEXT.
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
    ro_html->add( '</table>' ).

    ro_html->add( '<br>' ).

    ro_html->add( '<u>Commit author</u><br>' ).
    ro_html->add( '<input type="radio" name="amethod" value="fixed"' &&
      lv_afixed && '>Fixed<br>' )  ##NO_TEXT.
    ro_html->add( '<input type="radio" name="amethod" value="auto"' &&
      lv_aauto && '>Automatic<br>' )  ##NO_TEXT.
    ro_html->add( '<br>' ).

    ro_html->add( '<table>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td>Name:</td>' ).
    ro_html->add( '<td><input type="text" name="aname" value="' &&
      ls_per-aname && '"></td>' ).
    ro_html->add( '</tr>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<td>Email:</td>' ).
    ro_html->add( '<td><input type="text" name="amail" value="' &&
      ls_per-amail && '"></td>' ).
    ro_html->add( '</tr>' ).
    ro_html->add( '</table>' ).

    ro_html->add( '<br>' ).
    ro_html->add( '<input type="submit" value="Save">' ).

    ro_html->add( '</form>' ).
    ro_html->add( '<br>' ).

    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD render_content.

    CREATE OBJECT ro_html.

    ro_html->add( render_data( ) ).

  ENDMETHOD.  "render_content

ENDCLASS.
