CLASS zcl_abapgit_gui_page_gitignore DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          io_repo TYPE REF TO zcl_abapgit_repo.

    METHODS:
      zif_abapgit_gui_page~on_event REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF gc_action,
        update TYPE string VALUE 'update',
      END OF gc_action .

    DATA:
      mo_repo TYPE REF TO zcl_abapgit_repo.

    METHODS:
      decode
        IMPORTING
          it_postdata         TYPE cnht_post_data_tab
        RETURNING
          VALUE(rt_gitignore) TYPE zif_abapgit_definitions=>tty_dot_gitignore,

      update
        IMPORTING
          it_gitignore TYPE zif_abapgit_definitions=>tty_dot_gitignore
        RAISING
          zcx_abapgit_exception,

      render_banners_and_toolbar
        IMPORTING
          io_html TYPE REF TO zcl_abapgit_html
        RAISING
          zcx_abapgit_exception,

      render_form
        IMPORTING
          io_html TYPE REF TO zcl_abapgit_html.

ENDCLASS.



CLASS zcl_abapgit_gui_page_gitignore IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mo_repo = io_repo.
    ms_control-page_title = '.gitignore'.
  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="db_entry">' ).

    render_banners_and_toolbar( ro_html ).
    render_form( ro_html ).

    ro_html->add( '</div>' ).

  ENDMETHOD.  "render_content


  METHOD zif_abapgit_gui_page~on_event.

    CASE iv_action.
      WHEN gc_action-update.

        update( decode( it_postdata ) ).

        mo_repo->refresh( ).

        ev_state = zif_abapgit_definitions=>gc_event_state-go_back.

    ENDCASE.

  ENDMETHOD.


  METHOD decode.

    DATA: lt_fields TYPE tihttpnvp,
          lv_text   TYPE string,
          lv_string TYPE string.

    CONCATENATE LINES OF it_postdata INTO lv_string.

    lt_fields = zcl_abapgit_html_action_utils=>parse_fields_upper_case_name( lv_string ).

    zcl_abapgit_html_action_utils=>get_field( EXPORTING name = 'TEXTDATA'
                                                        it   = lt_fields
                                              CHANGING  cv   = lv_text ).

    rt_gitignore = zcl_abapgit_dot_gitignore=>from_text( lv_text ).

  ENDMETHOD.


  METHOD update.

    DATA: lo_persistence_repo TYPE REF TO zcl_abapgit_persistence_repo.

    CREATE OBJECT lo_persistence_repo.
    lo_persistence_repo->update_dot_gitignore( iv_key           = mo_repo->get_key( )
                                               it_dot_gitignore = it_gitignore ).

  ENDMETHOD.


  METHOD render_banners_and_toolbar.

    DATA: lo_toolbar TYPE REF TO zcl_abapgit_html_toolbar,
          lv_text    TYPE string.

    CREATE OBJECT lo_toolbar.
    lo_toolbar->add( iv_act = 'submitFormById(''dot_abapgit_form'');'
                     iv_txt = 'Save'
                     iv_typ = zif_abapgit_definitions=>gc_action_type-onclick
                     iv_opt = zif_abapgit_definitions=>gc_html_opt-strong ) ##NO_TEXT.

    io_html->add( '<table class="toolbar"><tr><td>' ).

    lv_text = |<table class="tag"><tr><td class="label">Name:</td>|
           && | <td>{ mo_repo->get_name( ) }</td></tr></table>|
           && zif_abapgit_definitions=>gc_newline
           && |<table class="tag"><tr><td class="label">Key:</td>|
           && |  <td>{ mo_repo->get_key( ) }</td></tr></table>|.

    io_html->add( lv_text ).

    io_html->add( '</td><td>' ).
    io_html->add( lo_toolbar->render( iv_right = abap_true ) ).
    io_html->add( '</td></tr></table>' ).

  ENDMETHOD.


  METHOD render_form.

    DATA: lv_data      TYPE string,
          lt_gitignore TYPE zif_abapgit_definitions=>tty_dot_gitignore.

    lt_gitignore = mo_repo->get_dot_gitignore( )->get_data( ).

    lv_data = zcl_abapgit_dot_gitignore=>to_text( lt_gitignore ).

    io_html->add( |<form id="dot_abapgit_form" method="post" action="sapevent:| && |{ gc_action-update }">| ).
    io_html->add( |<input type="hidden" name="value" value="{ mo_repo->get_key( ) }">| ).
    io_html->add( |<textarea rows="20" cols="100" name="textdata">{ lv_data }</textarea>| ).
    io_html->add( '</form>' ).

  ENDMETHOD.

ENDCLASS.
