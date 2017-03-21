*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_REPO_SETTINGS
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_repo_settings DEFINITION FINAL INHERITING FROM lcl_gui_page.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING io_repo TYPE REF TO lcl_repo,
      lif_gui_page~on_event REDEFINITION.

  PROTECTED SECTION.
    CONSTANTS:
      BEGIN OF c_action,
        save_settings TYPE string VALUE 'save_settings',
      END OF c_action.

    DATA: mo_repo TYPE REF TO lcl_repo.

    METHODS:
      render_content REDEFINITION,
      parse_post
        IMPORTING
          it_postdata           TYPE cnht_post_data_tab
        RETURNING
          VALUE(rt_post_fields) TYPE tihttpnvp.

ENDCLASS.                       "lcl_gui_page_debuginfo

CLASS lcl_gui_page_repo_settings IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'REPO SETTINGS'.
    mo_repo = io_repo.
  ENDMETHOD.  " constructor.

  METHOD parse_post.

    DATA lv_serialized_post_data TYPE string.

    CONCATENATE LINES OF it_postdata INTO lv_serialized_post_data.
    rt_post_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_serialized_post_data ).

  ENDMETHOD.

  METHOD render_content.

    DATA: ls_dot TYPE lcl_dot_abapgit=>ty_dot_abapgit.


    ls_dot = mo_repo->get_dot_abapgit( )->get_data( ).

    CREATE OBJECT ro_html.
    ro_html->add( '<div class="settings_container">' ).
    ro_html->add( '<form id="settings_form" method="post" action="sapevent:' && c_action-save_settings && '">' ).
    ro_html->add( '<br>' ).
    ro_html->add( 'Folder logic: <input name="folder_logic" type="text" size="10" value="' && ls_dot-folder_logic && '">' ).
    ro_html->add( '<br>' ).
    ro_html->add( '<input type="submit" value="Save" class="submit">' ).
    ro_html->add( '</form>' ).
    ro_html->add( '</div>' ).

  ENDMETHOD.  "render_content

  METHOD lif_gui_page~on_event.

    DATA: lt_post_fields TYPE tihttpnvp,
          lo_dot         TYPE REF TO lcl_dot_abapgit,
          ls_post_field  LIKE LINE OF lt_post_fields.


    CASE iv_action.
      WHEN c_action-save_settings.
        lt_post_fields = parse_post( it_postdata ).

        READ TABLE lt_post_fields INTO ls_post_field WITH KEY name = 'folder_logic'.
        ASSERT sy-subrc = 0.

        lo_dot = mo_repo->get_dot_abapgit( ).
        lo_dot->set_folder_logic( ls_post_field-value ).
        mo_repo->set_dot_abapgit( lo_dot ).

        ev_state = gc_event_state-go_back.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.                       "lcl_gui_page_debuginfo
