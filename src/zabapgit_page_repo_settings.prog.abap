*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_REPO_SETTINGS
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_repo_sett DEFINITION FINAL INHERITING FROM lcl_gui_page.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING io_repo TYPE REF TO lcl_repo,
      zif_abapgit_gui_page~on_event REDEFINITION.

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

ENDCLASS.

CLASS lcl_gui_page_repo_sett IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'REPO SETTINGS'.
    mo_repo = io_repo.
  ENDMETHOD.  " constructor.

  METHOD parse_post.

    DATA lv_serialized_post_data TYPE string.

    CONCATENATE LINES OF it_postdata INTO lv_serialized_post_data.
    rt_post_fields = zcl_abapgit_html_action_utils=>parse_fields( lv_serialized_post_data ).

  ENDMETHOD.

  METHOD render_content.

    DATA: ls_dot          TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit,
          lv_selected     TYPE string,
          lt_folder_logic TYPE stringtab.
    FIELD-SYMBOLS: <lv_folder_logic> TYPE LINE OF stringtab.

    ls_dot = mo_repo->get_dot_abapgit( )->get_data( ).

    INSERT zif_abapgit_dot_abapgit=>c_folder_logic-full
           INTO TABLE lt_folder_logic.

    INSERT zif_abapgit_dot_abapgit=>c_folder_logic-prefix
           INTO TABLE lt_folder_logic.

    CREATE OBJECT ro_html.
    ro_html->add( '<div class="settings_container">' ).
    ro_html->add( '<form id="settings_form" method="post" action="sapevent:' &&
      c_action-save_settings && '">' ).

    ro_html->add( '<br>' ).
    ro_html->add( 'Folder logic: <select name="folder_logic">' ).

    LOOP AT lt_folder_logic ASSIGNING <lv_folder_logic>.

      IF ls_dot-folder_logic = <lv_folder_logic>.
        lv_selected = 'selected'.
      ELSE.
        CLEAR: lv_selected.
      ENDIF.

      ro_html->add( |<option value="{ <lv_folder_logic> }" |
                 && |{ lv_selected }>|
                 && |{ <lv_folder_logic> }</option>| ).

    ENDLOOP.

    ro_html->add( '</select>' ).
    ro_html->add( '<br>' ).

    ro_html->add( 'Starting folder: <input name="starting_folder" type="text" size="10" value="' &&
      ls_dot-starting_folder && '">' ).
    ro_html->add( '<br>' ).
    ro_html->add( '<input type="submit" value="Save" class="submit">' ).
    ro_html->add( '</form>' ).
    ro_html->add( '</div>' ).

  ENDMETHOD.  "render_content

  METHOD zif_abapgit_gui_page~on_event.

    DATA: lt_post_fields TYPE tihttpnvp,
          lo_dot         TYPE REF TO zcl_abapgit_dot_abapgit,
          ls_post_field  LIKE LINE OF lt_post_fields.


    CASE iv_action.
      WHEN c_action-save_settings.
        lt_post_fields = parse_post( it_postdata ).

        lo_dot = mo_repo->get_dot_abapgit( ).

        READ TABLE lt_post_fields INTO ls_post_field WITH KEY name = 'folder_logic'.
        ASSERT sy-subrc = 0.
        lo_dot->set_folder_logic( ls_post_field-value ).

        READ TABLE lt_post_fields INTO ls_post_field WITH KEY name = 'starting_folder'.
        ASSERT sy-subrc = 0.
        lo_dot->set_starting_folder( ls_post_field-value ).

        mo_repo->set_dot_abapgit( lo_dot ).
        mo_repo->refresh( ).

        ev_state = zif_abapgit_definitions=>gc_event_state-go_back.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.                       "lcl_gui_page_debuginfo
