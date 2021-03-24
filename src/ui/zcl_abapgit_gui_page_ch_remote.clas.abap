CLASS zcl_abapgit_gui_page_ch_remote DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception .

    METHODS zif_abapgit_gui_event_handler~on_event
        REDEFINITION .
  PROTECTED SECTION.

    METHODS render_content
        REDEFINITION .
  PRIVATE SECTION.

    CONSTANTS c_remote_field TYPE string VALUE 'REMOTE' ##NO_TEXT.
    CONSTANTS:
      BEGIN OF c_event,
        go_back TYPE string VALUE 'go_back',
        save    TYPE string VALUE 'save',
      END OF c_event .
    DATA mo_repo TYPE REF TO zcl_abapgit_repo_online .
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_CH_REMOTE IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).
    mo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    ms_control-page_title = 'Change Remote'.

  ENDMETHOD.


  METHOD render_content.

    DATA lo_form TYPE REF TO zcl_abapgit_html_form.
    DATA lo_map TYPE REF TO zcl_abapgit_string_map.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    CREATE OBJECT lo_map.

    lo_form = zcl_abapgit_html_form=>create( ).

    lo_form->text(
      iv_name     = c_remote_field
      iv_required = abap_true
      iv_label = 'New GIT Repository URL'
      iv_hint  = 'HTTPS address of the repository' ).
    lo_map->set(
      iv_key = c_remote_field
      iv_val = mo_repo->get_url( ) ).

    lo_form->command(
      iv_label    = 'Save'
      iv_cmd_type = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action   = c_event-save ).
    lo_form->command(
      iv_label  = 'Back'
      iv_action = c_event-go_back ).

    ri_html->add( lo_form->render( lo_map ) ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA lv_url TYPE string.

    CASE ii_event->mv_action.
      WHEN c_event-go_back.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.

      WHEN c_event-save.
        lv_url = condense( ii_event->form_data( )->get( c_remote_field ) ).

        zcl_abapgit_repo_srv=>get_instance( )->validate_url( lv_url ).

        mo_repo->set_url( lv_url ).
        COMMIT WORK.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
