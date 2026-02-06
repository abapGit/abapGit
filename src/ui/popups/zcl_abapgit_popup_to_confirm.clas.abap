CLASS zcl_abapgit_popup_to_confirm DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.

    CONSTANTS:
      BEGIN OF c_action,
        button_1 TYPE string VALUE 'button_1',
        button_2 TYPE string VALUE 'button_2',
        cancel   TYPE string VALUE 'cancel',
      END OF c_action.

    TYPES ty_char1 TYPE c LENGTH 1.

    CLASS-METHODS create
      IMPORTING
        !iv_titlebar              TYPE clike
        !iv_text_question         TYPE clike
        !iv_text_button_1         TYPE clike DEFAULT 'Yes'
        !iv_action_button_1       TYPE clike DEFAULT c_action-button_1
        !iv_text_button_2         TYPE clike DEFAULT 'No'
        !iv_action_button_2       TYPE clike DEFAULT c_action-button_2
        !iv_default_button        TYPE ty_char1 DEFAULT '1'
        !iv_display_cancel_button TYPE abap_bool DEFAULT abap_true
        !iv_action_cancel         TYPE clike DEFAULT c_action-cancel
        !iv_popup_type            TYPE clike DEFAULT 'ICON_MESSAGE_QUESTION'
      RETURNING
        VALUE(ro_popup)           TYPE REF TO zcl_abapgit_popup_to_confirm
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        !iv_titlebar              TYPE clike
        !iv_text_question         TYPE clike
        !iv_text_button_1         TYPE clike
        !iv_action_button_1       TYPE clike
        !iv_text_button_2         TYPE clike
        !iv_action_button_2       TYPE clike
        !iv_default_button        TYPE ty_char1
        !iv_display_cancel_button TYPE abap_bool
        !iv_action_cancel         TYPE clike
        !iv_popup_type            TYPE clike
      RAISING
        zcx_abapgit_exception.

    METHODS close.

    METHODS was_closed
      RETURNING
        VALUE(rv_yes) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_id,
        title TYPE string VALUE 'title',
        icon  TYPE string VALUE 'icon',
        text  TYPE string VALUE 'text',
      END OF c_id.

    DATA mo_form TYPE REF TO zcl_abapgit_html_form.
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA mv_closed TYPE abap_bool.

    DATA mv_title TYPE string.
    DATA mv_text_question TYPE string.
    DATA mv_text_button_1 TYPE string.
    DATA mv_text_button_2 TYPE string.
    DATA mv_action_button_1 TYPE string.
    DATA mv_action_button_2 TYPE string.
    DATA mv_default_button TYPE c LENGTH 1.
    DATA mv_display_cancel_button TYPE abap_bool.
    DATA mv_action_cancel TYPE string.
    DATA mv_popup_type TYPE string.

    METHODS get_form_schema
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form
      RAISING
        zcx_abapgit_exception.

    METHODS render_scripts
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_popup_to_confirm IMPLEMENTATION.


  METHOD close.
    mv_closed = abap_true.
  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    mv_title                 = iv_titlebar.
    mv_text_question         = iv_text_question.
    mv_text_button_1         = iv_text_button_1.
    mv_text_button_2         = iv_text_button_2.
    mv_action_button_1       = iv_action_button_1.
    mv_action_button_2       = iv_action_button_2.
    mv_default_button        = iv_default_button.
    mv_display_cancel_button = iv_display_cancel_button.
    mv_action_cancel         = iv_action_cancel.
    mv_popup_type            = iv_popup_type.

    CREATE OBJECT mo_form_data.
    mo_form = get_form_schema( ).

  ENDMETHOD.


  METHOD create.

    CREATE OBJECT ro_popup
      EXPORTING
        iv_titlebar              = iv_titlebar
        iv_text_question         = iv_text_question
        iv_text_button_1         = iv_text_button_1
        iv_action_button_1       = iv_action_button_1
        iv_text_button_2         = iv_text_button_2
        iv_action_button_2       = iv_action_button_2
        iv_default_button        = iv_default_button
        iv_display_cancel_button = iv_display_cancel_button
        iv_action_cancel         = iv_action_cancel
        iv_popup_type            = iv_popup_type.

  ENDMETHOD.


  METHOD get_form_schema.

    DATA lv_icon TYPE string.
    DATA lv_text TYPE string.
    DATA lv_default_1 TYPE i VALUE zif_abapgit_html_form=>c_cmd_type-input.
    DATA lv_default_2 TYPE i VALUE zif_abapgit_html_form=>c_cmd_type-input.
    DATA lv_default_cancel TYPE i VALUE zif_abapgit_html_form=>c_cmd_type-input.

    ro_form = zcl_abapgit_html_form=>create( iv_form_id = 'popup_to_confirm' ).

    ro_form->start_group(
      iv_name  = c_id-title
      iv_label = mv_title ).

    CASE mv_popup_type.
      WHEN 'ICON_MESSAGE_QUESTION'.
        lv_icon = 's_m_ques.gif'.
      WHEN 'ICON_MESSAGE_INFORMATION'.
        lv_icon = 's_m_info.gif'.
      WHEN 'ICON_MESSAGE_WARNING'.
        lv_icon = 's_m_warn.gif'.
      WHEN 'ICON_MESSAGE_ERROR'.
        lv_icon = 's_m_erro.gif'.
      WHEN 'ICON_MESSAGE_CRITICAL'.
        lv_icon = 's_m_crit.gif'.
    ENDCASE.

    IF lv_icon IS NOT INITIAL.
      ro_form->icon(
        iv_name = c_id-icon
        iv_icon = lv_icon ).
    ENDIF.

    lv_text = mv_text_question.
    REPLACE ALL OCCURRENCES OF |\n| IN lv_text WITH '<br>'.

    ro_form->freetext(
      iv_name = c_id-text
      iv_text = lv_text ).

    CASE mv_default_button.
      WHEN '1'.
        lv_default_1 = zif_abapgit_html_form=>c_cmd_type-input_main.
      WHEN '2'.
        lv_default_2 = zif_abapgit_html_form=>c_cmd_type-input_main.
      WHEN 'A'.
        lv_default_cancel = zif_abapgit_html_form=>c_cmd_type-input_main.
    ENDCASE.

    ro_form->command(
      iv_label    = mv_text_button_1
      iv_cmd_type = lv_default_1
      iv_action   = mv_action_button_1
    )->command(
      iv_label    = mv_text_button_2
      iv_cmd_type = lv_default_2
      iv_action   = mv_action_button_2 ).

    IF mv_display_cancel_button = abap_true.
      ro_form->command(
        iv_label    = 'Cancel'
        iv_cmd_type = lv_default_cancel
        iv_action   = mv_action_cancel ).
    ENDIF.

  ENDMETHOD.


  METHOD render_scripts.
    " Prevent keyboard navigation to elements outside the modal popup
    ri_html = zcl_abapgit_html=>create( )->set_title( 'popup_to_confirm' )->add( 'trapFocus();' ).
  ENDMETHOD.


  METHOD was_closed.
    rv_yes = mv_closed.
  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN mv_action_cancel OR zif_abapgit_definitions=>c_action-go_back.
        " Handle go_back as a "graceful back" - implicit cancel by F3/ESC
        close( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    ri_html = zcl_abapgit_html=>create( mo_form->render( mo_form_data ) ).

    register_deferred_script( render_scripts( ) ).

  ENDMETHOD.
ENDCLASS.
