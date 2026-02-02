CLASS lcl_popup_to_confirm DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !iv_titlebar              TYPE clike DEFAULT 'Warning'
        !iv_text_question         TYPE clike
        !iv_text_button_1         TYPE clike DEFAULT 'Yes'
        !iv_action_button_1       TYPE clike
        !iv_text_button_2         TYPE clike DEFAULT 'No'
        !iv_action_button_2       TYPE clike
        !iv_default_button        TYPE clike DEFAULT '2'
        !iv_display_cancel_button TYPE abap_bool DEFAULT abap_false
        !iv_action_cancel         TYPE clike DEFAULT zcl_abapgit_popup_to_confirm=>c_action-cancel
        !iv_popup_type            TYPE clike DEFAULT 'ICON_MESSAGE_WARNING'
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS render
      RETURNING
        VALUE(ri_html) TYPE REF TO object
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS close.

  PRIVATE SECTION.

    CLASS-DATA mo_popup TYPE REF TO zcl_abapgit_popup_to_confirm.

ENDCLASS.

CLASS lcl_popup_to_confirm IMPLEMENTATION.

  METHOD create.
    mo_popup = zcl_abapgit_popup_to_confirm=>create(
      iv_titlebar              = iv_titlebar
      iv_text_question         = iv_text_question
      iv_text_button_1         = iv_text_button_1
      iv_action_button_1       = iv_action_button_1
      iv_text_button_2         = iv_text_button_2
      iv_action_button_2       = iv_action_button_2
      iv_default_button        = iv_default_button
      iv_display_cancel_button = iv_display_cancel_button
      iv_action_cancel         = iv_action_cancel
      iv_popup_type            = iv_popup_type ).
  ENDMETHOD.

  METHOD render.
    IF mo_popup IS INITIAL OR mo_popup->was_closed( ) = abap_true.
      ri_html = zcl_abapgit_html=>create( ).
    ELSE.
      ri_html = zcl_abapgit_gui_in_page_modal=>create( mo_popup ).
    ENDIF.
  ENDMETHOD.

  METHOD close.
    mo_popup->close( ).
  ENDMETHOD.

ENDCLASS.
