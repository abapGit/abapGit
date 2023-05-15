CLASS zcl_abapgit_gui_component DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_html_parts,
        scripts      TYPE string VALUE 'scripts',
        hidden_forms TYPE string VALUE 'hidden_forms',
      END OF c_html_parts.

  PROTECTED SECTION.

    METHODS register_deferred_script
      IMPORTING
        ii_part TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.
    METHODS gui_services
      RETURNING
        VALUE(ri_gui_services) TYPE REF TO zif_abapgit_gui_services
      RAISING
        zcx_abapgit_exception.
    METHODS register_handlers
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA mi_gui_services TYPE REF TO zif_abapgit_gui_services.

    METHODS register_event_handler
      IMPORTING
        ii_event_handler TYPE REF TO zif_abapgit_gui_event_handler OPTIONAL
      RAISING
        zcx_abapgit_exception.
    METHODS register_hotkeys
      IMPORTING
        ii_hotkey_provider TYPE REF TO zif_abapgit_gui_hotkeys OPTIONAL
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_COMPONENT IMPLEMENTATION.


  METHOD gui_services.
    IF mi_gui_services IS NOT BOUND.
      mi_gui_services = zcl_abapgit_ui_factory=>get_gui_services( ).
    ENDIF.
    ri_gui_services = mi_gui_services.
  ENDMETHOD.


  METHOD register_deferred_script.
    gui_services( )->get_html_parts( )->add_part(
      iv_collection = c_html_parts-scripts
      ii_part       = ii_part ).
  ENDMETHOD.


  METHOD register_event_handler.

    DATA li_event_handler TYPE REF TO zif_abapgit_gui_event_handler.

    IF ii_event_handler IS BOUND.
      li_event_handler = ii_event_handler.
    ELSE.
      TRY.
          li_event_handler ?= me.
        CATCH cx_root.
          RETURN.
      ENDTRY.
    ENDIF.

    gui_services( )->register_event_handler( li_event_handler ).

  ENDMETHOD.


  METHOD register_handlers.
    register_event_handler( ).
    register_hotkeys( ).
  ENDMETHOD.


  METHOD register_hotkeys.

    DATA li_hotkey_provider TYPE REF TO zif_abapgit_gui_hotkeys.

    IF ii_hotkey_provider IS BOUND.
      li_hotkey_provider = ii_hotkey_provider.
    ELSE.
      TRY.
          li_hotkey_provider ?= me.
        CATCH cx_root.
          RETURN.
      ENDTRY.
    ENDIF.

    gui_services( )->get_hotkeys_ctl( )->register_hotkeys( li_hotkey_provider->get_hotkey_actions( ) ).

  ENDMETHOD.
ENDCLASS.
