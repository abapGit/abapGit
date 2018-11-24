CLASS zcl_abapgit_hotkeys DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      get_default_hotkeys_from_pages
        IMPORTING
          io_page TYPE REF TO zcl_abapgit_gui_page OPTIONAL
        RETURNING
          VALUE(rt_hotkey_actions) TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_action
        RAISING
          zcx_abapgit_exception,

      get_relevant_hotkeys_for_page
        IMPORTING
          io_page           TYPE REF TO zcl_abapgit_gui_page
        RETURNING
          VALUE(rt_hotkeys) TYPE zif_abapgit_definitions=>tty_hotkey
        RAISING
          zcx_abapgit_exception.

    CLASS-METHODS should_show_hint
      RETURNING
        VALUE(rv_yes) TYPE abap_bool.

  PRIVATE SECTION.
    CLASS-DATA gv_hint_was_shown TYPE abap_bool.

ENDCLASS.



CLASS ZCL_ABAPGIT_HOTKEYS IMPLEMENTATION.


  METHOD get_default_hotkeys_from_pages.

    DATA: lt_hotkey_actions TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_action,
          lo_interface      TYPE REF TO cl_oo_interface,
          lv_class_name     TYPE abap_abstypename,
          lt_classes        TYPE seo_relkeys.

    FIELD-SYMBOLS: <ls_class> TYPE seorelkey.

    TRY.
        lo_interface ?= cl_oo_class=>get_instance( |ZIF_ABAPGIT_GUI_PAGE_HOTKEY| ).

      CATCH cx_class_not_existent.
        " hotkeys are only available with installed abapGit repository
        RETURN.
    ENDTRY.

    lt_classes = lo_interface->get_implementing_classes( ).
    IF io_page IS BOUND.
      lv_class_name = cl_abap_classdescr=>get_class_name( io_page ).
      SHIFT lv_class_name LEFT DELETING LEADING '\CLASS='.
    ENDIF.

    LOOP AT lt_classes ASSIGNING <ls_class>.
      CHECK lv_class_name IS INITIAL OR lv_class_name = <ls_class>-clsname.

      CALL METHOD (<ls_class>-clsname)=>zif_abapgit_gui_page_hotkey~get_hotkey_actions
        RECEIVING
          rt_hotkey_actions = lt_hotkey_actions.

      INSERT LINES OF lt_hotkey_actions INTO TABLE rt_hotkey_actions.

    ENDLOOP.

    " the global shortcuts are defined in the base class
    lt_hotkey_actions = zcl_abapgit_gui_page=>get_hotkey_actions( ).
    INSERT LINES OF lt_hotkey_actions INTO TABLE rt_hotkey_actions.

    SORT rt_hotkey_actions.
    DELETE ADJACENT DUPLICATES FROM rt_hotkey_actions.

  ENDMETHOD.


  METHOD get_relevant_hotkeys_for_page.

    DATA: lo_settings                    TYPE REF TO zcl_abapgit_settings,
          lv_class_name                  TYPE abap_abstypename,
          lt_hotkey_actions_of_curr_page TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_action,
          lv_save_tabix                  TYPE syst-tabix,
          lt_hotkey_actions              TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_action.

    FIELD-SYMBOLS: <ls_hotkey>              TYPE zif_abapgit_definitions=>ty_hotkey.

    lo_settings = zcl_abapgit_persist_settings=>get_instance( )->read( ).

    rt_hotkeys = lo_settings->get_hotkeys( ).

    lv_class_name = cl_abap_classdescr=>get_class_name( io_page ).

    TRY.
        CALL METHOD (lv_class_name)=>zif_abapgit_gui_page_hotkey~get_hotkey_actions
          RECEIVING
            rt_hotkey_actions = lt_hotkey_actions_of_curr_page.

      CATCH cx_root.
        RETURN.
    ENDTRY.

    " these are the global shortcuts
    lt_hotkey_actions = zcl_abapgit_gui_page=>get_hotkey_actions( ).
    INSERT LINES OF lt_hotkey_actions INTO TABLE lt_hotkey_actions_of_curr_page.

    LOOP AT rt_hotkeys ASSIGNING <ls_hotkey>.

      lv_save_tabix = sy-tabix.

      READ TABLE lt_hotkey_actions_of_curr_page TRANSPORTING NO FIELDS
                                                WITH TABLE KEY action
                                                COMPONENTS action = <ls_hotkey>-action.
      IF sy-subrc <> 0.
        " We only offer hotkeys which are supported by the current page or globally
        DELETE rt_hotkeys INDEX lv_save_tabix.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD should_show_hint.
    IF gv_hint_was_shown = abap_false.
      rv_yes = abap_true.
      gv_hint_was_shown = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
