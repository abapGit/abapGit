CLASS zcl_abapgit_hotkeys DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      get_all_default_hotkeys
        IMPORTING
          io_page                  TYPE REF TO zcl_abapgit_gui_page OPTIONAL
        RETURNING
          VALUE(rt_hotkey_actions) TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_with_name
        RAISING
          zcx_abapgit_exception.

    CLASS-METHODS should_show_hint
      RETURNING
        VALUE(rv_yes) TYPE abap_bool.

  PRIVATE SECTION.
    CLASS-DATA gv_hint_was_shown TYPE abap_bool.

ENDCLASS.



CLASS zcl_abapgit_hotkeys IMPLEMENTATION.


  METHOD get_all_default_hotkeys.

    DATA: lt_hotkey_actions TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_with_name,
          lo_interface      TYPE REF TO cl_oo_interface,
          lv_class_name     TYPE abap_abstypename,
          lt_classes        TYPE seo_relkeys,
          lv_where          TYPE string.

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
      lv_where = |CLSNAME = LV_CLASS_NAME|.
    ENDIF.

    LOOP AT lt_classes ASSIGNING <ls_class>
                       WHERE (lv_where).

      CALL METHOD (<ls_class>-clsname)=>zif_abapgit_gui_page_hotkey~get_hotkey_actions
        RECEIVING
          rt_hotkey_actions = lt_hotkey_actions.

      INSERT LINES OF lt_hotkey_actions INTO TABLE rt_hotkey_actions.

    ENDLOOP.

    " the global shortcuts are defined in the base class
    INSERT LINES OF zcl_abapgit_gui_page=>get_global_hotkeys( ) INTO TABLE rt_hotkey_actions.

    SORT rt_hotkey_actions BY name.

  ENDMETHOD.


  METHOD should_show_hint.
    IF gv_hint_was_shown = abap_false.
      rv_yes = abap_true.
      gv_hint_was_shown = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
