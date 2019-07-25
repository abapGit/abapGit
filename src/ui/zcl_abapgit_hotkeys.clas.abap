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

    CLASS-METHODS:
      should_show_hint
        RETURNING
          VALUE(rv_yes) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      mc_hotkey_interface TYPE string VALUE `ZIF_ABAPGIT_GUI_PAGE_HOTKEY` ##NO_TEXT.

    CLASS-DATA:
      gv_hint_was_shown            TYPE abap_bool,
      gt_interface_implementations TYPE saboo_iimpt.

    CLASS-METHODS:
      get_hotkeys_from_global_intf
        IMPORTING
          io_page           TYPE REF TO zcl_abapgit_gui_page
        RETURNING
          VALUE(rt_hotkeys) TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_with_name,

      get_hotkeys_from_local_intf
        IMPORTING
          io_page           TYPE REF TO zcl_abapgit_gui_page
        RETURNING
          VALUE(rt_hotkeys) TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_with_name
        RAISING
          zcx_abapgit_exception,

      get_local_intf_implementations
        RETURNING
          VALUE(rt_interface_implementations) TYPE saboo_iimpt
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_HOTKEYS IMPLEMENTATION.


  METHOD get_all_default_hotkeys.

    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      rt_hotkey_actions = get_hotkeys_from_local_intf( io_page ).
    ELSE.
      rt_hotkey_actions = get_hotkeys_from_global_intf( io_page ).
    ENDIF.

    " the global shortcuts are defined in the base class
    INSERT LINES OF zcl_abapgit_gui_page=>get_global_hotkeys( ) INTO TABLE rt_hotkey_actions.

    SORT rt_hotkey_actions BY name.

  ENDMETHOD.


  METHOD get_hotkeys_from_global_intf.

    DATA: lt_hotkey_actions TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_with_name,
          lo_interface      TYPE REF TO cl_oo_interface,
          lv_class_name     TYPE abap_abstypename,
          lt_classes        TYPE seo_relkeys,
          lv_where_clause   TYPE string.

    FIELD-SYMBOLS: <ls_class> LIKE LINE OF lt_classes.

    TRY.
        lo_interface ?= cl_oo_class=>get_instance( |{ mc_hotkey_interface }| ).
      CATCH cx_class_not_existent.
        RETURN.
    ENDTRY.

    lt_classes = lo_interface->get_implementing_classes( ).

    IF io_page IS BOUND.
      lv_class_name = cl_abap_classdescr=>get_class_name( io_page ).
      lv_class_name = substring_after( val   = lv_class_name
                                       regex = '^\\CLASS=' ).
      lv_where_clause = |CLSNAME = LV_CLASS_NAME|.
    ENDIF.

    LOOP AT lt_classes ASSIGNING <ls_class>
                       WHERE (lv_where_clause).

      CALL METHOD (<ls_class>-clsname)=>zif_abapgit_gui_page_hotkey~get_hotkey_actions
        RECEIVING
          rt_hotkey_actions = lt_hotkey_actions.

      INSERT LINES OF lt_hotkey_actions INTO TABLE rt_hotkeys.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_hotkeys_from_local_intf.

    DATA: lt_hotkey_actions            TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_with_name,
          lv_where_clause              TYPE string,
          lv_class_name                TYPE abap_abstypename,
          lt_interface_implementations TYPE saboo_iimpt.

    FIELD-SYMBOLS: <ls_intf_implementation> TYPE vseoimplem.

    lt_interface_implementations = get_local_intf_implementations( ).

    lv_where_clause = |REFCLSNAME = MC_HOTKEY_INTERFACE|.

    IF io_page IS BOUND.
      lv_class_name = cl_abap_classdescr=>get_class_name( io_page ).
      lv_class_name = substring_after( val   = lv_class_name
                                       regex = `^\\PROGRAM=` && sy-cprog && `\\CLASS=` ).
      lv_where_clause = |{ lv_where_clause } AND CLSNAME = LV_CLASS_NAME|.
    ENDIF.

    LOOP AT lt_interface_implementations ASSIGNING <ls_intf_implementation>
                                         WHERE (lv_where_clause).

      CALL METHOD (<ls_intf_implementation>-clsname)=>zif_abapgit_gui_page_hotkey~get_hotkey_actions
        RECEIVING
          rt_hotkey_actions = lt_hotkey_actions.

      INSERT LINES OF lt_hotkey_actions INTO TABLE rt_hotkeys.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_local_intf_implementations.

    DATA: lt_type_infos             TYPE saboo_vseot,
          lt_method_implementations TYPE saboo_method_impl_tab,
          lt_source                 TYPE saboo_sourt.

    IF gt_interface_implementations IS INITIAL.

      READ REPORT sy-cprog INTO lt_source.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Cannot read { sy-cprog }| ).
      ENDIF.

      CALL FUNCTION 'SCAN_ABAP_OBJECTS_CLASSES'
        CHANGING
          vseo_tabs                   = lt_type_infos
          method_impls                = lt_method_implementations
          sourc_tab                   = lt_source
        EXCEPTIONS
          scan_abap_source_error      = 1
          scan_abap_src_line_too_long = 2
          OTHERS                      = 3.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      gt_interface_implementations = lt_type_infos-iimpl_tab.

    ENDIF.

    rt_interface_implementations = gt_interface_implementations.

  ENDMETHOD.


  METHOD should_show_hint.
    IF gv_hint_was_shown = abap_false.
      rv_yes = abap_true.
      gv_hint_was_shown = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
