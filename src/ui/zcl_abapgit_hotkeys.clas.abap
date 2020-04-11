CLASS zcl_abapgit_hotkeys DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      zif_abapgit_gui_hotkey_ctl,
      zif_abapgit_gui_hotkeys,
      zif_abapgit_gui_renderable.

    CONSTANTS:
      c_showhotkeys_action TYPE string VALUE `showHotkeys` ##NO_TEXT.

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

    DATA mt_hotkey_providers TYPE TABLE OF REF TO zif_abapgit_gui_hotkeys.

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

    DATA: ls_type_infos             TYPE saboo_vseot,
          lt_method_implementations TYPE saboo_method_impl_tab,
          lt_source                 TYPE saboo_sourt.

    IF gt_interface_implementations IS INITIAL.

      READ REPORT sy-cprog INTO lt_source.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Cannot read { sy-cprog }| ).
      ENDIF.

      CALL FUNCTION 'SCAN_ABAP_OBJECTS_CLASSES'
        CHANGING
          vseo_tabs                   = ls_type_infos
          method_impls                = lt_method_implementations
          sourc_tab                   = lt_source
        EXCEPTIONS
          scan_abap_source_error      = 1
          scan_abap_src_line_too_long = 2
          OTHERS                      = 3.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      gt_interface_implementations = ls_type_infos-iimpl_tab.

    ENDIF.

    rt_interface_implementations = gt_interface_implementations.

  ENDMETHOD.


  METHOD should_show_hint.
    IF gv_hint_was_shown = abap_false.
      rv_yes = abap_true.
      gv_hint_was_shown = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA ls_hotkey LIKE LINE OF rt_hotkey_actions.

    ls_hotkey-ui_component  = 'Hotkeys'.
    ls_hotkey-action        = c_showhotkeys_action.
    ls_hotkey-description   = 'Show hotkeys help'.
    ls_hotkey-hotkey = '?'.
    INSERT ls_hotkey INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkey_ctl~get_registered_hotkeys.

    DATA li_hotkey_provider      LIKE LINE OF mt_hotkey_providers.
    DATA lt_hotkey_portion       LIKE rt_registered_hotkeys.
    DATA lt_user_defined_hotkeys TYPE zif_abapgit_definitions=>tty_hotkey.
    DATA lo_settings             TYPE REF TO zcl_abapgit_settings.
    DATA ls_hotkey_to_add        LIKE LINE OF lt_hotkey_portion.

    FIELD-SYMBOLS <ls_user_defined_hotkey> LIKE LINE OF lt_user_defined_hotkeys.

    lo_settings             = zcl_abapgit_persist_settings=>get_instance( )->read( ).
    lt_user_defined_hotkeys = lo_settings->get_hotkeys( ).

    LOOP AT mt_hotkey_providers INTO li_hotkey_provider.
      lt_hotkey_portion = li_hotkey_provider->get_hotkey_actions( ).

      LOOP AT lt_hotkey_portion INTO ls_hotkey_to_add.
        READ TABLE lt_user_defined_hotkeys ASSIGNING <ls_user_defined_hotkey>
          WITH TABLE KEY action COMPONENTS
            ui_component = ls_hotkey_to_add-ui_component
            action       = ls_hotkey_to_add-action.
        IF sy-subrc = 0.
          ls_hotkey_to_add-hotkey = <ls_user_defined_hotkey>-hotkey.
        ENDIF.

        READ TABLE rt_registered_hotkeys TRANSPORTING NO FIELDS
          WITH TABLE KEY action COMPONENTS
            ui_component = ls_hotkey_to_add-ui_component
            action       = ls_hotkey_to_add-action.
        IF sy-subrc = 0.
          DELETE rt_registered_hotkeys INDEX sy-tabix.
        ENDIF.
      ENDLOOP.

      APPEND LINES OF lt_hotkey_portion TO rt_registered_hotkeys.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkey_ctl~register_hotkeys.
    IF ii_hotkeys IS BOUND.
      APPEND ii_hotkeys TO mt_hotkey_providers.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkey_ctl~reset.
    CLEAR mt_hotkey_providers.
  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA:
      lv_hint                 TYPE string,
      lt_registered_hotkeys   TYPE zif_abapgit_gui_hotkeys=>tty_hotkey_with_descr,
      lv_hotkey               TYPE string.

    FIELD-SYMBOLS <ls_hotkey> LIKE LINE OF lt_registered_hotkeys.

    zif_abapgit_gui_hotkey_ctl~register_hotkeys( me ). " TODO refactor ? don't do this directly ?

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    lt_registered_hotkeys = zif_abapgit_gui_hotkey_ctl~get_registered_hotkeys( ).
    SORT lt_registered_hotkeys BY description.

    " Render hotkeys
    ri_html->add( '<ul class="hotkeys">' ).
    LOOP AT lt_registered_hotkeys ASSIGNING <ls_hotkey>.
      ri_html->add( |<li>|
        && |<span class="key-id">{ <ls_hotkey>-hotkey }</span>|
        && |<span class="key-descr">{ <ls_hotkey>-description }</span>|
        && |</li>| ).
    ENDLOOP.
    ri_html->add( '</ul>' ).

    " Wrap
    CLEAR: lv_hotkey.

*    READ TABLE lt_registered_hotkeys ASSIGNING <ls_hotkey>
*      WITH TABLE KEY action COMPONENTS
*      action = zcl_abapgit_gui_page=>c_global_page_action-showhotkeys.
*    IF sy-subrc = 0.
*      lv_hotkey = <ls_hotkey>-hotkey.
*    ENDIF.
*
*    IF lv_hotkey IS NOT INITIAL.
*      lv_hint = |Close window with '{ <ls_hotkey>-hotkey }' or upper right corner 'X'|.
*    ENDIF.

    ri_html = zcl_abapgit_gui_chunk_lib=>render_infopanel(
      iv_div_id     = 'hotkeys'
      iv_title      = 'Hotkeys'
      iv_hint       = lv_hint
      iv_hide       = abap_true
      iv_scrollable = abap_false
      io_content    = ri_html ).

*    IF <ls_hotkey> IS ASSIGNED AND zcl_abapgit_hotkeys=>should_show_hint( ) = abap_true.
*      ro_html->add( |<div id="hotkeys-hint" class="corner-hint">|
*        && |Press '{ <ls_hotkey>-hotkey }' to get keyboard shortcuts list|
*        && |</div>| ).
*    ENDIF.

  ENDMETHOD.
ENDCLASS.
