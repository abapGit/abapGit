CLASS zcl_abapgit_gui_hotkey_ctl DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_hotkeys.
    INTERFACES zif_abapgit_gui_hotkey_ctl.
    INTERFACES zif_abapgit_gui_renderable.

    CONSTANTS c_showhotkeys_action TYPE string VALUE `showHotkeys` ##NO_TEXT.

    CLASS-METHODS should_show_hint
      RETURNING
        VALUE(rv_yes) TYPE abap_bool.
    METHODS constructor
      RAISING
        zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      mt_hotkeys       TYPE zif_abapgit_gui_hotkeys=>ty_hotkeys_with_descr,
      ms_user_settings TYPE zif_abapgit_definitions=>ty_s_user_settings,
      mv_visible       TYPE abap_bool.
    CLASS-DATA gv_hint_was_shown TYPE abap_bool .

    METHODS render_scripts
      IMPORTING
        !it_hotkeys    TYPE zif_abapgit_gui_hotkeys=>ty_hotkeys_with_descr
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_HOTKEY_CTL IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    ms_user_settings = zcl_abapgit_persistence_user=>get_instance( )->get_settings( ).

  ENDMETHOD.


  METHOD render_scripts.

    DATA lv_json TYPE string.

    FIELD-SYMBOLS: <ls_hotkey> LIKE LINE OF it_hotkeys.

    lv_json = `{`.

    LOOP AT it_hotkeys ASSIGNING <ls_hotkey>.

      IF sy-tabix > 1.
        lv_json = lv_json && |,|.
      ENDIF.

      lv_json = lv_json && |  "{ <ls_hotkey>-hotkey }" : "{ <ls_hotkey>-action }" |.

    ENDLOOP.

    lv_json = lv_json && `}`.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    ri_html->add( |setKeyBindings({ lv_json });| ).

  ENDMETHOD.


  METHOD should_show_hint.
    IF gv_hint_was_shown = abap_false.
      rv_yes = abap_true.
      gv_hint_was_shown = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA ls_hotkey LIKE LINE OF rt_hotkey_actions.

    ls_hotkey-ui_component = 'Hotkeys'.
    ls_hotkey-action       = c_showhotkeys_action.
    ls_hotkey-description  = 'Show Hotkeys Help'.
    ls_hotkey-hotkey       = '?'.
    INSERT ls_hotkey INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkey_ctl~get_registered_hotkeys.
    rt_registered_hotkeys = mt_hotkeys.
  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkey_ctl~register_hotkeys.

    FIELD-SYMBOLS <ls_hotkey> LIKE LINE OF it_hotkeys.

    " Compress duplicates
    LOOP AT it_hotkeys ASSIGNING <ls_hotkey>.
      READ TABLE mt_hotkeys WITH KEY hotkey = <ls_hotkey>-hotkey TRANSPORTING NO FIELDS.
      IF sy-subrc = 0. " If found command with same hotkey
        DELETE mt_hotkeys INDEX sy-tabix. " Later registered commands enjoys the priority
      ENDIF.

      IF  ms_user_settings-link_hints_enabled = abap_true
      AND ms_user_settings-link_hint_key      = <ls_hotkey>-hotkey.
        " Link hint activation key is more important
        CONTINUE.
      ENDIF.

      APPEND <ls_hotkey> TO mt_hotkeys.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkey_ctl~reset.
    CLEAR mt_hotkeys.
  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkey_ctl~set_visible.

    mv_visible = iv_visible.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA:
      lv_hint               TYPE string,
      lt_registered_hotkeys TYPE zif_abapgit_gui_hotkeys=>ty_hotkeys_with_descr,
      lv_hotkey             TYPE string,
      ls_user_settings      TYPE zif_abapgit_definitions=>ty_s_user_settings.

    FIELD-SYMBOLS <ls_hotkey> LIKE LINE OF lt_registered_hotkeys.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    lt_registered_hotkeys = zif_abapgit_gui_hotkey_ctl~get_registered_hotkeys( ).
    SORT lt_registered_hotkeys BY ui_component description.

    register_deferred_script( render_scripts( lt_registered_hotkeys ) ).

    " Render hotkeys
    ri_html->add( '<ul class="hotkeys">' ).
    LOOP AT lt_registered_hotkeys ASSIGNING <ls_hotkey>.
      ri_html->add( |<li>|
        && |<span class="key-id">{ <ls_hotkey>-hotkey }</span>|
        && |<span class="key-descr">{ <ls_hotkey>-description }</span>|
        && |</li>| ).
    ENDLOOP.

    " render link hints activation key
    ls_user_settings = zcl_abapgit_persistence_user=>get_instance( )->get_settings( ).
    IF ls_user_settings-link_hints_enabled = abap_true.
      ri_html->add( |<li>|
         && |<span class="key-id">{ ls_user_settings-link_hint_key }</span>|
         && |<span class="key-descr">Link Hints</span>|
         && |</li>| ).
      ri_html->add( |<li>|
         && |<span class="key-id">y{ ls_user_settings-link_hint_key }</span>|
         && |<span class="key-descr">Copy Link Text</span>|
         && |</li>| ).
    ENDIF.

    ri_html->add( '</ul>' ).

    CLEAR lv_hotkey.

    READ TABLE lt_registered_hotkeys ASSIGNING <ls_hotkey>
      WITH KEY action = c_showhotkeys_action.
    IF sy-subrc = 0.
      lv_hotkey = <ls_hotkey>-hotkey.
    ENDIF.

    lv_hint = |Close window with upper right corner 'X'|.
    IF lv_hotkey IS NOT INITIAL.
      lv_hint = lv_hint && | or press '{ <ls_hotkey>-hotkey }'|.
    ENDIF.

    ri_html = zcl_abapgit_gui_chunk_lib=>render_infopanel(
      iv_div_id     = 'hotkeys'
      iv_title      = 'Hotkeys'
      iv_hint       = lv_hint
      iv_hide       = boolc( mv_visible = abap_false )
      iv_scrollable = abap_false
      io_content    = ri_html ).

    IF lv_hotkey IS NOT INITIAL AND should_show_hint( ) = abap_true.
      ri_html->add( |<div id="hotkeys-hint" class="corner-hint">|
        && |Press '{ <ls_hotkey>-hotkey }' to get keyboard shortcuts list|
        && |</div>| ).
    ENDIF.

    " Always reset visibility here. Closing of the popup has to be done by the
    " user and is handeled in JS.
    mv_visible = abap_false.

  ENDMETHOD.
ENDCLASS.
