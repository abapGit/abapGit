CLASS zcl_abapgit_gui_page_repo_sett DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_page_hotkey .

    METHODS constructor
      IMPORTING
        !io_repo TYPE REF TO zcl_abapgit_repo .

    METHODS zif_abapgit_gui_event_handler~on_event
        REDEFINITION .
  PROTECTED SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        save_settings TYPE string VALUE 'save_settings',
      END OF c_action .
    DATA mo_repo TYPE REF TO zcl_abapgit_repo.

    METHODS render_dot_abapgit
      IMPORTING
        !io_html TYPE REF TO zcl_abapgit_html .
    METHODS render_local_settings
      IMPORTING
        !io_html TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS save
      IMPORTING
        !it_postdata TYPE cnht_post_data_tab
      RAISING
        zcx_abapgit_exception .
    METHODS save_dot_abap
      IMPORTING
        !it_post_fields TYPE tihttpnvp
      RAISING
        zcx_abapgit_exception .
    METHODS save_local_settings
      IMPORTING
        !it_post_fields TYPE tihttpnvp
      RAISING
        zcx_abapgit_exception .
    METHODS parse_post
      IMPORTING
        !it_postdata          TYPE cnht_post_data_tab
      RETURNING
        VALUE(rt_post_fields) TYPE tihttpnvp .

    METHODS render_content
        REDEFINITION .

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_REPO_SETT IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'REPO SETTINGS'.
    mo_repo = io_repo.
  ENDMETHOD.


  METHOD parse_post.

    DATA lv_serialized_post_data TYPE string.

    CONCATENATE LINES OF it_postdata INTO lv_serialized_post_data.
    rt_post_fields = zcl_abapgit_html_action_utils=>parse_fields( lv_serialized_post_data ).

  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ro_html.
    ro_html->add( '<div class="settings_container">' ).
    ro_html->add( '<form id="settings_form" method="post" action="sapevent:' &&
      c_action-save_settings && '">' ).

    render_dot_abapgit( ro_html ).
    render_local_settings( ro_html ).

    ro_html->add( '<br><input type="submit" value="Save" class="floating-button blue-set emphasis">' ).
    ro_html->add( '</form>' ).
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_dot_abapgit.

    CONSTANTS: lc_requirement_edit_count TYPE i VALUE 5.
    DATA: ls_dot               TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit,
          lv_selected          TYPE string,
          lt_folder_logic      TYPE string_table,
          lv_req_index         TYPE i,
          lv_requirement_count TYPE i.

    FIELD-SYMBOLS: <lv_folder_logic> TYPE LINE OF string_table,
                   <ls_requirement>  TYPE zif_abapgit_dot_abapgit=>ty_requirement.

    ls_dot = mo_repo->get_dot_abapgit( )->get_data( ).

    lv_requirement_count = lines( ls_dot-requirements ).
    IF lv_requirement_count < lc_requirement_edit_count.
      DO - lv_requirement_count + lc_requirement_edit_count TIMES.
        INSERT INITIAL LINE INTO TABLE ls_dot-requirements.
      ENDDO.
    ENDIF.

    INSERT zif_abapgit_dot_abapgit=>c_folder_logic-full
           INTO TABLE lt_folder_logic.

    INSERT zif_abapgit_dot_abapgit=>c_folder_logic-prefix
           INTO TABLE lt_folder_logic.

    io_html->add( '<h2>.abapgit.xml</h2>' ).
    io_html->add( 'Folder logic: <select name="folder_logic">' ).

    LOOP AT lt_folder_logic ASSIGNING <lv_folder_logic>.

      IF ls_dot-folder_logic = <lv_folder_logic>.
        lv_selected = 'selected'.
      ELSE.
        CLEAR: lv_selected.
      ENDIF.

      io_html->add( |<option value="{ <lv_folder_logic> }" |
                 && |{ lv_selected }>|
                 && |{ <lv_folder_logic> }</option>| ).

    ENDLOOP.

    io_html->add( '</select>' ).
    io_html->add( '<br>' ).

    io_html->add( 'Starting folder: <input name="starting_folder" type="text" size="10" value="' &&
      ls_dot-starting_folder && '">' ).
    io_html->add( '<br>' ).

    io_html->add( '<h3>Requirements</h3>' ).
    io_html->add( '<table class="repo_tab" id="requirement-tab" style="max-width: 300px;">' ).
    io_html->add( '<tr><th>Software Component</th><th>Min Release</th><th>Min Patch</th></tr>' ).

    LOOP AT ls_dot-requirements ASSIGNING <ls_requirement>.
      lv_req_index = sy-tabix.

      io_html->add( '<tr>' ).
      io_html->add( |<td><input name="req_com_{ lv_req_index }" maxlength=30 type="text" | &&
                    |value="{ <ls_requirement>-component }"></td>| ).
      io_html->add( |<td><input name="req_rel_{ lv_req_index }" maxlength=10 type="text" | &&
                    |value="{ <ls_requirement>-min_release }"></td>| ).
      io_html->add( |<td><input name="req_pat_{ lv_req_index }" maxlength=10 type="text" | &&
                    |value="{ <ls_requirement>-min_patch }"></td>| ).
      io_html->add( '</tr>' ).
    ENDLOOP.

    io_html->add( '</table>' ).

  ENDMETHOD.


  METHOD render_local_settings.

    DATA: lv_checked  TYPE string,
          ls_settings TYPE zif_abapgit_persistence=>ty_repo-local_settings.



    ls_settings = mo_repo->get_local_settings( ).

    io_html->add( '<h2>Local settings</h2>' ).

    IF mo_repo->is_offline( ) = abap_false.
      io_html->add( '<br>' ).
      io_html->add( 'Display name: <input name="display_name" type="text" size="30" value="' &&
        ls_settings-display_name && '">' ).
      io_html->add( '<br>' ).
    ENDIF.

    CLEAR lv_checked.
    IF ls_settings-write_protected = abap_true.
      IF zcl_abapgit_factory=>get_environment( )->is_repo_object_changes_allowed( ) = abap_true.
        lv_checked = | checked|.
      ELSE.
        lv_checked = | checked disabled|.
      ENDIF.
    ENDIF.
    io_html->add( |Write protected <input name="write_protected" type="checkbox"{ lv_checked }><br>| ).

    CLEAR lv_checked.
    IF ls_settings-ignore_subpackages = abap_true.
      lv_checked = | checked|.
    ENDIF.
    io_html->add( |Ignore subpackages <input name="ignore_subpackages" type="checkbox"{ lv_checked }><br>| ).

    CLEAR lv_checked.
    IF ls_settings-only_local_objects = abap_true.
      lv_checked = | checked|.
    ENDIF.
    io_html->add( |Only local objects <input name="only_local_objects" type="checkbox"{ lv_checked }><br>| ).

    io_html->add( '<br>' ).
    io_html->add( 'Code inspector check variant: <input name="check_variant" type="text" size="30" value="' &&
      ls_settings-code_inspector_check_variant && '">' ).
    io_html->add( '<br>' ).

    CLEAR lv_checked.
    IF ls_settings-block_commit = abap_true.
      lv_checked = | checked|.
    ENDIF.
    io_html->add( |Block commit commit/push if code inspection has erros: |
               && |<input name="block_commit" type="checkbox"{ lv_checked }><br>| ).


  ENDMETHOD.


  METHOD save.

    DATA: lt_post_fields TYPE tihttpnvp.


    lt_post_fields = parse_post( it_postdata ).

    save_dot_abap( lt_post_fields ).
    save_local_settings( lt_post_fields ).

    mo_repo->refresh( ).

  ENDMETHOD.


  METHOD save_dot_abap.

    DATA: lo_dot          TYPE REF TO zcl_abapgit_dot_abapgit,
          ls_post_field   LIKE LINE OF it_post_fields,
          lo_requirements TYPE REF TO lcl_requirements.


    lo_dot = mo_repo->get_dot_abapgit( ).

    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'folder_logic'.
    ASSERT sy-subrc = 0.
    lo_dot->set_folder_logic( ls_post_field-value ).

    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'starting_folder'.
    ASSERT sy-subrc = 0.
    lo_dot->set_starting_folder( ls_post_field-value ).

    lo_requirements = lcl_requirements=>new( ).
    LOOP AT it_post_fields INTO ls_post_field WHERE name CP 'req_*'.
      CASE ls_post_field-name+4(3).
        WHEN 'com'.
          lo_requirements->set_component( ls_post_field-value ).
        WHEN 'rel'.
          lo_requirements->set_min_release( ls_post_field-value ).
        WHEN 'pat'.
          lo_requirements->set_min_patch( ls_post_field-value ).
      ENDCASE.
    ENDLOOP.

    lo_dot->set_requirements( lo_requirements->get_as_table( ) ).

    mo_repo->set_dot_abapgit( lo_dot ).

  ENDMETHOD.


  METHOD save_local_settings.

    DATA: ls_settings      TYPE zif_abapgit_persistence=>ty_repo-local_settings,
          ls_post_field    LIKE LINE OF it_post_fields,
          lv_check_variant TYPE sci_chkv.


    ls_settings = mo_repo->get_local_settings( ).

    IF mo_repo->is_offline( ) = abap_false.
      READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'display_name'.
      ASSERT sy-subrc = 0.
      ls_settings-display_name = ls_post_field-value.
    ENDIF.

    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'write_protected' value = 'on'.
    IF sy-subrc = 0.
      ls_settings-write_protected = abap_true.
    ELSE.
      ls_settings-write_protected = abap_false.
    ENDIF.

    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'ignore_subpackages' value = 'on'.
    IF sy-subrc = 0.
      ls_settings-ignore_subpackages = abap_true.
    ELSE.
      ls_settings-ignore_subpackages = abap_false.
    ENDIF.

    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'only_local_objects' value = 'on'.
    IF sy-subrc = 0.
      ls_settings-only_local_objects = abap_true.
    ELSE.
      ls_settings-only_local_objects = abap_false.
    ENDIF.

    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'check_variant'.
    ASSERT sy-subrc = 0.
    lv_check_variant = to_upper( ls_post_field-value ).
    IF ls_post_field-value IS NOT INITIAL.
      zcl_abapgit_code_inspector=>validate_check_variant( lv_check_variant ).
    ENDIF.
    ls_settings-code_inspector_check_variant = lv_check_variant.

    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'block_commit' value = 'on'.
    IF sy-subrc = 0.
      ls_settings-block_commit = abap_true.
    ELSE.
      ls_settings-block_commit = abap_false.
    ENDIF.

    IF ls_settings-block_commit = abap_true
        AND ls_settings-code_inspector_check_variant IS INITIAL.
      zcx_abapgit_exception=>raise( |If block commit is active, a check variant has to be maintained.| ).
    ENDIF.

    mo_repo->set_local_settings( ls_settings ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE iv_action.
      WHEN c_action-save_settings.
        save( it_postdata ).
        ev_state = zcl_abapgit_gui=>c_event_state-go_back.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_hotkey~get_hotkey_actions.

  ENDMETHOD.
ENDCLASS.
