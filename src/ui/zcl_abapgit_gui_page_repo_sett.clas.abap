CLASS zcl_abapgit_gui_page_repo_sett DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
                !io_repo TYPE REF TO zcl_abapgit_repo
      RAISING   zcx_abapgit_exception.

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
    METHODS render_dot_abapgit_reqs
      IMPORTING
        io_html         TYPE REF TO zcl_abapgit_html
        it_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt.
    METHODS render_table_row
      IMPORTING
        iv_name        TYPE string
        iv_value       TYPE string
      RETURNING
        VALUE(rv_html) TYPE string.


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
    ro_html->add( |<form id="settings_form" method="post" action="sapevent:{ c_action-save_settings }">| ).

    render_dot_abapgit( ro_html ).
    render_local_settings( ro_html ).

    ro_html->add( '<input type="submit" value="Save" class="floating-button blue-set emphasis">' ).
    ro_html->add( '</form>' ).
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_dot_abapgit.

    DATA: ls_dot          TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit,
          lv_select_html  TYPE string,
          lv_selected     TYPE string,
          lv_language     TYPE t002t-sptxt,
          lv_ignore       TYPE string,
          lt_folder_logic TYPE string_table.

    FIELD-SYMBOLS: <lv_folder_logic> TYPE LINE OF string_table,
                   <lv_ignore>       TYPE string.

    ls_dot = mo_repo->get_dot_abapgit( )->get_data( ).

    APPEND zif_abapgit_dot_abapgit=>c_folder_logic-full TO lt_folder_logic.
    APPEND zif_abapgit_dot_abapgit=>c_folder_logic-prefix TO lt_folder_logic.

    io_html->add( '<h2>.abapgit.xml</h2>' ).
    io_html->add( '<table class="settings">' ).

    SELECT SINGLE sptxt INTO lv_language FROM t002t
      WHERE spras = sy-langu AND sprsl = ls_dot-master_language.
    IF sy-subrc <> 0.
      lv_language = 'Unknown language. Check your settings.'.
    ENDIF.

    io_html->add( render_table_row(
      iv_name  = 'Master language'
      iv_value = |{ ls_dot-master_language } ({ lv_language })|
    ) ).

    lv_select_html = '<select name="folder_logic">'.
    LOOP AT lt_folder_logic ASSIGNING <lv_folder_logic>.

      IF ls_dot-folder_logic = <lv_folder_logic>.
        lv_selected = ' selected'.
      ELSE.
        CLEAR: lv_selected.
      ENDIF.

      lv_select_html = lv_select_html
        && |<option value="{ <lv_folder_logic> }"{ lv_selected }>{ <lv_folder_logic> }</option>|.

    ENDLOOP.
    lv_select_html = lv_select_html && '</select>'.

    io_html->add( render_table_row(
      iv_name  = 'Folder logic'
      iv_value = lv_select_html
    ) ).

    io_html->add( render_table_row(
      iv_name  = 'Starting folder'
      iv_value = |<input name="starting_folder" type="text" size="10" value="{ ls_dot-starting_folder }">|
    ) ).

    LOOP AT ls_dot-ignore ASSIGNING <lv_ignore>.
      lv_ignore = lv_ignore && <lv_ignore> && zif_abapgit_definitions=>c_newline.
    ENDLOOP.

    io_html->add( render_table_row(
      iv_name  = 'Ignore files'
      iv_value = |<textarea name="ignore_files" rows="{ lines( ls_dot-ignore )
                 }" cols="50">{ lv_ignore }</textarea>|
    ) ).

    io_html->add( '</table>' ).

    render_dot_abapgit_reqs(
      it_requirements = ls_dot-requirements
      io_html         = io_html ).


  ENDMETHOD.


  METHOD render_dot_abapgit_reqs.

    CONSTANTS: lc_requirement_edit_min_count TYPE i VALUE 5.
    DATA lv_req_index TYPE i.
    DATA lv_requirement_count TYPE i.
    DATA lt_requirements LIKE it_requirements.
    FIELD-SYMBOLS <ls_requirement> TYPE zif_abapgit_dot_abapgit=>ty_requirement.

    lt_requirements      = it_requirements.
    lv_requirement_count = lines( lt_requirements ).
    IF lv_requirement_count < lc_requirement_edit_min_count.
      DO - lv_requirement_count + lc_requirement_edit_min_count TIMES.
        APPEND INITIAL LINE TO lt_requirements.
      ENDDO.
    ENDIF.

    io_html->add( '<h3>Requirements</h3>' ).
    io_html->add( '<table class="settings-package-requirements" id="requirement-tab">' ).
    io_html->add( '<tr><th>Software Component</th><th>Min Release</th><th>Min Patch</th></tr>' ).

    LOOP AT lt_requirements ASSIGNING <ls_requirement>.
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
    io_html->add( '<table class="settings">' ).

    io_html->add( render_table_row(
      iv_name  = 'Display name'
      iv_value = |<input name="display_name" type="text" size="30" value="{ ls_settings-display_name }">|
    ) ).

    CLEAR lv_checked.
    IF ls_settings-write_protected = abap_true.
      IF zcl_abapgit_factory=>get_environment( )->is_repo_object_changes_allowed( ) = abap_true.
        lv_checked = | checked|.
      ELSE.
        lv_checked = | checked disabled|.
      ENDIF.
    ENDIF.
    io_html->add( render_table_row(
      iv_name  = 'Write protected'
      iv_value = |<input name="write_protected" type="checkbox"{ lv_checked }>|
    ) ).

    CLEAR lv_checked.
    IF ls_settings-ignore_subpackages = abap_true.
      lv_checked = | checked|.
    ENDIF.
    io_html->add( render_table_row(
      iv_name  = 'Ignore subpackages'
      iv_value = |<input name="ignore_subpackages" type="checkbox"{ lv_checked }>|
    ) ).

    CLEAR lv_checked.
    IF ls_settings-only_local_objects = abap_true.
      lv_checked = | checked|.
    ENDIF.
    io_html->add( render_table_row(
      iv_name  = 'Only local objects'
      iv_value = |<input name="only_local_objects" type="checkbox"{ lv_checked }>|
    ) ).

    io_html->add( render_table_row(
      iv_name  = 'Code inspector check variant'
      iv_value = |<input name="check_variant" type="text" size="30" value="{
        ls_settings-code_inspector_check_variant }">|
    ) ).

    CLEAR lv_checked.
    IF ls_settings-block_commit = abap_true.
      lv_checked = | checked|.
    ENDIF.
    io_html->add( render_table_row(
      iv_name  = 'Block commit if code inspection has errors'
      iv_value = |<input name="block_commit" type="checkbox"{ lv_checked }>|
    ) ).

    CLEAR lv_checked.
    IF ls_settings-serialize_master_lang_only = abap_true.
      lv_checked = | checked|.
    ENDIF.
    io_html->add( render_table_row(
      iv_name  = 'Serialize master language only'
      iv_value = |<input name="serialize_master_lang_only" type="checkbox"{ lv_checked }>|
    ) ).

    io_html->add( '</table>' ).

  ENDMETHOD.


  METHOD render_table_row.

    rv_html = '<tr>'
      && |<td>{ iv_name }</td>|
      && |<td>{ iv_value }</td>|
      && '</tr>'.

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
          lv_ignore       TYPE string,
          lt_ignore       TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          lo_requirements TYPE REF TO lcl_requirements.

    lo_dot = mo_repo->get_dot_abapgit( ).

    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'folder_logic'.
    ASSERT sy-subrc = 0.
    lo_dot->set_folder_logic( ls_post_field-value ).

    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'starting_folder'.
    ASSERT sy-subrc = 0.
    lo_dot->set_starting_folder( ls_post_field-value ).

    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'ignore_files'.
    ASSERT sy-subrc = 0.

    " Remove everything
    lt_ignore = lo_dot->get_data( )-ignore.
    LOOP AT lt_ignore INTO lv_ignore.
      lo_dot->remove_ignore( iv_path = ''
                             iv_filename = lv_ignore ).
    ENDLOOP.

    " Add newly entered files
    CLEAR lt_ignore.
    SPLIT ls_post_field-value AT zif_abapgit_definitions=>c_newline INTO TABLE lt_ignore.
    DELETE lt_ignore WHERE table_line IS INITIAL.
    LOOP AT lt_ignore INTO lv_ignore.
      lo_dot->add_ignore( iv_path = ''
                          iv_filename = lv_ignore ).
    ENDLOOP.

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

    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'display_name'.
    ASSERT sy-subrc = 0.
    ls_settings-display_name = ls_post_field-value.

    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'write_protected' value = 'on'.
    ls_settings-write_protected = boolc( sy-subrc = 0 ).

    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'ignore_subpackages' value = 'on'.
    ls_settings-ignore_subpackages = boolc( sy-subrc = 0 ).

    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'only_local_objects' value = 'on'.
    ls_settings-only_local_objects = boolc( sy-subrc = 0 ).

    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'check_variant'.
    ASSERT sy-subrc = 0.
    lv_check_variant = to_upper( ls_post_field-value ).
    IF ls_post_field-value IS NOT INITIAL.
      zcl_abapgit_code_inspector=>validate_check_variant( lv_check_variant ).
    ENDIF.
    ls_settings-code_inspector_check_variant = lv_check_variant.

    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'block_commit' value = 'on'.
    ls_settings-block_commit = boolc( sy-subrc = 0 ).

    IF ls_settings-block_commit = abap_true
        AND ls_settings-code_inspector_check_variant IS INITIAL.
      zcx_abapgit_exception=>raise( |If block commit is active, a check variant has to be maintained.| ).
    ENDIF.

    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'serialize_master_lang_only' value = 'on'.
    ls_settings-serialize_master_lang_only = boolc( sy-subrc = 0 ).

    mo_repo->set_local_settings( ls_settings ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE iv_action.
      WHEN c_action-save_settings.
        save( it_postdata ).
        ev_state = zcl_abapgit_gui=>c_event_state-go_back.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
