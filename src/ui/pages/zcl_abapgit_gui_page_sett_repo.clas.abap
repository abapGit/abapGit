CLASS zcl_abapgit_gui_page_sett_repo DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler .
    INTERFACES zif_abapgit_gui_renderable .

    CLASS-METHODS create
      IMPORTING
        !io_repo       TYPE REF TO zcl_abapgit_repo
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      IMPORTING
        !io_repo TYPE REF TO zcl_abapgit_repo
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_id,
        dot              TYPE string VALUE 'dot',
        main_language    TYPE string VALUE 'main_language',
        i18n_langs       TYPE string VALUE 'i18n_langs',
        use_lxe          TYPE string VALUE 'use_lxe',
        starting_folder  TYPE string VALUE 'starting_folder',
        folder_logic     TYPE string VALUE 'folder_logic',
        ignore           TYPE string VALUE 'ignore',
        requirements     TYPE string VALUE 'requirements',
        version_constant TYPE string VALUE 'version_constant',
        version_value    TYPE string VALUE 'version_value',
      END OF c_id.
    CONSTANTS:
      BEGIN OF c_event,
        save TYPE string VALUE 'save',
      END OF c_event .
    CONSTANTS c_empty_rows TYPE i VALUE 2 ##NO_TEXT.

    DATA mo_form TYPE REF TO zcl_abapgit_html_form .
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map .
    DATA mo_form_util TYPE REF TO zcl_abapgit_html_form_utils .
    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map .

    DATA mo_repo TYPE REF TO zcl_abapgit_repo .
    DATA mv_requirements_count TYPE i .

    METHODS validate_form
      IMPORTING
        !io_form_data            TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(ro_validation_log) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception .
    METHODS get_form_schema
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form
      RAISING
        zcx_abapgit_exception .
    METHODS read_settings
      RAISING
        zcx_abapgit_exception .
    METHODS save_settings
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_gui_page_sett_repo IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).
    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.
    mo_repo = io_repo.
    mo_form = get_form_schema( ).
    mo_form_util = zcl_abapgit_html_form_utils=>create( mo_form ).

    read_settings( ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_sett_repo.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo = io_repo.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Repository Settings'
      io_page_menu       = zcl_abapgit_gui_chunk_lib=>settings_repo_toolbar(
                             iv_key = io_repo->get_key( )
                             iv_act = zif_abapgit_definitions=>c_action-repo_settings )
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD get_form_schema.

    ro_form = zcl_abapgit_html_form=>create(
                iv_form_id   = 'repo-settings-form'
                iv_help_page = 'https://docs.abapgit.org/settings-dot-abapgit.html' ).

    ro_form->start_group(
      iv_name        = c_id-dot
      iv_label       = 'Repository Settings (.abapgit.xml)'
      iv_hint        = 'Settings stored in root folder in .abapgit.xml file'
    )->text(
      iv_name        = c_id-main_language
      iv_label       = 'Main Language'
      iv_hint        = 'Main language of repository (cannot be changed)'
      iv_readonly    = abap_true
    )->text(
      iv_name        = c_id-i18n_langs
      iv_label       = 'Serialize Translations for These Languages'
      iv_hint        = 'Comma-separate 2-letter ISO language codes e.g. "DE,ES,..." - should not include main language'
    )->checkbox(
      iv_name        = c_id-use_lxe
      iv_label       = 'Use experimental LXE approach for translations'
      iv_hint        = 'It''s mandatory to specify the list of languages above in addition to this setting'
    )->radio(
      iv_name        = c_id-folder_logic
      iv_default_value = zif_abapgit_dot_abapgit=>c_folder_logic-prefix
      iv_label       = 'Folder Logic'
      iv_hint        = 'Define how package folders are named in repository'
    )->option(
      iv_label       = 'Prefix'
      iv_value       = zif_abapgit_dot_abapgit=>c_folder_logic-prefix
    )->option(
      iv_label       = 'Full'
      iv_value       = zif_abapgit_dot_abapgit=>c_folder_logic-full
    )->option(
      iv_label       = 'Mixed'
      iv_value       = zif_abapgit_dot_abapgit=>c_folder_logic-mixed
    )->text(
      iv_name        = c_id-starting_folder
      iv_label       = 'Starting Folder'
      iv_hint        = 'Root folder that defines where serialization starts'
    )->textarea(
      iv_name        = c_id-ignore
      iv_label       = 'Ignore Files'
      iv_hint        = 'List of files in starting folder that shall not be serialized'
    )->table(
      iv_name        = c_id-requirements
      iv_label       = 'Requirements'
      iv_hint        = 'List of software components with minimum release and patch'
    )->column(
      iv_label       = 'Software Component'
      iv_width       = '40%'
    )->column(
      iv_label       = 'Minimum Release'
      iv_width       = '30%'
    )->column(
      iv_label       = 'Minimum Patch'
      iv_width       = '30%'
    )->text(
      iv_name        = c_id-version_constant
      iv_label       = 'Version Constant'
      iv_placeholder = 'ZVERSION_CLASS=>VERSION_CONSTANT'
    )->text(
      iv_name        = c_id-version_value
      iv_label       = 'Version Value'
      iv_readonly    = abap_true
    )->command(
      iv_label       = 'Save Settings'
      iv_cmd_type    = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-save
    )->command(
      iv_label       = 'Back'
      iv_action      = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD read_settings.

    DATA:
      lo_dot          TYPE REF TO zcl_abapgit_dot_abapgit,
      ls_dot          TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit,
      lv_main_lang    TYPE spras,
      lv_language     TYPE t002t-sptxt,
      lv_ignore       TYPE string,
      ls_requirements LIKE LINE OF ls_dot-requirements,
      lv_row          TYPE i,
      lv_val          TYPE string.

    " Get settings from DB
    lo_dot = mo_repo->get_dot_abapgit( ).
    ls_dot = lo_dot->get_data( ).
    lv_main_lang = lo_dot->get_main_language( ).

    " Repository Settings
    SELECT SINGLE sptxt INTO lv_language FROM t002t
      WHERE spras = sy-langu AND sprsl = lv_main_lang.
    IF sy-subrc <> 0.
      lv_language = 'Unknown language; Check your .abapgit.xml file'.
    ENDIF.

    mo_form_data->set(
      iv_key = c_id-main_language
      iv_val = |{ lv_main_lang } ({ lv_language })| ).
    mo_form_data->set(
      iv_key = c_id-i18n_langs
      iv_val = zcl_abapgit_lxe_texts=>convert_table_to_lang_string( lo_dot->get_i18n_languages( ) ) ).
    mo_form_data->set(
      iv_key = c_id-use_lxe
      iv_val = boolc( lo_dot->use_lxe( ) = abap_true ) ) ##TYPE.
    mo_form_data->set(
      iv_key = c_id-folder_logic
      iv_val = ls_dot-folder_logic ).
    mo_form_data->set(
      iv_key = c_id-starting_folder
      iv_val = ls_dot-starting_folder ).
    mo_form_data->set(
      iv_key = c_id-version_constant
      iv_val = ls_dot-version_constant ).
    TRY.
        mo_form_data->set(
          iv_key = c_id-version_value
          iv_val = zcl_abapgit_version=>get_version_constant_value( ls_dot-version_constant ) ).
      CATCH zcx_abapgit_exception.
        mo_form_data->set(
          iv_key = c_id-version_value
          iv_val = '' ).
    ENDTRY.

    lv_ignore = concat_lines_of(
      table = ls_dot-ignore
      sep   = cl_abap_char_utilities=>newline ).

    mo_form_data->set(
      iv_key = c_id-ignore
      iv_val = lv_ignore ).

    LOOP AT ls_dot-requirements INTO ls_requirements.
      lv_row = lv_row + 1.
      DO 3 TIMES.
        CASE sy-index.
          WHEN 1.
            lv_val = ls_requirements-component.
          WHEN 2.
            lv_val = ls_requirements-min_release.
          WHEN 3.
            lv_val = ls_requirements-min_patch.
        ENDCASE.
        mo_form_data->set(
          iv_key = |{ c_id-requirements }-{ lv_row }-{ sy-index }|
          iv_val = lv_val ).
      ENDDO.
    ENDLOOP.

    DO c_empty_rows TIMES.
      lv_row = lv_row + 1.
      DO 3 TIMES.
        mo_form_data->set(
          iv_key = |{ c_id-requirements }-{ lv_row }-{ sy-index }|
          iv_val = '' ).
      ENDDO.
    ENDDO.

    mv_requirements_count = lv_row.

    mo_form_data->set(
      iv_key = |{ c_id-requirements }-{ zif_abapgit_html_form=>c_rows }|
      iv_val = |{ mv_requirements_count }| ).

    " Set for is_dirty check
    mo_form_util->set_data( mo_form_data ).

  ENDMETHOD.


  METHOD save_settings.

    DATA:
      lo_dot          TYPE REF TO zcl_abapgit_dot_abapgit,
      lv_ignore       TYPE string,
      lt_ignore       TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      ls_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt.

    lo_dot = mo_repo->get_dot_abapgit( ).

    lo_dot->set_folder_logic( mo_form_data->get( c_id-folder_logic ) ).
    lo_dot->set_starting_folder( mo_form_data->get( c_id-starting_folder ) ).
    lo_dot->set_version_constant( mo_form_data->get( c_id-version_constant ) ).

    lo_dot->set_i18n_languages(
      zcl_abapgit_lxe_texts=>convert_lang_string_to_table(
        iv_langs              = mo_form_data->get( c_id-i18n_langs )
        iv_skip_main_language = lo_dot->get_main_language( ) ) ).
    lo_dot->use_lxe( boolc( mo_form_data->get( c_id-use_lxe ) = abap_true ) ).

    " Remove all ignores
    lt_ignore = lo_dot->get_data( )-ignore.
    LOOP AT lt_ignore INTO lv_ignore.
      lo_dot->remove_ignore( iv_path = ''
                             iv_filename = lv_ignore ).
    ENDLOOP.

    " Add newly entered ignores
    lt_ignore = zcl_abapgit_convert=>split_string( mo_form_data->get( c_id-ignore ) ).
    LOOP AT lt_ignore INTO lv_ignore.
      lv_ignore = condense( lv_ignore ).
      IF lv_ignore IS NOT INITIAL.
        lo_dot->add_ignore( iv_path = ''
                            iv_filename = lv_ignore ).
      ENDIF.
    ENDLOOP.

    " Requirements
    DO mv_requirements_count TIMES.
      ls_requirements-component   = to_upper( mo_form_data->get( |{ c_id-requirements }-{ sy-index }-1| ) ).
      ls_requirements-min_release = mo_form_data->get( |{ c_id-requirements }-{ sy-index }-2| ).
      ls_requirements-min_patch   = mo_form_data->get( |{ c_id-requirements }-{ sy-index }-3| ).
      APPEND ls_requirements TO lt_requirements.
    ENDDO.

    SORT lt_requirements BY component min_release min_patch.
    DELETE lt_requirements WHERE component IS INITIAL.
    DELETE ADJACENT DUPLICATES FROM lt_requirements COMPARING ALL FIELDS.

    lo_dot->set_requirements( lt_requirements ).

    mo_repo->set_dot_abapgit( lo_dot ).
    mo_repo->refresh( ).

    COMMIT WORK AND WAIT.

    MESSAGE 'Settings succesfully saved' TYPE 'S'.

    read_settings( ).

  ENDMETHOD.


  METHOD validate_form.

    DATA:
      lt_lang_list        TYPE zif_abapgit_definitions=>ty_languages,
      lv_folder           TYPE string,
      lv_len              TYPE i,
      lv_component        TYPE zif_abapgit_dot_abapgit=>ty_requirement-component,
      lv_min_release      TYPE zif_abapgit_dot_abapgit=>ty_requirement-min_release,
      lv_min_patch        TYPE zif_abapgit_dot_abapgit=>ty_requirement-min_patch,
      lv_version_constant TYPE string,
      lx_exception        TYPE REF TO zcx_abapgit_exception.

    ro_validation_log = mo_form_util->validate( io_form_data ).

    lv_folder = io_form_data->get( c_id-starting_folder ).
    lv_len = strlen( lv_folder ) - 1.
    IF lv_len > 0 AND lv_folder(1) <> '/'.
      ro_validation_log->set(
        iv_key = c_id-starting_folder
        iv_val = |The folder must begin with /| ).
    ELSEIF lv_len > 0 AND lv_folder+lv_len(1) <> '/'.
      ro_validation_log->set(
        iv_key = c_id-starting_folder
        iv_val = |The folder must end with /| ).
    ELSEIF lv_folder CA '\'.
      ro_validation_log->set(
        iv_key = c_id-starting_folder
        iv_val = |Use / instead of \\| ).
    ENDIF.

    DO mv_requirements_count TIMES.
      lv_component   = mo_form_data->get( |{ c_id-requirements }-{ sy-index }-1| ).
      lv_min_release = mo_form_data->get( |{ c_id-requirements }-{ sy-index }-2| ).
      lv_min_patch   = mo_form_data->get( |{ c_id-requirements }-{ sy-index }-3| ).

      IF lv_component IS INITIAL AND ( lv_min_release IS NOT INITIAL OR lv_min_patch IS NOT INITIAL ).
        ro_validation_log->set(
          iv_key = c_id-requirements
          iv_val = |If you enter a release or patch, you must also enter a software component| ).
      ELSEIF lv_component IS NOT INITIAL AND lv_min_release IS INITIAL.
        ro_validation_log->set(
          iv_key = c_id-requirements
          iv_val = |If you enter a software component, you must also enter a minumum release| ).
      ENDIF.
    ENDDO.

    TRY.
        lv_version_constant = io_form_data->get( c_id-version_constant ).
        IF lv_version_constant IS NOT INITIAL.
          zcl_abapgit_version=>get_version_constant_value( lv_version_constant ).
        ENDIF.
      CATCH zcx_abapgit_exception INTO lx_exception.
        ro_validation_log->set(
          iv_key = c_id-version_constant
          iv_val = lx_exception->get_text( ) ).
    ENDTRY.

    lt_lang_list = zcl_abapgit_lxe_texts=>convert_lang_string_to_table(
      iv_langs              = io_form_data->get( c_id-i18n_langs )
      iv_skip_main_language = mo_repo->get_dot_abapgit( )->get_main_language( ) ).
    IF io_form_data->get( c_id-use_lxe ) = abap_true AND lt_lang_list IS INITIAL.
      ro_validation_log->set(
        iv_key = c_id-i18n_langs
        iv_val = 'LXE approach requires a non-empy list of languages' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN zif_abapgit_definitions=>c_action-go_back.
        rs_handled-state = mo_form_util->exit( mo_form_data ).

      WHEN c_event-save.
        " Validate all form entries
        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          save_settings( ).
        ENDIF.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    IF mo_form_util->is_empty( mo_form_data ) = abap_true.
      read_settings( ).
    ENDIF.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( `<div class="repo">` ).

    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
                    io_repo               = mo_repo
                    iv_show_commit        = abap_false
                    iv_interactive_branch = abap_true ) ).

    ri_html->add( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).

    ri_html->add( `</div>` ).

  ENDMETHOD.
ENDCLASS.
