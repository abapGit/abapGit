INTERFACE zif_abapgit_definitions
  PUBLIC .

  TYPES:
    ty_string_tt TYPE STANDARD TABLE OF string WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_item_signature,
      obj_type TYPE tadir-object,
      obj_name TYPE tadir-obj_name,
      devclass TYPE devclass,
    END OF ty_item_signature .
  TYPES:
    BEGIN OF ty_obj_namespace,
      namespace             TYPE trnspace-namespace,
      obj_without_namespace TYPE tadir-obj_name,
    END OF ty_obj_namespace.
  TYPES:
    BEGIN OF ty_item.
      INCLUDE TYPE ty_item_signature.
  TYPES:
      srcsystem             TYPE tadir-srcsystem,
      origlang              TYPE tadir-masterlang,
      inactive              TYPE abap_bool,
      abap_language_version TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version,
    END OF ty_item .
  TYPES:
    ty_items_tt TYPE STANDARD TABLE OF ty_item WITH DEFAULT KEY .
  TYPES:
    ty_items_ts TYPE SORTED TABLE OF ty_item WITH UNIQUE KEY obj_type obj_name .
  TYPES:
    BEGIN OF ty_file_item,
      file TYPE zif_abapgit_git_definitions=>ty_file,
      item TYPE ty_item,
    END OF ty_file_item .
  TYPES:
    ty_files_item_tt TYPE STANDARD TABLE OF ty_file_item WITH DEFAULT KEY .
  TYPES:
    ty_files_item_by_file_tt TYPE SORTED TABLE OF ty_file_item WITH UNIQUE KEY file-path file-filename.
  TYPES:
    ty_yes_no         TYPE c LENGTH 1,
    ty_yes_no_partial TYPE c LENGTH 1.
  TYPES:
    BEGIN OF ty_overwrite.
      INCLUDE TYPE ty_item.
  TYPES:
      state    TYPE c LENGTH 2,
      action   TYPE i,
      icon     TYPE icon_d,
      text     TYPE string,
      decision TYPE ty_yes_no,
    END OF ty_overwrite .
  TYPES:
    ty_overwrite_tt TYPE STANDARD TABLE OF ty_overwrite WITH DEFAULT KEY
                              WITH UNIQUE HASHED KEY object_type_and_name
                                   COMPONENTS obj_type obj_name .
  TYPES:
    BEGIN OF ty_requirements,
      met      TYPE ty_yes_no,
      decision TYPE ty_yes_no,
    END OF ty_requirements .
  TYPES:
    BEGIN OF ty_dependencies,
      met      TYPE ty_yes_no,
      decision TYPE ty_yes_no,
    END OF ty_dependencies .
  TYPES:
    BEGIN OF ty_transport_type,
      request TYPE trfunction,
      task    TYPE trfunction,
    END OF ty_transport_type .
  TYPES:
    BEGIN OF ty_transport,
      required  TYPE abap_bool,
      transport TYPE trkorr,
      type      TYPE ty_transport_type,
    END OF ty_transport .
  TYPES:
    BEGIN OF ty_deserialize_checks,
      overwrite       TYPE ty_overwrite_tt,
      warning_package TYPE ty_overwrite_tt,
      requirements    TYPE ty_requirements,
      dependencies    TYPE ty_dependencies,
      transport       TYPE ty_transport,
      customizing     TYPE ty_transport,
    END OF ty_deserialize_checks .
  TYPES:
    BEGIN OF ty_delete_checks,
      transport TYPE ty_transport,
    END OF ty_delete_checks .
  TYPES:
    BEGIN OF ty_metadata,
      class   TYPE string,
      version TYPE string,
    END OF ty_metadata .
  TYPES:
    BEGIN OF ty_repo_file,
      path       TYPE string,
      filename   TYPE string,
      is_changed TYPE abap_bool,
      rstate     TYPE zif_abapgit_git_definitions=>ty_item_state,
      lstate     TYPE zif_abapgit_git_definitions=>ty_item_state,
    END OF ty_repo_file .
  TYPES:
    ty_repo_file_tt TYPE STANDARD TABLE OF ty_repo_file WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_object,
      sha1    TYPE zif_abapgit_git_definitions=>ty_sha1,
      type    TYPE zif_abapgit_git_definitions=>ty_type,
      data    TYPE xstring,
      adler32 TYPE zif_abapgit_git_definitions=>ty_adler32,
      index   TYPE i,
    END OF ty_object .
  TYPES:
    ty_objects_tt TYPE STANDARD TABLE OF ty_object WITH DEFAULT KEY
      WITH NON-UNIQUE SORTED KEY sha COMPONENTS sha1
      WITH NON-UNIQUE SORTED KEY type COMPONENTS type sha1 .
  TYPES:
    BEGIN OF ty_tadir,
      pgmid      TYPE tadir-pgmid,
      object     TYPE tadir-object,
      obj_name   TYPE tadir-obj_name,
      devclass   TYPE tadir-devclass,
      korrnum    TYPE tadir-korrnum, " used by ZCL_ABAPGIT_DEPENDENCIES->RESOLVE
      delflag    TYPE tadir-delflag,
      genflag    TYPE tadir-genflag,
      path       TYPE string,
      srcsystem  TYPE tadir-srcsystem,
      masterlang TYPE tadir-masterlang,
    END OF ty_tadir .
  TYPES:
    ty_tadir_tt TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_result,
      obj_type  TYPE tadir-object,
      obj_name  TYPE tadir-obj_name,
      inactive  TYPE abap_bool,
      path      TYPE string,
      filename  TYPE string,
      package   TYPE devclass,
      match     TYPE abap_bool,
      lstate    TYPE zif_abapgit_git_definitions=>ty_item_state,
      rstate    TYPE zif_abapgit_git_definitions=>ty_item_state,
      packmove  TYPE abap_bool,
      srcsystem TYPE tadir-srcsystem,
      origlang  TYPE tadir-masterlang,
    END OF ty_result .
  TYPES:
    ty_results_tt TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY
                       WITH NON-UNIQUE SORTED KEY sec_key
                       COMPONENTS obj_type obj_name.
  TYPES:
    ty_results_ts_path TYPE HASHED TABLE OF ty_result WITH UNIQUE KEY path filename .
  TYPES:
    BEGIN OF ty_stage_files,
      local  TYPE ty_files_item_tt,
      remote TYPE zif_abapgit_git_definitions=>ty_files_tt,
      status TYPE ty_results_ts_path,
    END OF ty_stage_files .
  TYPES:
    BEGIN OF ty_tpool.
      INCLUDE TYPE textpool.
  TYPES: split TYPE c LENGTH 8.
  TYPES: END OF ty_tpool .
  TYPES:
    ty_tpool_tt TYPE STANDARD TABLE OF ty_tpool WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_obj_attribute,
      cmpname   TYPE seocmpname,
      attkeyfld TYPE seokeyfld,
      attbusobj TYPE seobusobj,
      exposure  TYPE seoexpose,
    END OF ty_obj_attribute .
  TYPES:
    ty_obj_attribute_tt TYPE STANDARD TABLE OF ty_obj_attribute WITH DEFAULT KEY
                             WITH NON-UNIQUE SORTED KEY cmpname COMPONENTS cmpname .
  TYPES:
    BEGIN OF ty_transport_to_branch,
      branch_name TYPE string,
      commit_text TYPE string,
    END OF ty_transport_to_branch .

  TYPES:
    BEGIN OF ty_diff,
      patch_flag TYPE abap_bool,
      new_num    TYPE c LENGTH 6,
      new        TYPE string,
      result     TYPE c LENGTH 1,
      old_num    TYPE c LENGTH 6,
      old        TYPE string,
      short      TYPE abap_bool,
      beacon     TYPE i,
    END OF ty_diff .
  TYPES:
    ty_diffs_tt TYPE STANDARD TABLE OF ty_diff
                     WITH DEFAULT KEY
                     WITH NON-UNIQUE SORTED KEY new_num COMPONENTS new_num
                     WITH NON-UNIQUE SORTED KEY old_num COMPONENTS old_num.
  TYPES:
    BEGIN OF ty_count,
      insert TYPE i,
      delete TYPE i,
      update TYPE i,
    END OF ty_count .
  TYPES:
    BEGIN OF ty_ancestor,
      commit TYPE zif_abapgit_git_definitions=>ty_sha1,
      tree   TYPE zif_abapgit_git_definitions=>ty_sha1,
      time   TYPE string,
      body   TYPE string,
    END OF ty_ancestor .
  TYPES:
    BEGIN OF ty_repo_item,
      obj_type   TYPE tadir-object,
      obj_name   TYPE tadir-obj_name,
      inactive   TYPE abap_bool,
      sortkey    TYPE i,
      path       TYPE string,
      is_dir     TYPE abap_bool,
      changes    TYPE i,
      lstate     TYPE zif_abapgit_git_definitions=>ty_item_state,
      rstate     TYPE zif_abapgit_git_definitions=>ty_item_state,
      files      TYPE ty_repo_file_tt,
      changed_by TYPE syuname,
      transport  TYPE trkorr,
      packmove   TYPE abap_bool,
      srcsystem  TYPE tadir-srcsystem,
      origlang   TYPE tadir-masterlang,
    END OF ty_repo_item .
  TYPES:
    ty_repo_item_tt TYPE STANDARD TABLE OF ty_repo_item WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_s_user_settings,
      max_lines              TYPE i,
      adt_jump_enabled       TYPE abap_bool,
      show_default_repo      TYPE abap_bool,
      link_hints_enabled     TYPE abap_bool,
      link_hint_key          TYPE c LENGTH 1,
      parallel_proc_disabled TYPE abap_bool,
      icon_scaling           TYPE c LENGTH 1,
      ui_theme               TYPE string,
      hide_sapgui_hint       TYPE abap_bool,
      activate_wo_popup      TYPE abap_bool,
      label_colors           TYPE string,
    END OF ty_s_user_settings .
  TYPES:
    BEGIN OF ty_list_settings,
      filter           TYPE string,
      only_favorites   TYPE abap_bool,
      show_details     TYPE abap_bool,
      order_by         TYPE string,
      order_descending TYPE abap_bool,
    END OF ty_list_settings.
  TYPES:
    ty_dokil_tt TYPE STANDARD TABLE OF dokil
                         WITH NON-UNIQUE DEFAULT KEY .
  TYPES:
    BEGIN OF ty_col_spec,
      tech_name      TYPE string,
      display_name   TYPE string,
      css_class      TYPE string,
      add_tz         TYPE abap_bool,
      title          TYPE string,
      allow_order_by TYPE abap_bool,
    END OF ty_col_spec,
    ty_col_spec_tt TYPE STANDARD TABLE OF ty_col_spec
                      WITH NON-UNIQUE KEY tech_name.
  TYPES:
    ty_proxy_bypass_url       TYPE c LENGTH 255,
    ty_range_proxy_bypass_url TYPE RANGE OF ty_proxy_bypass_url.
  TYPES:
    BEGIN OF ty_version,
      major           TYPE i,
      minor           TYPE i,
      patch           TYPE i,
      prerelase       TYPE string,
      prerelase_patch TYPE i,
    END OF ty_version.
  TYPES:
    ty_deserialization_step TYPE string.
  TYPES:
    ty_deserialization_step_tt TYPE STANDARD TABLE OF ty_deserialization_step
                                          WITH DEFAULT KEY .
  TYPES ty_sci_result TYPE c LENGTH 1.
  CONSTANTS:
    BEGIN OF c_sci_result,
      no_run  TYPE ty_sci_result VALUE '',
      failed  TYPE ty_sci_result VALUE 'F',
      warning TYPE ty_sci_result VALUE 'W',
      passed  TYPE ty_sci_result VALUE 'P',
    END OF c_sci_result.
  CONSTANTS:
    BEGIN OF c_diff,
      unchanged TYPE c LENGTH 1 VALUE ' ',
      insert    TYPE c LENGTH 1 VALUE 'I',
      delete    TYPE c LENGTH 1 VALUE 'D',
      update    TYPE c LENGTH 1 VALUE 'U',
    END OF c_diff .
  CONSTANTS:
    BEGIN OF c_state, " https://git-scm.com/docs/git-status
      unchanged TYPE zif_abapgit_git_definitions=>ty_item_state VALUE '',
      added     TYPE zif_abapgit_git_definitions=>ty_item_state VALUE 'A',
      modified  TYPE zif_abapgit_git_definitions=>ty_item_state VALUE 'M',
      deleted   TYPE zif_abapgit_git_definitions=>ty_item_state VALUE 'D',
      mixed     TYPE zif_abapgit_git_definitions=>ty_item_state VALUE '*',
    END OF c_state .
  CONSTANTS c_english TYPE spras VALUE 'E' ##NO_TEXT.
  CONSTANTS c_root_dir TYPE string VALUE '/' ##NO_TEXT.
  CONSTANTS c_dot_abapgit TYPE string VALUE '.abapgit.xml' ##NO_TEXT.
  CONSTANTS c_author_regex TYPE string VALUE '^(.+) <(.*)> (\d{10})\s?.\d{4}$' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF c_action,
      abapgit_home                  TYPE string VALUE 'abapgit_home',
      bg_update                     TYPE string VALUE 'bg_update',
      change_order_by               TYPE string VALUE 'change_order_by',
      changelog                     TYPE string VALUE 'changelog',
      clipboard                     TYPE string VALUE 'clipboard',
      db_display                    TYPE string VALUE 'db_display',
      db_edit                       TYPE string VALUE 'db_edit',
      direction                     TYPE string VALUE 'direction',
      documentation                 TYPE string VALUE 'documentation',
      flow                          TYPE string VALUE 'flow',
      git_branch_create             TYPE string VALUE 'git_branch_create',
      git_branch_delete             TYPE string VALUE 'git_branch_delete',
      git_branch_merge              TYPE string VALUE 'git_branch_merge',
      git_branch_switch             TYPE string VALUE 'git_branch_switch',
      git_commit                    TYPE string VALUE 'git_commit',
      git_pull                      TYPE string VALUE 'git_pull',
      git_tag_create                TYPE string VALUE 'git_tag_create',
      git_tag_delete                TYPE string VALUE 'git_tag_delete',
      git_tag_switch                TYPE string VALUE 'git_tag_switch',
      go_back                       TYPE string VALUE 'go_back',
      go_background                 TYPE string VALUE 'go_background',
      go_background_run             TYPE string VALUE 'go_background_run',
      go_commit                     TYPE string VALUE 'go_commit',
      go_db                         TYPE string VALUE 'go_db',
      go_debuginfo                  TYPE string VALUE 'go_debuginfo',
      go_explore                    TYPE string VALUE 'go_explore',
      go_file_diff                  TYPE string VALUE 'go_file_diff',
      go_home                       TYPE string VALUE 'go_home',
      go_patch                      TYPE string VALUE 'go_patch',
      go_repo                       TYPE string VALUE 'go_repo',
      go_repo_diff                  TYPE string VALUE 'go_repo_diff',
      go_settings                   TYPE string VALUE 'go_settings',
      go_settings_personal          TYPE string VALUE 'go_settings_personal',
      go_stage                      TYPE string VALUE 'go_stage',
      go_stage_transport            TYPE string VALUE 'go_stage_transport',
      go_tutorial                   TYPE string VALUE 'go_tutorial',
      goto_message                  TYPE string VALUE 'goto_message',
      goto_source                   TYPE string VALUE 'goto_source',
      homepage                      TYPE string VALUE 'homepage',
      ie_devtools                   TYPE string VALUE 'ie_devtools',
      jump                          TYPE string VALUE 'jump',
      jump_transport                TYPE string VALUE 'jump_transport',
      jump_user                     TYPE string VALUE 'jump_user',
      performance_test              TYPE string VALUE 'performance_test',
      repo_activate_objects         TYPE string VALUE 'repo_activate_objects',
      repo_add_all_obj_to_trans_req TYPE string VALUE 'repo_add_all_obj_to_trans_req',
      repo_background               TYPE string VALUE 'repo_background',
      repo_code_inspector           TYPE string VALUE 'repo_code_inspector',
      repo_delete_objects           TYPE string VALUE 'repo_delete_objects',
      repo_infos                    TYPE string VALUE 'repo_infos',
      repo_local_settings           TYPE string VALUE 'repo_local_settings',
      repo_log                      TYPE string VALUE 'repo_log',
      repo_newoffline               TYPE string VALUE 'repo_newoffline',
      repo_newonline                TYPE string VALUE 'repo_newonline',
      repo_open_in_master_lang      TYPE string VALUE 'repo_open_in_master_lang',
      repo_purge                    TYPE string VALUE 'repo_purge',
      repo_refresh                  TYPE string VALUE 'repo_refresh',
      repo_refresh_checksums        TYPE string VALUE 'repo_refresh_checksums',
      repo_remote_settings          TYPE string VALUE 'repo_remote_settings',
      repo_remove                   TYPE string VALUE 'repo_remove',
      repo_settings                 TYPE string VALUE 'repo_settings',
      repo_syntax_check             TYPE string VALUE 'repo_syntax_check',
      repo_toggle_fav               TYPE string VALUE 'repo_toggle_fav',
      repo_transport_to_branch      TYPE string VALUE 'repo_transport_to_branch',
      rfc_compare                   TYPE string VALUE 'rfc_compare',
      show_callstack                TYPE string VALUE 'show_callstack',
      show_hotkeys                  TYPE string VALUE 'show_hotkeys',
      sponsor                       TYPE string VALUE 'sponsor',
      toggle_favorites              TYPE string VALUE 'toggle_favorites',
      url                           TYPE string VALUE 'url',
      yank_to_clipboard             TYPE string VALUE 'yank_to_clipboard',
      zip_export                    TYPE string VALUE 'zip_export',
      zip_export_transport          TYPE string VALUE 'zip_export_transport',
      zip_import                    TYPE string VALUE 'zip_import',
      zip_object                    TYPE string VALUE 'zip_object',
      zip_package                   TYPE string VALUE 'zip_package',
      zip_transport                 TYPE string VALUE 'zip_transport',
    END OF c_action.
  CONSTANTS c_spagpa_param_repo_key TYPE c LENGTH 20 VALUE 'REPO_KEY' ##NO_TEXT.
  CONSTANTS c_spagpa_param_package TYPE c LENGTH 20 VALUE 'PACKAGE' ##NO_TEXT.
  CONSTANTS c_yes TYPE ty_yes_no VALUE 'Y'.
  CONSTANTS c_no TYPE ty_yes_no VALUE 'N'.
  CONSTANTS c_partial TYPE ty_yes_no_partial VALUE 'P'.

  TYPES:
    ty_method TYPE c LENGTH 1 .
  TYPES:
    BEGIN OF ty_stage,
      file   TYPE zif_abapgit_git_definitions=>ty_file,
      method TYPE ty_method,
      status TYPE ty_result,
    END OF ty_stage .
  TYPES:
    ty_stage_tt TYPE SORTED TABLE OF ty_stage
          WITH UNIQUE KEY file-path file-filename .

  CONSTANTS:
    BEGIN OF c_method,
      add    TYPE ty_method VALUE 'A',
      rm     TYPE ty_method VALUE 'R',
      ignore TYPE ty_method VALUE 'I',
      skip   TYPE ty_method VALUE '?',
    END OF c_method .

  TYPES:
    ty_sap_langu_tab TYPE STANDARD TABLE OF langu WITH DEFAULT KEY.
  TYPES:
    ty_languages TYPE STANDARD TABLE OF laiso WITH DEFAULT KEY.
  TYPES:
    BEGIN OF ty_i18n_params,
      main_language         TYPE sy-langu,
      main_language_only    TYPE abap_bool,
      translation_languages TYPE ty_languages,
      use_lxe               TYPE abap_bool,
    END OF ty_i18n_params .
  TYPES ty_trrngtrkor_tt TYPE RANGE OF trkorr.
ENDINTERFACE.
