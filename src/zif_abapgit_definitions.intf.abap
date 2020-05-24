INTERFACE zif_abapgit_definitions
  PUBLIC .


  TYPES:
    ty_type    TYPE c LENGTH 6 .
  TYPES:
    ty_bitbyte TYPE c LENGTH 8 .
  TYPES:
    ty_sha1    TYPE c LENGTH 40 .
  TYPES:
    ty_adler32 TYPE x LENGTH 4 .
  TYPES:
    BEGIN OF ty_file_signature,
      path     TYPE string,
      filename TYPE string,
      sha1     TYPE ty_sha1,
    END OF ty_file_signature .
  TYPES:
    ty_file_signatures_tt TYPE STANDARD TABLE OF
           ty_file_signature WITH DEFAULT KEY .
  TYPES:
    ty_file_signatures_ts TYPE SORTED TABLE OF
           ty_file_signature WITH UNIQUE KEY path filename .
  TYPES:
    BEGIN OF ty_file.
      INCLUDE TYPE ty_file_signature.
  TYPES: data TYPE xstring,
         END OF ty_file .
  TYPES:
    ty_files_tt TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY .
  TYPES:
    ty_string_tt TYPE STANDARD TABLE OF string WITH DEFAULT KEY .
  TYPES:
    ty_repo_ref_tt TYPE STANDARD TABLE OF REF TO zcl_abapgit_repo WITH DEFAULT KEY .
  TYPES ty_git_branch_type TYPE c LENGTH 2 .
  TYPES:
    BEGIN OF ty_git_branch,
      sha1         TYPE ty_sha1,
      name         TYPE string,
      type         TYPE ty_git_branch_type,
      is_head      TYPE abap_bool,
      display_name TYPE string,
    END OF ty_git_branch .
  TYPES:
    ty_git_branch_list_tt TYPE STANDARD TABLE OF ty_git_branch WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_git_tag,
      sha1         TYPE ty_sha1,
      object       TYPE ty_sha1,
      name         TYPE string,
      type         TYPE ty_git_branch_type,
      display_name TYPE string,
      tagger_name  TYPE string,
      tagger_email TYPE string,
      message      TYPE string,
      body         TYPE string,
    END OF ty_git_tag .
  TYPES:
    ty_git_tag_list_tt TYPE STANDARD TABLE OF ty_git_tag WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_hotkey,
      ui_component TYPE string,
      action       TYPE string,
      hotkey       TYPE string,
    END OF ty_hotkey .
  TYPES:
    tty_hotkey TYPE STANDARD TABLE OF ty_hotkey
                    WITH NON-UNIQUE DEFAULT KEY
                    WITH NON-UNIQUE SORTED KEY action
                         COMPONENTS ui_component action.
  TYPES:
    BEGIN OF ty_git_user,
      name  TYPE string,
      email TYPE string,
    END OF ty_git_user .
  TYPES:
    BEGIN OF ty_comment,
      committer TYPE ty_git_user,
      author    TYPE ty_git_user,
      comment   TYPE string,
    END OF ty_comment .
  TYPES:
    BEGIN OF ty_item,
      obj_type TYPE tadir-object,
      obj_name TYPE tadir-obj_name,
      devclass TYPE devclass,
      inactive TYPE abap_bool,
    END OF ty_item .
  TYPES:
    ty_items_tt TYPE STANDARD TABLE OF ty_item WITH DEFAULT KEY .
  TYPES:
    ty_items_ts TYPE SORTED TABLE OF ty_item WITH UNIQUE KEY obj_type obj_name .
  TYPES:
    BEGIN OF ty_file_item,
      file TYPE ty_file,
      item TYPE ty_item,
    END OF ty_file_item .
  TYPES:
    ty_files_item_tt TYPE STANDARD TABLE OF ty_file_item WITH DEFAULT KEY .
  TYPES:
    ty_yes_no         TYPE c LENGTH 1,
    ty_yes_no_partial TYPE c LENGTH 1.
  TYPES:
    BEGIN OF ty_overwrite.
      INCLUDE TYPE ty_item.
  TYPES: decision TYPE ty_yes_no,
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
      met TYPE ty_yes_no,
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
    END OF ty_deserialize_checks .
  TYPES:
    BEGIN OF ty_delete_checks,
      transport TYPE ty_transport,
    END OF ty_delete_checks .
  TYPES:
    BEGIN OF ty_metadata,
      class        TYPE string,
      version      TYPE string,
      late_deser   TYPE abap_bool, " refactor: can be removed later. replaced by steps
      delete_tadir TYPE abap_bool,
      ddic         TYPE abap_bool,
    END OF ty_metadata .
  TYPES:
    BEGIN OF ty_repo_file,
      path       TYPE string,
      filename   TYPE string,
      is_changed TYPE abap_bool,
      rstate     TYPE c LENGTH 1,
      lstate     TYPE c LENGTH 1,
    END OF ty_repo_file .
  TYPES:
    tt_repo_files TYPE STANDARD TABLE OF ty_repo_file WITH DEFAULT KEY .
  TYPES:
    ty_chmod TYPE c LENGTH 6 .
  TYPES:
    BEGIN OF ty_object,
      sha1    TYPE ty_sha1,
      type    TYPE ty_type,
      data    TYPE xstring,
      adler32 TYPE ty_adler32,
      index   TYPE i,
    END OF ty_object .
  TYPES:
    ty_objects_tt TYPE STANDARD TABLE OF ty_object WITH DEFAULT KEY
      WITH NON-UNIQUE SORTED KEY sha COMPONENTS sha1
      WITH NON-UNIQUE SORTED KEY type COMPONENTS type sha1 .
  TYPES:
    BEGIN OF ty_tadir,
      pgmid    TYPE tadir-pgmid,
      object   TYPE tadir-object,
      obj_name TYPE tadir-obj_name,
      devclass TYPE tadir-devclass,
      korrnum  TYPE tadir-korrnum,
      delflag  TYPE tadir-delflag,
      genflag  TYPE tadir-genflag,
      path     TYPE string,
    END OF ty_tadir .
  TYPES:
    ty_tadir_tt TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_result,
      obj_type TYPE tadir-object,
      obj_name TYPE tadir-obj_name,
      inactive TYPE abap_bool,
      path     TYPE string,
      filename TYPE string,
      package  TYPE devclass,
      match    TYPE abap_bool,
      lstate   TYPE c LENGTH 1,
      rstate   TYPE c LENGTH 1,
    END OF ty_result .
  TYPES:
    ty_results_tt TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY .
  TYPES:
    ty_results_ts_path TYPE HASHED TABLE OF ty_result WITH UNIQUE KEY path filename .
  TYPES:
    BEGIN OF ty_stage_files,
      local  TYPE ty_files_item_tt,
      remote TYPE ty_files_tt,
      status TYPE ty_results_ts_path,
    END OF ty_stage_files .
  TYPES:
    ty_sval_tt TYPE STANDARD TABLE OF sval WITH DEFAULT KEY .
  TYPES:
    ty_seocompotx_tt TYPE STANDARD TABLE OF seocompotx WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_tpool.
      INCLUDE TYPE textpool.
  TYPES:   split TYPE c LENGTH 8.
  TYPES: END OF ty_tpool .
  TYPES:
    ty_tpool_tt TYPE STANDARD TABLE OF ty_tpool WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_sotr,
      header  TYPE sotr_head,
      entries TYPE sotr_text_tt,
    END OF ty_sotr .
  TYPES:
    ty_sotr_tt TYPE STANDARD TABLE OF ty_sotr WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_obj_attribute,
      cmpname   TYPE seocmpname,
      attkeyfld TYPE seokeyfld,
      attbusobj TYPE seobusobj,
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
    BEGIN OF ty_create,
      name   TYPE string,
      parent TYPE string,
    END OF ty_create .
  TYPES:
    BEGIN OF ty_commit,
      sha1       TYPE ty_sha1,
      parent1    TYPE ty_sha1,
      parent2    TYPE ty_sha1,
      author     TYPE string,
      email      TYPE string,
      time       TYPE string,
      message    TYPE string,
      body       TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      branch     TYPE string,
      merge      TYPE string,
      tags       TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      create     TYPE STANDARD TABLE OF ty_create WITH DEFAULT KEY,
      compressed TYPE abap_bool,
    END OF ty_commit .
  TYPES:
    ty_commit_tt TYPE STANDARD TABLE OF ty_commit WITH DEFAULT KEY .
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
    ty_diffs_tt TYPE STANDARD TABLE OF ty_diff WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_count,
      insert TYPE i,
      delete TYPE i,
      update TYPE i,
    END OF ty_count .
  TYPES:
    BEGIN OF ty_expanded,
      path  TYPE string,
      name  TYPE string,
      sha1  TYPE ty_sha1,
      chmod TYPE ty_chmod,
    END OF ty_expanded .
  TYPES:
    ty_expanded_tt TYPE STANDARD TABLE OF ty_expanded WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_ancestor,
      commit TYPE ty_sha1,
      tree   TYPE ty_sha1,
      time   TYPE string,
      body   TYPE string,
    END OF ty_ancestor .
  TYPES:
    BEGIN OF ty_merge,
      repo     TYPE REF TO zcl_abapgit_repo_online,
      source   TYPE ty_git_branch,
      target   TYPE ty_git_branch,
      common   TYPE ty_ancestor,
      stree    TYPE ty_expanded_tt,
      ttree    TYPE ty_expanded_tt,
      ctree    TYPE ty_expanded_tt,
      result   TYPE ty_expanded_tt,
      stage    TYPE REF TO zcl_abapgit_stage,
      conflict TYPE string,
    END OF ty_merge .
  TYPES:
    BEGIN OF ty_merge_conflict,
      path        TYPE string,
      filename    TYPE string,
      source_sha1 TYPE ty_sha1,
      source_data TYPE xstring,
      target_sha1 TYPE ty_sha1,
      target_data TYPE xstring,
      result_sha1 TYPE ty_sha1,
      result_data TYPE xstring,
    END OF ty_merge_conflict .
  TYPES:
    tt_merge_conflict TYPE STANDARD TABLE OF ty_merge_conflict WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_repo_item,
      obj_type TYPE tadir-object,
      obj_name TYPE tadir-obj_name,
      inactive TYPE abap_bool,
      sortkey  TYPE i,
      path     TYPE string,
      is_dir   TYPE abap_bool,
      changes  TYPE i,
      lstate   TYPE c LENGTH 1,
      rstate   TYPE c LENGTH 1,
      files    TYPE tt_repo_files,
    END OF ty_repo_item .
  TYPES:
    tt_repo_items TYPE STANDARD TABLE OF ty_repo_item WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_s_user_settings,
      max_lines              TYPE i,
      adt_jump_enabled       TYPE abap_bool,
      show_default_repo      TYPE abap_bool,
      link_hints_enabled     TYPE abap_bool,
      link_hint_key          TYPE c LENGTH 1,
      hotkeys                TYPE tty_hotkey,
      parallel_proc_disabled TYPE abap_bool,
      icon_scaling           TYPE c LENGTH 1,
      ui_theme               TYPE string,
      hide_sapgui_hint       TYPE abap_bool,
    END OF ty_s_user_settings .
  TYPES:
    tty_dokil TYPE STANDARD TABLE OF dokil
                         WITH NON-UNIQUE DEFAULT KEY .
  TYPES:
    tty_lines TYPE STANDARD TABLE OF i
                        WITH NON-UNIQUE DEFAULT KEY .
  TYPES:
    BEGIN OF ty_col_spec,
      tech_name    TYPE string,
      display_name TYPE string,
      css_class    TYPE string,
      add_tz       TYPE abap_bool,
      title        TYPE string,
    END OF ty_col_spec,
    tty_col_spec TYPE STANDARD TABLE OF ty_col_spec
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
  TYPES: BEGIN OF ty_alv_column,
           name   TYPE string,
           text   TYPE string,
           length TYPE lvc_outlen,
         END OF ty_alv_column,
         ty_alv_column_tt TYPE TABLE OF ty_alv_column WITH DEFAULT KEY.
  CONSTANTS:
    BEGIN OF c_git_branch_type,
      branch          TYPE ty_git_branch_type VALUE 'HD',
      lightweight_tag TYPE ty_git_branch_type VALUE 'TG',
      annotated_tag   TYPE ty_git_branch_type VALUE 'AT',
      other           TYPE ty_git_branch_type VALUE 'ZZ',
    END OF c_git_branch_type .
  CONSTANTS c_head_name TYPE string VALUE 'HEAD' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF c_diff,
      insert TYPE c LENGTH 1 VALUE 'I',
      delete TYPE c LENGTH 1 VALUE 'D',
      update TYPE c LENGTH 1 VALUE 'U',
    END OF c_diff .
  CONSTANTS:
    BEGIN OF c_type,
      commit TYPE ty_type VALUE 'commit', "#EC NOTEXT
      tree   TYPE ty_type VALUE 'tree', "#EC NOTEXT
      ref_d  TYPE ty_type VALUE 'ref_d', "#EC NOTEXT
      tag    TYPE ty_type VALUE 'tag', "#EC NOTEXT
      blob   TYPE ty_type VALUE 'blob', "#EC NOTEXT
    END OF c_type .
  CONSTANTS:
    BEGIN OF c_state, " https://git-scm.com/docs/git-status
      unchanged TYPE c LENGTH 1 VALUE '',
      added     TYPE c LENGTH 1 VALUE 'A',
      modified  TYPE c LENGTH 1 VALUE 'M',
      deleted   TYPE c LENGTH 1 VALUE 'D', "For future use
      mixed     TYPE c LENGTH 1 VALUE '*',
    END OF c_state .
  CONSTANTS:
    BEGIN OF c_chmod,
      file       TYPE ty_chmod VALUE '100644',
      executable TYPE ty_chmod VALUE '100755',
      dir        TYPE ty_chmod VALUE '40000 ',
    END OF c_chmod .
  CONSTANTS c_crlf TYPE abap_cr_lf VALUE cl_abap_char_utilities=>cr_lf ##NO_TEXT.
  CONSTANTS c_newline TYPE abap_char1 VALUE cl_abap_char_utilities=>newline ##NO_TEXT.
  CONSTANTS c_english TYPE spras VALUE 'E' ##NO_TEXT.
  CONSTANTS c_root_dir TYPE string VALUE '/' ##NO_TEXT.
  CONSTANTS c_dot_abapgit TYPE string VALUE '.abapgit.xml' ##NO_TEXT.
  CONSTANTS c_author_regex TYPE string VALUE '^(.+) <(.*)> (\d{10})\s?.\d{4}$' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF c_action,
      repo_refresh                  TYPE string VALUE 'repo_refresh',
      repo_remove                   TYPE string VALUE 'repo_remove',
      repo_settings                 TYPE string VALUE 'repo_settings',
      repo_purge                    TYPE string VALUE 'repo_purge',
      repo_newonline                TYPE string VALUE 'repo_newonline',
      repo_newoffline               TYPE string VALUE 'repo_newoffline',
      repo_add_all_obj_to_trans_req TYPE string VALUE 'repo_add_all_obj_to_trans_req',
      repo_remote_attach            TYPE string VALUE 'repo_remote_attach',
      repo_remote_detach            TYPE string VALUE 'repo_remote_detach',
      repo_remote_change            TYPE string VALUE 'repo_remote_change',
      repo_refresh_checksums        TYPE string VALUE 'repo_refresh_checksums',
      repo_toggle_fav               TYPE string VALUE 'repo_toggle_fav',
      repo_transport_to_branch      TYPE string VALUE 'repo_transport_to_branch',
      repo_syntax_check             TYPE string VALUE 'repo_syntax_check',
      repo_code_inspector           TYPE string VALUE 'repo_code_inspector',
      repo_open_in_master_lang      TYPE string VALUE 'repo_open_in_master_lang',
      repo_log                      TYPE string VALUE 'repo_log',
      abapgit_home                  TYPE string VALUE 'abapgit_home',
      abapgit_install               TYPE string VALUE 'abapgit_install',
      zip_import                    TYPE string VALUE 'zip_import',
      zip_export                    TYPE string VALUE 'zip_export',
      zip_package                   TYPE string VALUE 'zip_package',
      zip_transport                 TYPE string VALUE 'zip_transport',
      zip_object                    TYPE string VALUE 'zip_object',
      git_pull                      TYPE string VALUE 'git_pull',
      git_reset                     TYPE string VALUE 'git_reset',
      git_branch_create             TYPE string VALUE 'git_branch_create',
      git_branch_switch             TYPE string VALUE 'git_branch_switch',
      git_branch_delete             TYPE string VALUE 'git_branch_delete',
      git_tag_create                TYPE string VALUE 'git_tag_create',
      git_tag_delete                TYPE string VALUE 'git_tag_delete',
      git_tag_switch                TYPE string VALUE 'git_tag_switch',
      git_commit                    TYPE string VALUE 'git_commit',
      db_display                    TYPE string VALUE 'db_display',
      db_edit                       TYPE string VALUE 'db_edit',
      bg_update                     TYPE string VALUE 'bg_update',
      go_explore                    TYPE string VALUE 'go_explore',
      go_repo_overview              TYPE string VALUE 'go_repo_overview',
      go_db                         TYPE string VALUE 'go_db',
      go_background                 TYPE string VALUE 'go_background',
      go_background_run             TYPE string VALUE 'go_background_run',
      go_diff                       TYPE string VALUE 'go_diff',
      go_stage                      TYPE string VALUE 'go_stage',
      go_commit                     TYPE string VALUE 'go_commit',
      go_branch_overview            TYPE string VALUE 'go_branch_overview',
      go_tag_overview               TYPE string VALUE 'go_tag_overview',
      go_debuginfo                  TYPE string VALUE 'go_debuginfo',
      go_settings                   TYPE string VALUE 'go_settings',
      go_tutorial                   TYPE string VALUE 'go_tutorial',
      go_patch                      TYPE string VALUE 'go_patch',
      jump                          TYPE string VALUE 'jump',
      jump_transport                TYPE string VALUE 'jump_transport',
      url                           TYPE string VALUE 'url',
      goto_source                   TYPE string VALUE 'goto_source',
      show_callstack                TYPE string VALUE 'show_callstack',
      change_order_by               TYPE string VALUE 'change_order_by',
      goto_message                  TYPE string VALUE 'goto_message',
      direction                     TYPE string VALUE 'direction',
    END OF c_action .
  CONSTANTS c_tag_prefix TYPE string VALUE 'refs/tags/' ##NO_TEXT.
  CONSTANTS c_spagpa_param_repo_key TYPE c LENGTH 20 VALUE 'REPO_KEY' ##NO_TEXT.
  CONSTANTS c_spagpa_param_package TYPE c LENGTH 20 VALUE 'PACKAGE' ##NO_TEXT.

  CONSTANTS gc_yes TYPE ty_yes_no VALUE 'Y'.
  CONSTANTS gc_no TYPE ty_yes_no VALUE 'N'.
  CONSTANTS gc_partial TYPE ty_yes_no_partial VALUE 'P'.

  TYPES:
    ty_method TYPE c LENGTH 1 .
  TYPES:
    BEGIN OF ty_stage,
      file   TYPE ty_file,
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

ENDINTERFACE.
