*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_DEFINITIONS
*&---------------------------------------------------------------------*

TYPE-POOLS seop.

TYPES: ty_type    TYPE c LENGTH 6,
       ty_bitbyte TYPE c LENGTH 8,
       ty_sha1    TYPE c LENGTH 40.

TYPES: BEGIN OF ty_file_signature,
         path     TYPE string,
         filename TYPE string,
         sha1     TYPE ty_sha1,
       END OF ty_file_signature.

TYPES: ty_file_signatures_tt TYPE STANDARD TABLE OF
         ty_file_signature WITH DEFAULT KEY.

TYPES: ty_file_signatures_ts TYPE SORTED TABLE OF
         ty_file_signature WITH UNIQUE KEY path filename.

TYPES: BEGIN OF ty_file.
    INCLUDE TYPE ty_file_signature.
TYPES: data TYPE xstring,
       END OF ty_file.
TYPES: ty_files_tt TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY.

TYPES: ty_string_tt TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

TYPES: BEGIN OF ty_comment,
         username TYPE string,
         email    TYPE string,
         comment  TYPE string,
       END OF ty_comment.

TYPES: BEGIN OF ty_item,
         obj_type TYPE tadir-object,
         obj_name TYPE tadir-obj_name,
         devclass TYPE devclass,
       END OF ty_item,
       ty_items_tt TYPE STANDARD TABLE OF ty_item WITH DEFAULT KEY,
       ty_items_ts TYPE SORTED TABLE OF ty_item WITH UNIQUE KEY obj_type obj_name.

TYPES: BEGIN OF ty_file_item,
         file TYPE ty_file,
         item TYPE ty_item,
       END OF ty_file_item.
TYPES: ty_files_item_tt TYPE STANDARD TABLE OF ty_file_item WITH DEFAULT KEY.

TYPES: BEGIN OF ty_metadata,
         class        TYPE string,
         version      TYPE string,
         late_deser   TYPE abap_bool,
         delete_tadir TYPE abap_bool,
         ddic         TYPE abap_bool,
       END OF ty_metadata.

TYPES: BEGIN OF ty_web_asset,
         url     TYPE w3url,
         base64  TYPE string,
         content TYPE xstring,
       END OF ty_web_asset.
TYPES  tt_web_assets TYPE STANDARD TABLE OF ty_web_asset WITH DEFAULT KEY.

TYPES: BEGIN OF ty_repo_file,
         path       TYPE string,
         filename   TYPE string,
         is_changed TYPE abap_bool,
         rstate     TYPE char1,
         lstate     TYPE char1,
       END OF ty_repo_file.
TYPES  tt_repo_files TYPE STANDARD TABLE OF ty_repo_file WITH DEFAULT KEY.

TYPES: BEGIN OF ty_stage_files,
         local  TYPE ty_files_item_tt,
         remote TYPE ty_files_tt,
       END OF ty_stage_files.

CONSTANTS: BEGIN OF gc_type,
             commit TYPE ty_type VALUE 'commit',            "#EC NOTEXT
             tree   TYPE ty_type VALUE 'tree',              "#EC NOTEXT
             ref_d  TYPE ty_type VALUE 'ref_d',             "#EC NOTEXT
             blob   TYPE ty_type VALUE 'blob',              "#EC NOTEXT
           END OF gc_type.

TYPES: ty_chmod TYPE c LENGTH 6.

TYPES: BEGIN OF ty_object,
         sha1 TYPE ty_sha1,
         type TYPE ty_type,
         data TYPE xstring,
       END OF ty_object.
TYPES: ty_objects_tt TYPE STANDARD TABLE OF ty_object WITH DEFAULT KEY.

TYPES: BEGIN OF ty_tadir,
         pgmid    TYPE tadir-pgmid,
         object   TYPE tadir-object,
         obj_name TYPE tadir-obj_name,
         devclass TYPE tadir-devclass,
         korrnum  TYPE tadir-korrnum,
         path     TYPE string,
       END OF ty_tadir.
TYPES: ty_tadir_tt TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY.

TYPES: BEGIN OF ty_result,
         obj_type TYPE tadir-object,
         obj_name TYPE tadir-obj_name,
         path     TYPE string,
         filename TYPE string,
         package  TYPE devclass,
         match    TYPE sap_bool,
         lstate   TYPE char1,
         rstate   TYPE char1,
       END OF ty_result.
TYPES: ty_results_tt TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY.

TYPES: ty_sval_tt TYPE STANDARD TABLE OF sval WITH DEFAULT KEY.

TYPES: ty_seocompotx_tt TYPE STANDARD TABLE OF seocompotx WITH DEFAULT KEY.

TYPES: BEGIN OF ty_tpool.
    INCLUDE TYPE textpool.
TYPES:   split TYPE c LENGTH 8.
TYPES: END OF ty_tpool.

TYPES: ty_tpool_tt TYPE STANDARD TABLE OF ty_tpool WITH DEFAULT KEY.

TYPES: BEGIN OF ty_sotr,
         header  TYPE sotr_head,
         entries TYPE sotr_text_tt,
       END OF ty_sotr.

TYPES: ty_sotr_tt TYPE STANDARD TABLE OF ty_sotr WITH DEFAULT KEY.

CONSTANTS: BEGIN OF gc_state, " https://git-scm.com/docs/git-status
             unchanged TYPE char1 VALUE '',
             added     TYPE char1 VALUE 'A',
             modified  TYPE char1 VALUE 'M',
             deleted   TYPE char1 VALUE 'D', "For future use
             mixed     TYPE char1 VALUE '*',
           END OF gc_state.

CONSTANTS: BEGIN OF gc_chmod,
             file       TYPE ty_chmod VALUE '100644',
             executable TYPE ty_chmod VALUE '100755',
             dir        TYPE ty_chmod VALUE '40000 ',
           END OF gc_chmod.

CONSTANTS: BEGIN OF gc_event_state,
             not_handled         VALUE 0,
             re_render           VALUE 1,
             new_page            VALUE 2,
             go_back             VALUE 3,
             no_more_act         VALUE 4,
             new_page_w_bookmark VALUE 5,
             go_back_to_bookmark VALUE 6,
             new_page_replacing  VALUE 7,
           END OF gc_event_state.

CONSTANTS: BEGIN OF gc_html_opt,
             strong   TYPE c VALUE 'E',
             cancel   TYPE c VALUE 'C',
             crossout TYPE c VALUE 'X',
           END OF gc_html_opt.

CONSTANTS: BEGIN OF gc_action_type,
             sapevent TYPE c VALUE 'E',
             url      TYPE c VALUE 'U',
             onclick  TYPE c VALUE 'C',
           END OF gc_action_type.

CONSTANTS: gc_newline TYPE abap_char1 VALUE cl_abap_char_utilities=>newline.

CONSTANTS: gc_english TYPE spras VALUE 'E'.

CONSTANTS: gc_root_dir    TYPE string VALUE '/',
           gc_dot_abapgit TYPE string VALUE '.abapgit.xml' ##NO_TEXT,
           gc_author_regex TYPE string VALUE '^([\w\s\.@\-_1-9]+) <(.*)> (\d{10}) .\d{4}$' ##NO_TEXT.

CONSTANTS: BEGIN OF gc_action,
             repo_clone             TYPE string VALUE 'repo_clone',
             repo_refresh           TYPE string VALUE 'repo_refresh',
             repo_remove            TYPE string VALUE 'repo_remove',
             repo_purge             TYPE string VALUE 'repo_purge',
             repo_newoffline        TYPE string VALUE 'repo_newoffline',
             repo_remote_attach     TYPE string VALUE 'repo_remote_attach',
             repo_remote_detach     TYPE string VALUE 'repo_remote_detach',
             repo_remote_change     TYPE string VALUE 'repo_remote_change',
             repo_refresh_checksums TYPE string VALUE 'repo_refresh_checksums',
             repo_toggle_fav        TYPE string VALUE 'repo_toggle_fav',

             abapgit_home           TYPE string VALUE 'abapgit_home',
             abapgit_wiki           TYPE string VALUE 'abapgit_wiki',
             abapgit_install        TYPE string VALUE 'abapgit_install',
             abapgit_install_pi     TYPE string VALUE 'abapgit_install_pi',

             zip_import             TYPE string VALUE 'zip_import',
             zip_export             TYPE string VALUE 'zip_export',
             zip_package            TYPE string VALUE 'zip_package',
             zip_transport          TYPE string VALUE 'zip_transport',
             zip_object             TYPE string VALUE 'zip_object',

             git_pull               TYPE string VALUE 'git_pull',
             git_reset              TYPE string VALUE 'git_reset',
             git_branch_create      TYPE string VALUE 'git_branch_create',
             git_branch_switch      TYPE string VALUE 'git_branch_switch',
             git_branch_delete      TYPE string VALUE 'git_branch_delete',
             git_commit             TYPE string VALUE 'git_commit',

             db_delete              TYPE string VALUE 'db_delete',
             db_update              TYPE string VALUE 'db_update',
             db_display             TYPE string VALUE 'db_display',
             db_edit                TYPE string VALUE 'db_edit',
             bg_update              TYPE string VALUE 'bg_update',

             go_main                TYPE string VALUE 'go_main',
             go_explore             TYPE string VALUE 'go_explore',
             go_db                  TYPE string VALUE 'go_db',
             go_background          TYPE string VALUE 'go_background',
             go_background_run      TYPE string VALUE 'go_background_run',
             go_diff                TYPE string VALUE 'go_diff',
             go_stage               TYPE string VALUE 'go_stage',
             go_commit              TYPE string VALUE 'go_commit',
             go_branch_overview     TYPE string VALUE 'go_branch_overview',
             go_playground          TYPE string VALUE 'go_playground',
             go_debuginfo           TYPE string VALUE 'go_debuginfo',
             go_settings            TYPE string VALUE 'go_settings',
             go_tutorial            TYPE string VALUE 'go_tutorial',
             jump                   TYPE string VALUE 'jump',
             jump_pkg               TYPE string VALUE 'jump_pkg',
           END OF gc_action.