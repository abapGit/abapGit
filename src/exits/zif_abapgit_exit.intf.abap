INTERFACE zif_abapgit_exit PUBLIC.

  TYPES:
    BEGIN OF ty_ci_repo,
      name      TYPE string,
      clone_url TYPE string,
    END OF ty_ci_repo.

  TYPES ty_ci_repos TYPE STANDARD TABLE OF ty_ci_repo WITH DEFAULT KEY.

  TYPES ty_object_types TYPE HASHED TABLE OF tadir-object WITH UNIQUE KEY table_line.

  TYPES:
    BEGIN OF ty_class_key,
      clsname TYPE abap_classname,
    END OF ty_class_key.

  METHODS adjust_display_commit_url
    IMPORTING
      !iv_repo_url    TYPE csequence
      !iv_repo_name   TYPE csequence
      !iv_repo_key    TYPE csequence
      !iv_commit_hash TYPE zif_abapgit_git_definitions=>ty_sha1
    CHANGING
      !cv_display_url TYPE csequence
    RAISING
      zcx_abapgit_exception.

  METHODS adjust_display_filename
    IMPORTING
      !is_repo_meta      TYPE zif_abapgit_persistence=>ty_repo
      !iv_filename       TYPE string
    RETURNING
      VALUE(rv_filename) TYPE string.

  METHODS allow_sap_objects
    RETURNING
      VALUE(rv_allowed) TYPE abap_bool.

  METHODS change_local_host
    CHANGING
      !ct_hosts TYPE zif_abapgit_definitions=>ty_string_tt.

  METHODS change_max_parallel_processes
    IMPORTING
      !iv_package       TYPE devclass
    CHANGING
      !cv_max_processes TYPE i.

  METHODS change_proxy_authentication
    IMPORTING
      !iv_repo_url             TYPE csequence
    CHANGING
      !cv_proxy_authentication TYPE abap_bool.

  METHODS change_proxy_port
    IMPORTING
      !iv_repo_url   TYPE csequence
    CHANGING
      !cv_proxy_port TYPE string.

  METHODS change_proxy_url
    IMPORTING
      !iv_repo_url  TYPE csequence
    CHANGING
      !cv_proxy_url TYPE string.

  METHODS change_rfc_server_group
    CHANGING
      !cv_group TYPE rzlli_apcl.

  METHODS change_supported_data_objects
    CHANGING
      !ct_objects TYPE zif_abapgit_data_supporter=>ty_objects.

  METHODS change_supported_object_types
    CHANGING
      !ct_types TYPE ty_object_types.

  METHODS change_tadir
    IMPORTING
      !iv_package            TYPE devclass
      !ii_log                TYPE REF TO zif_abapgit_log
      !is_dot_abapgit        TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit
      !iv_ignore_subpackages TYPE abap_bool DEFAULT abap_false
      !iv_only_local_objects TYPE abap_bool DEFAULT abap_false
    CHANGING
      !ct_tadir              TYPE zif_abapgit_definitions=>ty_tadir_tt.

  METHODS create_http_client
    IMPORTING
      !iv_url          TYPE string
    RETURNING
      VALUE(ri_client) TYPE REF TO if_http_client
    RAISING
      zcx_abapgit_exception.

  METHODS custom_serialize_abap_clif
    IMPORTING
      !is_class_key    TYPE ty_class_key
      !it_source       TYPE zif_abapgit_definitions=>ty_string_tt OPTIONAL
    RETURNING
      VALUE(rt_source) TYPE zif_abapgit_definitions=>ty_string_tt
    RAISING
      zcx_abapgit_exception.

  METHODS deserialize_postprocess
    IMPORTING
      !is_step TYPE zif_abapgit_objects=>ty_step_data
      !ii_log  TYPE REF TO zif_abapgit_log.

  METHODS determine_transport_request
    IMPORTING
      !ii_repo              TYPE REF TO zif_abapgit_repo
      !iv_transport_type    TYPE zif_abapgit_definitions=>ty_transport_type
    CHANGING
      !cv_transport_request TYPE trkorr.

  METHODS enhance_any_toolbar
    IMPORTING
      !io_menu TYPE REF TO zcl_abapgit_html_toolbar.

  METHODS enhance_repo_toolbar
    IMPORTING
      !io_menu TYPE REF TO zcl_abapgit_html_toolbar
      !iv_key  TYPE zif_abapgit_persistence=>ty_value
      !iv_act  TYPE string.

  METHODS get_ci_tests
    IMPORTING
      !iv_object   TYPE tadir-object
    CHANGING
      !ct_ci_repos TYPE ty_ci_repos.

  METHODS get_ssl_id
    RETURNING
      VALUE(rv_ssl_id) TYPE ssfapplssl.

  METHODS http_client
    IMPORTING
      !iv_url    TYPE string
      !ii_client TYPE REF TO if_http_client.

  METHODS on_event
    IMPORTING
      !ii_event         TYPE REF TO zif_abapgit_gui_event
    RETURNING
      VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
    RAISING
      zcx_abapgit_exception.

  METHODS pre_calculate_repo_status
    IMPORTING
      !is_repo_meta TYPE zif_abapgit_persistence=>ty_repo
    CHANGING
      !ct_local     TYPE zif_abapgit_definitions=>ty_files_item_tt
      !ct_remote    TYPE zif_abapgit_git_definitions=>ty_files_tt
    RAISING
      zcx_abapgit_exception.

  METHODS serialize_postprocess
    IMPORTING
      !iv_package TYPE devclass
      !ii_log     TYPE REF TO zif_abapgit_log
    CHANGING
      !ct_files   TYPE zif_abapgit_definitions=>ty_files_item_tt.

  METHODS validate_before_push
    IMPORTING
      !is_comment     TYPE zif_abapgit_git_definitions=>ty_comment
      !io_stage       TYPE REF TO zcl_abapgit_stage
      !ii_repo_online TYPE REF TO zif_abapgit_repo_online
    RAISING
      zcx_abapgit_exception.

  METHODS wall_message_list
    IMPORTING
      !ii_html TYPE REF TO zif_abapgit_html.

  METHODS wall_message_repo
    IMPORTING
      !is_repo_meta TYPE zif_abapgit_persistence=>ty_repo
      !ii_html      TYPE REF TO zif_abapgit_html.

  METHODS change_committer_info
    IMPORTING
      iv_repo_url TYPE csequence
    CHANGING
      cv_name     TYPE csequence
      cv_email    TYPE csequence.
ENDINTERFACE.
