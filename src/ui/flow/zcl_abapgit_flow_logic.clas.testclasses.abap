CLASS lcl_data DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS list_no_blobs_multi
      RETURNING
        VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_file,
            path     TYPE string,
            filename TYPE string,
            data     TYPE xstring,
          END OF ty_file.
    TYPES: BEGIN OF ty_branches,
             branch TYPE string,
             files  TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY,
           END OF ty_branches.
    DATA mt_branches TYPE STANDARD TABLE OF ty_branches WITH DEFAULT KEY.
ENDCLASS.

CLASS lcl_data IMPLEMENTATION.
  METHOD list_no_blobs_multi.
* assume: for all current branches

    DATA ls_branch LIKE LINE OF mt_branches.
    DATA lt_paths TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
    DATA ls_file LIKE LINE OF ls_branch-files.

    LOOP AT mt_branches INTO ls_branch.
      CLEAR lt_paths.
      LOOP AT ls_branch-files INTO ls_file.
        INSERT ls_file-path INTO TABLE lt_paths.
      ENDLOOP.
      INSERT |/| INTO TABLE lt_paths.

      LOOP AT lt_paths INTO lv_path.
* todo
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.
    DATA ls_main TYPE ty_branches.
    DATA ls_file LIKE LINE OF ls_main-files.

    ls_main-branch = zif_abapgit_git_definitions=>c_git_branch-heads_prefix && zif_abapgit_flow_logic=>c_main.

    ls_file-path = '/'.
    ls_file-filename = 'README.md'.
    ls_file-data = '001122333'.
    INSERT ls_file INTO TABLE ls_main-files.

    INSERT ls_main INTO TABLE mt_branches.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_branch_list DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_git_branch_list.
ENDCLASS.

CLASS lcl_branch_list IMPLEMENTATION.
  METHOD zif_abapgit_git_branch_list~find_by_name.
    ASSERT 1 = 2.
  ENDMETHOD.
  METHOD zif_abapgit_git_branch_list~get_head_symref.
    ASSERT 1 = 2.
  ENDMETHOD.
  METHOD zif_abapgit_git_branch_list~get_all.
    DATA ls_branch LIKE LINE OF rt_branches.

    ls_branch-display_name = zif_abapgit_flow_logic=>c_main.
    ls_branch-sha1 = '11111111111111111111111111111111'.
    INSERT ls_branch INTO TABLE rt_branches.
  ENDMETHOD.
  METHOD zif_abapgit_git_branch_list~get_branches_only.
    ASSERT 1 = 2.
  ENDMETHOD.
  METHOD zif_abapgit_git_branch_list~get_tags_only.
    ASSERT 1 = 2.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_gitv2 DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_gitv2_porcelain.

    METHODS constructor
      IMPORTING
        io_data TYPE REF TO lcl_data.

  PRIVATE SECTION.
    DATA mo_data TYPE REF TO lcl_data.
ENDCLASS.

CLASS lcl_gitv2 IMPLEMENTATION.
  METHOD constructor.
    mo_data = io_data.
  ENDMETHOD.
  METHOD zif_abapgit_gitv2_porcelain~list_branches.
    CREATE OBJECT ro_list TYPE lcl_branch_list.
  ENDMETHOD.
  METHOD zif_abapgit_gitv2_porcelain~list_no_blobs.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_gitv2_porcelain~list_no_blobs_multi.
    rt_objects = mo_data->list_no_blobs_multi( ).
  ENDMETHOD.
  METHOD zif_abapgit_gitv2_porcelain~commits_last_year.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_gitv2_porcelain~fetch_blob.
    RETURN.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_sap_package DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_sap_package.
ENDCLASS.

CLASS lcl_sap_package IMPLEMENTATION.
  METHOD zif_abapgit_sap_package~get.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_sap_package~validate_name.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_sap_package~create.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_sap_package~create_local.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_sap_package~list_subpackages.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_sap_package~list_superpackages.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_sap_package~read_parent.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_sap_package~read_description.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_sap_package~read_responsible.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_sap_package~create_child.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_sap_package~exists.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_sap_package~are_changes_recorded_in_tr_req.
    rv_are_changes_rec_in_tr_req = abap_true.
  ENDMETHOD.
  METHOD zif_abapgit_sap_package~get_transport_type.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_sap_package~get_default_transport_layer.
    RETURN.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_repo DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_repo.
    INTERFACES zif_abapgit_repo_online.

    CONSTANTS c_package TYPE string VALUE 'ZFLOWTESTTEST'.
ENDCLASS.

CLASS lcl_repo IMPLEMENTATION.
  METHOD zif_abapgit_repo~get_key.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_name.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~is_offline.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_package.
    rv_package = c_package.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_local_settings.
    rs_settings-flow = abap_true.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_tadir_objects.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_files_local_filtered.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_files_local.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_files_remote.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~refresh.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_dot_abapgit.
    DATA ls_data TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit.
    ls_data-starting_folder = '/src/'.
    CREATE OBJECT ro_dot_abapgit
      EXPORTING
        is_data = ls_data.
  ENDMETHOD.
  METHOD zif_abapgit_repo~set_dot_abapgit.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~find_remote_dot_abapgit.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~deserialize.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~deserialize_checks.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~checksums.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~has_remote_source.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_log.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~create_new_log.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_dot_apack.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~delete_checks.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~set_files_remote.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_unsupported_objects_local.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~set_local_settings.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~switch_repo_type.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~refresh_local_object.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~refresh_local_objects.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_data_config.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~bind_listener.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo~remove_ignored_files.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~get_url.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~get_selected_branch.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~set_url.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~select_branch.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~get_selected_commit.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~get_current_remote.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~select_commit.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~switch_origin.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~get_switched_origin.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~push.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~create_branch.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~check_for_valid_branch.
    RETURN.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_repo_srv DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_repo_srv.

    METHODS constructor IMPORTING io_repo TYPE REF TO lcl_repo.
  PRIVATE SECTION.
    DATA mi_repo TYPE REF TO lcl_repo.
ENDCLASS.

CLASS lcl_repo_srv IMPLEMENTATION.
  METHOD constructor.
    mi_repo = io_repo.
  ENDMETHOD.

  METHOD zif_abapgit_repo_srv~init.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~delete.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~get.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~is_repo_installed.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~list.
    INSERT mi_repo INTO TABLE rt_list.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~list_favorites.
    INSERT mi_repo INTO TABLE rt_list.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~new_offline.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~new_online.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~purge.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~validate_package.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~validate_url.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~get_repo_from_package.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~get_repo_from_url.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~get_label_list.
    RETURN.
  ENDMETHOD.
ENDCLASS.

************************************************

CLASS ltcl_flow_logic DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
  PUBLIC SECTION.
    METHODS inject RETURNING VALUE(ro_data) TYPE REF TO lcl_data.
    METHODS teardown.
    METHODS nothing FOR TESTING RAISING cx_static_check.
    METHODS only_transport FOR TESTING RAISING cx_static_check.
    METHODS only_branch FOR TESTING RAISING cx_static_check.
    METHODS branch_and_transport FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_flow_logic IMPLEMENTATION.

  METHOD inject.
    DATA lo_repo        TYPE REF TO lcl_repo.
    DATA lo_repo_srv    TYPE REF TO lcl_repo_srv.
    DATA lo_sap_package TYPE REF TO lcl_sap_package.
    DATA lo_gitv2       TYPE REF TO lcl_gitv2.


    CREATE OBJECT ro_data.
    CREATE OBJECT lo_repo.
    CREATE OBJECT lo_sap_package.
    CREATE OBJECT lo_gitv2 EXPORTING io_data = ro_data.
    CREATE OBJECT lo_repo_srv EXPORTING io_repo = lo_repo.

    zcl_abapgit_repo_srv=>inject_instance( lo_repo_srv ).

    zcl_abapgit_injector=>set_sap_package(
      iv_package     = lcl_repo=>c_package
      ii_sap_package = lo_sap_package ).

    zcl_abapgit_git_injector=>set_v2_porcelain( lo_gitv2 ).
  ENDMETHOD.

  METHOD teardown.
    DATA lo_sap_package TYPE REF TO zif_abapgit_sap_package.

    zcl_abapgit_repo_srv=>inject_instance( ).

    zcl_abapgit_injector=>set_sap_package(
      iv_package     = lcl_repo=>c_package
      ii_sap_package = lo_sap_package ).

    zcl_abapgit_git_injector=>set_v2_porcelain( ).
  ENDMETHOD.

  METHOD nothing.

    DATA lt_features TYPE zif_abapgit_flow_logic=>ty_features.

    lt_features = zcl_abapgit_flow_logic=>get( ).
    cl_abap_unit_assert=>assert_initial( lt_features ).

* todo

  ENDMETHOD.

  METHOD only_transport.
    RETURN. " todo, implement method
  ENDMETHOD.

  METHOD only_branch.
    RETURN. " todo, implement method
  ENDMETHOD.

  METHOD branch_and_transport.
    RETURN. " todo, implement method
  ENDMETHOD.

ENDCLASS.
