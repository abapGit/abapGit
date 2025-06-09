CLASS lcl_data DEFINITION FINAL.
  PUBLIC SECTION.
    CONSTANTS c_branch_name TYPE string VALUE 'branch'.
    CONSTANTS c_filename TYPE string VALUE 'zcl_foobar.clas.abap'.
    CONSTANTS c_devclass TYPE devclass VALUE 'ZFOOBAR'.

    METHODS constructor RAISING zcx_abapgit_exception.

    METHODS add_branch RAISING zcx_abapgit_exception.
    METHODS add_main RAISING zcx_abapgit_exception.
    METHODS add_transport RAISING zcx_abapgit_exception.

*******************************

    METHODS list_no_blobs_multi
      RETURNING
        VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception.

    METHODS list_branches
      RETURNING
        VALUE(rt_branches) TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
      RAISING
        zcx_abapgit_exception.

    METHODS list_open_requests
      RETURNING
        VALUE(rt_trkorr) TYPE zif_abapgit_cts_api=>ty_trkorr_tt
      RAISING
        zcx_abapgit_exception.

    METHODS list_r3tr_by_request
      IMPORTING
        !iv_request    TYPE trkorr
      RETURNING
        VALUE(rt_list) TYPE zif_abapgit_cts_api=>ty_transport_obj_tt
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_file,
            filename TYPE string,
            data     TYPE xstring,
          END OF ty_file.
    TYPES: BEGIN OF ty_branches,
             display_name TYPE string,
             files        TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY,
             sha1         TYPE zif_abapgit_git_definitions=>ty_sha1,
             objects      TYPE zif_abapgit_definitions=>ty_objects_tt,
           END OF ty_branches.
    DATA mt_branches TYPE STANDARD TABLE OF ty_branches WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_transports,
             trkorr  TYPE trkorr,
             objects TYPE zif_abapgit_cts_api=>ty_transport_obj_tt,
           END OF ty_transports.
    DATA mt_transports TYPE STANDARD TABLE OF ty_transports WITH DEFAULT KEY.

    METHODS encode RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS lcl_data IMPLEMENTATION.
  METHOD list_r3tr_by_request.
    DATA ls_transport LIKE LINE OF mt_transports.
    READ TABLE mt_transports INTO ls_transport WITH KEY trkorr = iv_request.
    ASSERT sy-subrc = 0.

    rt_list = ls_transport-objects.
  ENDMETHOD.

  METHOD list_open_requests.
    DATA lv_trkorr LIKE LINE OF rt_trkorr.
    DATA ls_transport LIKE LINE OF mt_transports.

    LOOP AT mt_transports INTO ls_transport.
      lv_trkorr = ls_transport-trkorr.
      INSERT lv_trkorr INTO TABLE rt_trkorr.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_transport.
    DATA ls_transport LIKE LINE OF mt_transports.
    DATA ls_object    LIKE LINE OF ls_transport-objects.

    ls_transport-trkorr = 'ABC0123456'.

    ls_object-object = 'CLAS'.
    ls_object-obj_name = 'ZCL_FOOBAR'.
    INSERT ls_object INTO TABLE ls_transport-objects.

    INSERT ls_transport INTO TABLE mt_transports.
  ENDMETHOD.

  METHOD list_branches.
    DATA ls_result LIKE LINE OF rt_branches.
    DATA ls_branch LIKE LINE OF mt_branches.

    LOOP AT mt_branches INTO ls_branch.
      ls_result-display_name = ls_branch-display_name.
      ls_result-sha1 = ls_branch-sha1.
      INSERT ls_result INTO TABLE rt_branches.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_main.

    DATA ls_main TYPE ty_branches.
    DATA ls_file LIKE LINE OF ls_main-files.

    ls_main-display_name = zif_abapgit_flow_logic=>c_main.

    ls_file-filename = c_filename.
    ls_file-data = '001122333'.
    INSERT ls_file INTO TABLE ls_main-files.

    INSERT ls_main INTO TABLE mt_branches.

    encode( ).
  ENDMETHOD.

  METHOD add_branch.

    DATA ls_main TYPE ty_branches.
    DATA ls_file LIKE LINE OF ls_main-files.

    ls_main-display_name = c_branch_name.

    ls_file-filename = c_filename.
    ls_file-data = '33221100'.
    INSERT ls_file INTO TABLE ls_main-files.

    INSERT ls_main INTO TABLE mt_branches.

    encode( ).
  ENDMETHOD.

  METHOD list_no_blobs_multi.
* assume: all branches

    DATA ls_branch LIKE LINE OF mt_branches.
    DATA ls_object LIKE LINE OF rt_objects.

    LOOP AT mt_branches INTO ls_branch.
      LOOP AT ls_branch-objects INTO ls_object WHERE type <> zif_abapgit_git_definitions=>c_type-blob.
        INSERT ls_object INTO TABLE rt_objects.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD encode.
* assume: for all current branches

    DATA ls_file   TYPE ty_file.
    DATA lt_nodes  TYPE zcl_abapgit_git_pack=>ty_nodes_tt.
    DATA ls_node   LIKE LINE OF lt_nodes.
    DATA ls_object TYPE zif_abapgit_definitions=>ty_object.
    DATA ls_commit TYPE zcl_abapgit_git_pack=>ty_commit.

    FIELD-SYMBOLS <ls_branch> LIKE LINE OF mt_branches.

    LOOP AT mt_branches ASSIGNING <ls_branch>.
      CLEAR <ls_branch>-objects.

      LOOP AT <ls_branch>-files INTO ls_file.
        ls_node-chmod = zif_abapgit_git_definitions=>c_chmod-file.
        ls_node-name = ls_file-filename.
        ls_node-sha1 = zcl_abapgit_hash=>sha1_raw( ls_file-data ).

        ls_object-type = zif_abapgit_git_definitions=>c_type-blob.
        ls_object-data = ls_file-data.
        ls_object-sha1 = ls_node-sha1.
        INSERT ls_object INTO TABLE <ls_branch>-objects.
      ENDLOOP.

      ls_object-type = zif_abapgit_git_definitions=>c_type-tree.
      ls_object-data = zcl_abapgit_git_pack=>encode_tree( lt_nodes ).
      ls_object-sha1 = zcl_abapgit_hash=>sha1_raw( ls_object-data ).
      INSERT ls_object INTO TABLE <ls_branch>-objects.

      ls_commit-tree = ls_object-sha1.
      ls_commit-author = 'John Doe'.
      ls_commit-committer = 'John Doe'.
      ls_commit-body = 'hello world'.

      ls_object-type = zif_abapgit_git_definitions=>c_type-commit.
      ls_object-data = zcl_abapgit_git_pack=>encode_commit( ls_commit ).
      ls_object-sha1 = zcl_abapgit_hash=>sha1_raw( ls_object-data ).
      INSERT ls_object INTO TABLE <ls_branch>-objects.

      <ls_branch>-sha1 = ls_object-sha1.
    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.
    add_main( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_cts DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_cts_api.
    METHODS constructor
      IMPORTING
        io_data TYPE REF TO lcl_data.
  PRIVATE SECTION.
    DATA mo_data TYPE REF TO lcl_data.
ENDCLASS.

CLASS lcl_cts IMPLEMENTATION.
  METHOD constructor.
    mo_data = io_data.
  ENDMETHOD.
  METHOD zif_abapgit_cts_api~confirm_transport_messages.
    RETURN. " todo, implement method
  ENDMETHOD.
  METHOD zif_abapgit_cts_api~create_transport_entries.
    RETURN. " todo, implement method
  ENDMETHOD.
  METHOD zif_abapgit_cts_api~get_r3tr_obj_for_limu_obj.
    RETURN. " todo, implement method
  ENDMETHOD.
  METHOD zif_abapgit_cts_api~get_transports_for_list.
    RETURN. " todo, implement method
  ENDMETHOD.
  METHOD zif_abapgit_cts_api~get_transport_for_object.
    RETURN. " todo, implement method
  ENDMETHOD.
  METHOD zif_abapgit_cts_api~insert_transport_object.
    RETURN. " todo, implement method
  ENDMETHOD.
  METHOD zif_abapgit_cts_api~is_chrec_possible_for_package.
    RETURN. " todo, implement method
  ENDMETHOD.
  METHOD zif_abapgit_cts_api~list_open_requests_by_user.
    RETURN. " todo, implement method
  ENDMETHOD.
  METHOD zif_abapgit_cts_api~list_open_requests.
    rt_trkorr = mo_data->list_open_requests( ).
  ENDMETHOD.
  METHOD zif_abapgit_cts_api~list_r3tr_by_request.
    rt_list = mo_data->list_r3tr_by_request( iv_request ).
  ENDMETHOD.
  METHOD zif_abapgit_cts_api~read.
    RETURN. " todo, implement method
  ENDMETHOD.
  METHOD zif_abapgit_cts_api~read_description.
    RETURN. " todo, implement method
  ENDMETHOD.
  METHOD zif_abapgit_cts_api~read_user.
    RETURN. " todo, implement method
  ENDMETHOD.
  METHOD zif_abapgit_cts_api~validate_transport_request.
    RETURN. " todo, implement method
  ENDMETHOD.
  METHOD zif_abapgit_cts_api~change_transport_type.
    RETURN. " todo, implement method
  ENDMETHOD.
ENDCLASS.

CLASS lcl_branch_list DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_git_branch_list.
    METHODS constructor
      IMPORTING
        io_data TYPE REF TO lcl_data.
  PRIVATE SECTION.
    DATA mo_data TYPE REF TO lcl_data.
ENDCLASS.

CLASS lcl_branch_list IMPLEMENTATION.
  METHOD constructor.
    mo_data = io_data.
  ENDMETHOD.
  METHOD zif_abapgit_git_branch_list~find_by_name.
    ASSERT 1 = 2.
  ENDMETHOD.
  METHOD zif_abapgit_git_branch_list~get_head_symref.
    ASSERT 1 = 2.
  ENDMETHOD.
  METHOD zif_abapgit_git_branch_list~get_all.
    rt_branches = mo_data->list_branches( ).
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
    CREATE OBJECT ro_list TYPE lcl_branch_list EXPORTING io_data = mo_data.
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
    rv_package = lcl_data=>c_devclass.
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
    ls_data-starting_folder = '/'.
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

CLASS lcl_tadir DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_tadir.
    METHODS constructor
      IMPORTING
        io_data TYPE REF TO lcl_data.
  PRIVATE SECTION.
    DATA mo_data TYPE REF TO lcl_data.
ENDCLASS.

CLASS lcl_tadir IMPLEMENTATION.

  METHOD constructor.
    mo_data = io_data.
  ENDMETHOD.

  METHOD zif_abapgit_tadir~get_object_package.
    RETURN. " todo, implement method
  ENDMETHOD.
  METHOD zif_abapgit_tadir~read.
    RETURN. " todo, implement method
  ENDMETHOD.
  METHOD zif_abapgit_tadir~read_single.
    rs_tadir-devclass = lcl_data=>c_devclass.
  ENDMETHOD.

ENDCLASS.

************************************************

CLASS ltcl_flow_logic DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
  PUBLIC SECTION.
    METHODS no_transports_no_branches FOR TESTING RAISING cx_static_check.
    METHODS only_branch FOR TESTING RAISING cx_static_check.
    METHODS only_transport FOR TESTING RAISING cx_static_check.
    METHODS branch_and_transport FOR TESTING RAISING cx_static_check.

  PRIVATE SECTION.
    METHODS inject
      RETURNING
        VALUE(ro_data) TYPE REF TO lcl_data
      RAISING
        cx_static_check.

    METHODS teardown.
ENDCLASS.

CLASS ltcl_flow_logic IMPLEMENTATION.

  METHOD inject.
    DATA lo_repo        TYPE REF TO lcl_repo.
    DATA lo_repo_srv    TYPE REF TO lcl_repo_srv.
    DATA lo_sap_package TYPE REF TO lcl_sap_package.
    DATA lo_gitv2       TYPE REF TO lcl_gitv2.
    DATA lo_tadir       TYPE REF TO lcl_tadir.
    DATA lo_cts         TYPE REF TO lcl_cts.

    CREATE OBJECT ro_data.
    CREATE OBJECT lo_repo.
    CREATE OBJECT lo_sap_package.
    CREATE OBJECT lo_tadir EXPORTING io_data = ro_data.
    CREATE OBJECT lo_cts EXPORTING io_data = ro_data.
    CREATE OBJECT lo_gitv2 EXPORTING io_data = ro_data.
    CREATE OBJECT lo_repo_srv EXPORTING io_repo = lo_repo.

    zcl_abapgit_repo_srv=>inject_instance( lo_repo_srv ).

    zcl_abapgit_injector=>set_sap_package(
      iv_package     = lcl_data=>c_devclass
      ii_sap_package = lo_sap_package ).

    zcl_abapgit_injector=>set_cts_api( lo_cts ).
    zcl_abapgit_injector=>set_tadir( lo_tadir ).

    zcl_abapgit_git_injector=>set_v2_porcelain( lo_gitv2 ).
  ENDMETHOD.

  METHOD teardown.
    DATA lo_sap_package TYPE REF TO zif_abapgit_sap_package.

    zcl_abapgit_repo_srv=>inject_instance( ).

    zcl_abapgit_injector=>set_sap_package(
      iv_package     = lcl_data=>c_devclass
      ii_sap_package = lo_sap_package ).

    zcl_abapgit_git_injector=>set_v2_porcelain( ).
  ENDMETHOD.

  METHOD no_transports_no_branches.

    DATA lt_features TYPE zif_abapgit_flow_logic=>ty_features.

    inject( ).
    lt_features = zcl_abapgit_flow_logic=>get( )-features.
    cl_abap_unit_assert=>assert_initial( lt_features ).

  ENDMETHOD.

  METHOD only_branch.
    DATA lo_data     TYPE REF TO lcl_data.
    DATA lt_features TYPE zif_abapgit_flow_logic=>ty_features.
    DATA ls_feature  LIKE LINE OF lt_features.

    lo_data = inject( ).
    lo_data->add_branch( ).

    lt_features = zcl_abapgit_flow_logic=>get( )-features.
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_features )
      exp = 1 ).

    READ TABLE lt_features INDEX 1 INTO ls_feature.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_feature-branch-display_name
      exp = lcl_data=>c_branch_name ).
    cl_abap_unit_assert=>assert_initial( ls_feature-transport-trkorr ).
  ENDMETHOD.

  METHOD only_transport.
    DATA lo_data     TYPE REF TO lcl_data.
    DATA lt_features TYPE zif_abapgit_flow_logic=>ty_features.
    DATA ls_feature  LIKE LINE OF lt_features.

    lo_data = inject( ).
    lo_data->add_transport( ).

    lt_features = zcl_abapgit_flow_logic=>get( )-features.
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_features )
      exp = 1 ).

    READ TABLE lt_features INDEX 1 INTO ls_feature.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_not_initial( ls_feature-transport-trkorr ).
    cl_abap_unit_assert=>assert_initial( ls_feature-branch-display_name ).

  ENDMETHOD.

  METHOD branch_and_transport.
    DATA lo_data     TYPE REF TO lcl_data.
    DATA lt_features TYPE zif_abapgit_flow_logic=>ty_features.

    lo_data = inject( ).
    lo_data->add_transport( ).
    lo_data->add_branch( ).

    lt_features = zcl_abapgit_flow_logic=>get( )-features.
    " todo, cl_abap_unit_assert=>assert_equals(
    "   act = lines( lt_features )
    "   exp = 1 ).

    " todo, READ TABLE lt_features INDEX 1 INTO ls_feature.
    " todo, cl_abap_unit_assert=>assert_subrc( ).

    " todo, cl_abap_unit_assert=>assert_not_initial( ls_feature-transport-trkorr ).
    " todo, cl_abap_unit_assert=>assert_not_initial( ls_feature-branch-display_name ).
  ENDMETHOD.

ENDCLASS.
