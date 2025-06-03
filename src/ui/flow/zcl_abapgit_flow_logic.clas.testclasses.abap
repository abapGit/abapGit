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
    RETURN.
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
    METHODS setup.
    METHODS test1 FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_flow_logic IMPLEMENTATION.
  METHOD setup.
    DATA lo_repo TYPE REF TO lcl_repo.
    DATA li_repo_srv TYPE REF TO zif_abapgit_repo_srv.

    CREATE OBJECT lo_repo TYPE lcl_repo.
    CREATE OBJECT li_repo_srv TYPE lcl_repo_srv EXPORTING io_repo = lo_repo.

    zcl_abapgit_repo_srv=>inject_instance( li_repo_srv ).
  ENDMETHOD.

  METHOD test1.

    DATA lt_features TYPE zif_abapgit_flow_logic=>ty_features.

    lt_features = zcl_abapgit_flow_logic=>get( ).

    cl_abap_unit_assert=>assert_not_initial( lt_features ).

* todo

  ENDMETHOD.

ENDCLASS.
