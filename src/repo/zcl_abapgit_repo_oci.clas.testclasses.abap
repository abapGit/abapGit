CLASS lcl_test_oci_connector DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_repo_connector.
    DATA ms_snapshot TYPE zif_abapgit_repo_connector=>ty_snapshot.
    DATA mv_fetch_count TYPE i.
    DATA mv_fail TYPE abap_bool.
    DATA ms_last_repo TYPE zif_abapgit_persistence=>ty_repo.
ENDCLASS.

CLASS lcl_test_oci_connector IMPLEMENTATION.
  METHOD zif_abapgit_repo_connector~fetch.
    mv_fetch_count = mv_fetch_count + 1.
    ms_last_repo = is_repo.
    IF mv_fail = abap_true.
      zcx_abapgit_exception=>raise( 'Simulated OCI snapshot fetch failure' ).
    ENDIF.
    rs_snapshot = ms_snapshot.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_repo_oci DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS cache_and_capability_test FOR TESTING RAISING zcx_abapgit_exception.
    METHODS moving_tag_refreshes_snapshot FOR TESTING RAISING zcx_abapgit_exception.
    METHODS failed_refresh_keeps_imported FOR TESTING RAISING zcx_abapgit_exception.
    METHODS filters_oci_snapshot_files FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS ltcl_repo_oci IMPLEMENTATION.
  METHOD cache_and_capability_test.
    DATA: ls_repo TYPE zif_abapgit_persistence=>ty_repo,
          ls_file TYPE zif_abapgit_git_definitions=>ty_file,
          lo_connector TYPE REF TO lcl_test_oci_connector,
          lo_repo TYPE REF TO zcl_abapgit_repo_oci,
          lt_files TYPE zif_abapgit_git_definitions=>ty_files_tt.

    ls_repo-key = '01'.
    ls_repo-offline = abap_false.
    ls_repo-repo_kind = zif_abapgit_persistence=>c_repo_kind-oci.
    ls_repo-oci_registry = 'registry.example.com'.
    ls_repo-oci_repository = 'team/library'.
    ls_repo-oci_reference = 'v1'.

    ls_file-path = '/'.
    ls_file-filename = '.abapgit.xml'.
    ls_file-data = '3C783E3C2F783E'.
    ls_file-sha1 = zcl_abapgit_hash=>sha1_blob( ls_file-data ).

    CREATE OBJECT lo_connector.
    APPEND ls_file TO lo_connector->ms_snapshot-files.
    lo_connector->ms_snapshot-resolved_revision =
      'sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'.

    CREATE OBJECT lo_repo
      EXPORTING
        is_data      = ls_repo
        ii_connector = lo_connector.

    cl_abap_unit_assert=>assert_equals(
      act = lo_repo->get_repo_kind( )
      exp = zif_abapgit_persistence=>c_repo_kind-oci ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_repo->is_offline( )
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_repo->supports_git( )
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_repo->supports_push( )
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_repo->get_selected_reference( )
      exp = 'v1' ).

    lt_files = lo_repo->get_files_remote( ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_connector->mv_fetch_count
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_repo->get_resolved_revision( )
      exp = lo_connector->ms_snapshot-resolved_revision ).
    cl_abap_unit_assert=>assert_initial( lo_repo->get_imported_revision( ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_connector->ms_last_repo-oci_registry
      exp = 'registry.example.com' ).

    lt_files = lo_repo->get_files_remote( ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_connector->mv_fetch_count
      exp = 1 ).

    lo_repo->set_oci_reference(
      iv_registry   = 'registry.example.com'
      iv_repository = 'team/library'
      iv_reference  = 'v2' ).
    lo_connector->ms_snapshot-resolved_revision =
      'sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'.
    lt_files = lo_repo->get_files_remote( ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_connector->mv_fetch_count
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_repo->get_resolved_revision( )
      exp = lo_connector->ms_snapshot-resolved_revision ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_repo->get_selected_reference( )
      exp = 'v2' ).
  ENDMETHOD.


  METHOD moving_tag_refreshes_snapshot.
    DATA: ls_repo TYPE zif_abapgit_persistence=>ty_repo,
          ls_file TYPE zif_abapgit_git_definitions=>ty_file,
          lo_connector TYPE REF TO lcl_test_oci_connector,
          lo_repo TYPE REF TO zcl_abapgit_repo_oci,
          lt_files TYPE zif_abapgit_git_definitions=>ty_files_tt.

    ls_repo-key = '02'.
    ls_repo-package = 'ZTEST'.
    ls_repo-offline = abap_false.
    ls_repo-repo_kind = zif_abapgit_persistence=>c_repo_kind-oci.
    ls_repo-oci_registry = 'registry.example.com'.
    ls_repo-oci_repository = 'team/library'.
    ls_repo-oci_reference = 'latest'.

    CREATE OBJECT lo_connector.
    ls_file-path = '/'.
    ls_file-filename = '.abapgit.xml'.
    ls_file-data = '3C783E3C2F783E'.
    ls_file-sha1 = zcl_abapgit_hash=>sha1_blob( ls_file-data ).
    APPEND ls_file TO lo_connector->ms_snapshot-files.
    ls_file-path = '/src/'.
    ls_file-filename = 'zcl_fixture.clas.abap'.
    ls_file-data = '7631'.
    ls_file-sha1 = zcl_abapgit_hash=>sha1_blob( ls_file-data ).
    APPEND ls_file TO lo_connector->ms_snapshot-files.
    ls_file-filename = 'zcl_removed.clas.abap'.
    ls_file-data = '6F6C64'.
    ls_file-sha1 = zcl_abapgit_hash=>sha1_blob( ls_file-data ).
    APPEND ls_file TO lo_connector->ms_snapshot-files.
    lo_connector->ms_snapshot-resolved_revision =
      'sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'.

    CREATE OBJECT lo_repo
      EXPORTING
        is_data      = ls_repo
        ii_connector = lo_connector.
    lt_files = lo_repo->get_files_remote( ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 3 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_repo->get_resolved_revision( )
      exp = lo_connector->ms_snapshot-resolved_revision ).

    CLEAR lo_connector->ms_snapshot-files.
    ls_file-path = '/'.
    ls_file-filename = '.abapgit.xml'.
    ls_file-data = '3C783E3C2F783E'.
    ls_file-sha1 = zcl_abapgit_hash=>sha1_blob( ls_file-data ).
    APPEND ls_file TO lo_connector->ms_snapshot-files.
    ls_file-path = '/src/'.
    ls_file-filename = 'zcl_fixture.clas.abap'.
    ls_file-data = '7632'.
    ls_file-sha1 = zcl_abapgit_hash=>sha1_blob( ls_file-data ).
    APPEND ls_file TO lo_connector->ms_snapshot-files.
    ls_file-filename = 'zcl_added.clas.abap'.
    ls_file-data = '6E6577'.
    ls_file-sha1 = zcl_abapgit_hash=>sha1_blob( ls_file-data ).
    APPEND ls_file TO lo_connector->ms_snapshot-files.
    lo_connector->ms_snapshot-resolved_revision =
      'sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'.

    lo_repo->refresh( iv_drop_cache = abap_false
                      iv_drop_log = abap_false ).
    lt_files = lo_repo->get_files_remote( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 3 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_connector->mv_fetch_count
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_repo->get_resolved_revision( )
      exp = lo_connector->ms_snapshot-resolved_revision ).
    cl_abap_unit_assert=>assert_initial( lo_repo->get_imported_revision( ) ).
    READ TABLE lt_files INTO ls_file WITH KEY filename = 'zcl_removed.clas.abap'.
    cl_abap_unit_assert=>assert_equals(
      act = sy-subrc
      exp = 4 ).
    READ TABLE lt_files INTO ls_file WITH KEY filename = 'zcl_added.clas.abap'.
    cl_abap_unit_assert=>assert_equals(
      act = sy-subrc
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_file-data
      exp = '6E6577' ).
  ENDMETHOD.


  METHOD failed_refresh_keeps_imported.
    DATA: ls_repo TYPE zif_abapgit_persistence=>ty_repo,
          lo_connector TYPE REF TO lcl_test_oci_connector,
          lo_repo TYPE REF TO zcl_abapgit_repo_oci,
          lv_failed TYPE abap_bool.

    ls_repo-key = '03'.
    ls_repo-repo_kind = zif_abapgit_persistence=>c_repo_kind-oci.
    ls_repo-oci_registry = 'registry.example.com'.
    ls_repo-oci_repository = 'team/library'.
    ls_repo-oci_reference = 'latest'.
    ls_repo-oci_resolved_digest =
      'sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'.
    ls_repo-oci_imported_digest =
      'sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'.

    CREATE OBJECT lo_connector.
    lo_connector->mv_fail = abap_true.
    CREATE OBJECT lo_repo
      EXPORTING
        is_data      = ls_repo
        ii_connector = lo_connector.

    TRY.
        lo_repo->get_files_remote( ).
      CATCH zcx_abapgit_exception.
        lv_failed = abap_true.
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      act = lv_failed
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_repo->get_resolved_revision( )
      exp = ls_repo-oci_resolved_digest ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_repo->get_imported_revision( )
      exp = ls_repo-oci_imported_digest ).
  ENDMETHOD.


  METHOD filters_oci_snapshot_files.
    DATA: ls_repo TYPE zif_abapgit_persistence=>ty_repo,
          ls_file TYPE zif_abapgit_git_definitions=>ty_file,
          ls_dot_data TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit,
          ls_filter TYPE zif_abapgit_definitions=>ty_tadir,
          lo_dot TYPE REF TO zcl_abapgit_dot_abapgit,
          lo_filter TYPE REF TO zcl_abapgit_object_filter_obj,
          lo_connector TYPE REF TO lcl_test_oci_connector,
          lo_repo TYPE REF TO zcl_abapgit_repo_oci,
          lt_filter TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lt_files TYPE zif_abapgit_git_definitions=>ty_files_tt.

    ls_repo-key = '04'.
    ls_repo-package = 'ZTEST'.
    ls_repo-repo_kind = zif_abapgit_persistence=>c_repo_kind-oci.
    ls_repo-oci_registry = 'registry.example.com'.
    ls_repo-oci_repository = 'team/library'.
    ls_repo-oci_reference = 'v1'.
    APPEND '/src/excluded.tmp' TO ls_repo-local_settings-exclude_remote_paths.

    ls_dot_data-name = 'OCI_FILTERS'.
    ls_dot_data-starting_folder = '/src/'.
    ls_dot_data-folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-prefix.
    APPEND '/src/ignored.tmp' TO ls_dot_data-ignore.
    CREATE OBJECT lo_dot EXPORTING is_data = ls_dot_data.

    CREATE OBJECT lo_connector.
    ls_file-path = '/'.
    ls_file-filename = '.abapgit.xml'.
    ls_file-data = lo_dot->serialize( ).
    ls_file-sha1 = zcl_abapgit_hash=>sha1_blob( ls_file-data ).
    APPEND ls_file TO lo_connector->ms_snapshot-files.
    ls_file-path = '/src/'.
    ls_file-filename = 'zcl_keep.clas.abap'.
    ls_file-data = '636C617373'.
    ls_file-sha1 = zcl_abapgit_hash=>sha1_blob( ls_file-data ).
    APPEND ls_file TO lo_connector->ms_snapshot-files.
    ls_file-filename = 'zcl_keep.clas.xml'.
    APPEND ls_file TO lo_connector->ms_snapshot-files.
    ls_file-filename = 'zcl_other.clas.abap'.
    APPEND ls_file TO lo_connector->ms_snapshot-files.
    ls_file-filename = 'ignored.tmp'.
    APPEND ls_file TO lo_connector->ms_snapshot-files.
    ls_file-filename = 'excluded.tmp'.
    APPEND ls_file TO lo_connector->ms_snapshot-files.
    lo_connector->ms_snapshot-resolved_revision =
      'sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc'.

    ls_filter-object = 'CLAS'.
    ls_filter-obj_name = 'ZCL_KEEP'.
    APPEND ls_filter TO lt_filter.
    CREATE OBJECT lo_filter EXPORTING it_filter = lt_filter.
    CREATE OBJECT lo_repo
      EXPORTING
        is_data      = ls_repo
        ii_connector = lo_connector.

    lt_files = lo_repo->get_files_remote(
      ii_obj_filter   = lo_filter
      iv_ignore_files = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 3 ).
    READ TABLE lt_files TRANSPORTING NO FIELDS
      WITH KEY filename = '.abapgit.xml'.
    cl_abap_unit_assert=>assert_equals(
      act = sy-subrc
      exp = 0 ).
    READ TABLE lt_files TRANSPORTING NO FIELDS
      WITH KEY filename = 'zcl_keep.clas.abap'.
    cl_abap_unit_assert=>assert_equals(
      act = sy-subrc
      exp = 0 ).
    READ TABLE lt_files TRANSPORTING NO FIELDS
      WITH KEY filename = 'zcl_keep.clas.xml'.
    cl_abap_unit_assert=>assert_equals(
      act = sy-subrc
      exp = 0 ).
    READ TABLE lt_files TRANSPORTING NO FIELDS
      WITH KEY filename = 'zcl_other.clas.abap'.
    cl_abap_unit_assert=>assert_equals(
      act = sy-subrc
      exp = 4 ).
    READ TABLE lt_files TRANSPORTING NO FIELDS
      WITH KEY filename = 'ignored.tmp'.
    cl_abap_unit_assert=>assert_equals(
      act = sy-subrc
      exp = 4 ).
    READ TABLE lt_files TRANSPORTING NO FIELDS
      WITH KEY filename = 'excluded.tmp'.
    cl_abap_unit_assert=>assert_equals(
      act = sy-subrc
      exp = 4 ).
  ENDMETHOD.
ENDCLASS.
