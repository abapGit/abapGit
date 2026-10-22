CLASS lcl_test_persistence_repo DEFINITION INHERITING FROM zcl_abapgit_persistence_repo CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS serialize
      IMPORTING
        is_repo       TYPE zif_abapgit_persistence=>ty_repo
      RETURNING
        VALUE(rv_xml) TYPE string.
    METHODS read_content
      IMPORTING
        is_content     TYPE zif_abapgit_persistence=>ty_content
      RETURNING
        VALUE(rs_repo) TYPE zif_abapgit_persistence=>ty_repo
      RAISING
        zcx_abapgit_exception.
ENDCLASS.

CLASS lcl_test_persistence_repo IMPLEMENTATION.
  METHOD serialize.
    rv_xml = to_xml( is_repo ).
  ENDMETHOD.

  METHOD read_content.
    rs_repo = get_repo_from_content( is_content ).
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_persistence_repo DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS legacy_repo_kind_defaults FOR TESTING RAISING zcx_abapgit_exception.
    METHODS oci_metadata_roundtrip FOR TESTING RAISING zcx_abapgit_exception.
    METHODS unknown_kind_is_rejected FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS ltcl_persistence_repo IMPLEMENTATION.
  METHOD legacy_repo_kind_defaults.
    DATA: lo_persistence TYPE REF TO lcl_test_persistence_repo,
          ls_repo TYPE zif_abapgit_persistence=>ty_repo,
          ls_content TYPE zif_abapgit_persistence=>ty_content,
          ls_read TYPE zif_abapgit_persistence=>ty_repo.

    CREATE OBJECT lo_persistence.

    ls_repo-offline = abap_true.
    ls_repo-url = 'https://example.com/legacy.zip'.
    ls_repo-local_settings-write_protected = abap_true.
    ls_content-value = '01'.
    ls_content-data_str = lo_persistence->serialize( ls_repo ).
    ls_read = lo_persistence->read_content( ls_content ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_read-repo_kind
      exp = zif_abapgit_persistence=>c_repo_kind-offline ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_read-key
      exp = ls_content-value ).

    CLEAR: ls_repo, ls_content.
    ls_repo-offline = abap_false.
    ls_repo-url = 'https://example.com/legacy.git'.
    ls_repo-local_settings-write_protected = abap_true.
    ls_content-value = '02'.
    ls_content-data_str = lo_persistence->serialize( ls_repo ).
    ls_read = lo_persistence->read_content( ls_content ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_read-repo_kind
      exp = zif_abapgit_persistence=>c_repo_kind-git ).
  ENDMETHOD.

  METHOD oci_metadata_roundtrip.
    DATA: lo_persistence TYPE REF TO lcl_test_persistence_repo,
          ls_repo TYPE zif_abapgit_persistence=>ty_repo,
          ls_content TYPE zif_abapgit_persistence=>ty_content,
          ls_read TYPE zif_abapgit_persistence=>ty_repo.

    CREATE OBJECT lo_persistence.

    ls_repo-repo_kind = zif_abapgit_persistence=>c_repo_kind-oci.
    ls_repo-offline = abap_false.
    ls_repo-oci_registry = 'registry.example.com'.
    ls_repo-oci_repository = 'team/library'.
    ls_repo-oci_reference = 'release-1'.
    ls_repo-oci_resolved_digest =
      'sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'.
    ls_repo-oci_imported_digest =
      'sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'.
    ls_repo-local_settings-write_protected = abap_true.
    ls_content-value = '03'.
    ls_content-data_str = lo_persistence->serialize( ls_repo ).
    ls_read = lo_persistence->read_content( ls_content ).

    cl_abap_unit_assert=>assert_equals( act = ls_read-repo_kind
                                        exp = zif_abapgit_persistence=>c_repo_kind-oci ).
    cl_abap_unit_assert=>assert_equals( act = ls_read-oci_registry
                                        exp = ls_repo-oci_registry ).
    cl_abap_unit_assert=>assert_equals( act = ls_read-oci_repository
                                        exp = ls_repo-oci_repository ).
    cl_abap_unit_assert=>assert_equals( act = ls_read-oci_reference
                                        exp = ls_repo-oci_reference ).
    cl_abap_unit_assert=>assert_equals( act = ls_read-oci_resolved_digest
                                        exp = ls_repo-oci_resolved_digest ).
    cl_abap_unit_assert=>assert_equals( act = ls_read-oci_imported_digest
                                        exp = ls_repo-oci_imported_digest ).
  ENDMETHOD.

  METHOD unknown_kind_is_rejected.
    DATA: lo_persistence TYPE REF TO lcl_test_persistence_repo,
          ls_repo TYPE zif_abapgit_persistence=>ty_repo,
          ls_content TYPE zif_abapgit_persistence=>ty_content,
          ls_read TYPE zif_abapgit_persistence=>ty_repo,
          lv_raised TYPE abap_bool.

    CREATE OBJECT lo_persistence.

    ls_repo-repo_kind = 'other'.
    ls_repo-offline = abap_false.
    ls_repo-url = 'https://example.com/repository'.
    ls_repo-local_settings-write_protected = abap_true.
    ls_content-value = '04'.
    ls_content-data_str = lo_persistence->serialize( ls_repo ).

    TRY.
        ls_read = lo_persistence->read_content( ls_content ).
      CATCH zcx_abapgit_exception.
        lv_raised = abap_true.
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      act = lv_raised
      exp = abap_true ).
  ENDMETHOD.
ENDCLASS.
