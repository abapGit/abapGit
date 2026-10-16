CLASS ltcl_oci_registry DEFINITION FOR TESTING DURATION MEDIUM RISK LEVEL CRITICAL FINAL.

  PRIVATE SECTION.
    METHODS fetch_local_tags FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_oci_registry IMPLEMENTATION.

  METHOD fetch_local_tags.
    DATA: lo_client TYPE REF TO zcl_abapgit_oci_client,
          ls_v1 TYPE zif_abapgit_repo_connector=>ty_snapshot,
          ls_v2 TYPE zif_abapgit_repo_connector=>ty_snapshot,
          ls_v1_file TYPE zif_abapgit_git_definitions=>ty_file,
          ls_v2_file TYPE zif_abapgit_git_definitions=>ty_file.

    CREATE OBJECT lo_client.
    ls_v1 = lo_client->fetch( 'oci://127.0.0.1:5443/team/library:v1' ).
    ls_v2 = lo_client->fetch( 'oci://127.0.0.1:5443/team/library:v2' ).

    IF ls_v1-resolved_revision = ls_v2-resolved_revision.
      cl_abap_unit_assert=>fail( 'Fixture tags must resolve to different manifests' ).
    ENDIF.

    READ TABLE ls_v1-files INTO ls_v1_file
      WITH KEY filename = 'zcl_oci_fixture.clas.abap'.
    cl_abap_unit_assert=>assert_equals(
      act = sy-subrc
      exp = 0 ).
    READ TABLE ls_v2-files INTO ls_v2_file
      WITH KEY filename = 'zcl_oci_fixture.clas.abap'.
    cl_abap_unit_assert=>assert_equals(
      act = sy-subrc
      exp = 0 ).
    IF ls_v1_file-sha1 = ls_v2_file-sha1.
      cl_abap_unit_assert=>fail( 'Version two must update the sample class' ).
    ENDIF.

    READ TABLE ls_v1-files TRANSPORTING NO FIELDS
      WITH KEY filename = 'zcl_oci_removed.clas.abap'.
    cl_abap_unit_assert=>assert_equals(
      act = sy-subrc
      exp = 0 ).
    READ TABLE ls_v2-files TRANSPORTING NO FIELDS
      WITH KEY filename = 'zcl_oci_removed.clas.abap'.
    cl_abap_unit_assert=>assert_equals(
      act = sy-subrc
      exp = 4 ).
  ENDMETHOD.

ENDCLASS.
