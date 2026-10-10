CLASS zcl_abapgit_repo_oci_connector DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_repo_connector.

    METHODS constructor
      IMPORTING
        io_client TYPE REF TO zcl_abapgit_oci_client OPTIONAL.

  PRIVATE SECTION.
    DATA mo_client TYPE REF TO zcl_abapgit_oci_client.
ENDCLASS.


CLASS zcl_abapgit_repo_oci_connector IMPLEMENTATION.

  METHOD constructor.
    mo_client = io_client.
    IF mo_client IS NOT BOUND.
      CREATE OBJECT mo_client.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_repo_connector~fetch.

    DATA ls_reference TYPE zcl_abapgit_oci_reference=>ty_reference.

    IF is_repo-repo_kind <> zif_abapgit_persistence=>c_repo_kind-oci OR
       is_repo-offline = abap_true.
      zcx_abapgit_exception=>raise( 'OCI connector requires OCI repository metadata' ).
    ENDIF.

    ls_reference = zcl_abapgit_oci_reference=>parse(
      zcl_abapgit_oci_reference=>build(
        iv_registry   = is_repo-oci_registry
        iv_repository = is_repo-oci_repository
        iv_reference  = is_repo-oci_reference ) ).
    rs_snapshot = mo_client->fetch( ls_reference-canonical ).

  ENDMETHOD.

ENDCLASS.
