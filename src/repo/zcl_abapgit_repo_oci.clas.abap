CLASS zcl_abapgit_repo_oci DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_repo
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        is_data      TYPE zif_abapgit_persistence=>ty_repo
        ii_connector TYPE REF TO zif_abapgit_repo_connector OPTIONAL.

    METHODS zif_abapgit_repo~get_files_remote REDEFINITION.
    METHODS zif_abapgit_repo~get_name REDEFINITION.
    METHODS zif_abapgit_repo~has_remote_source REDEFINITION.

  PRIVATE SECTION.
    DATA mi_connector TYPE REF TO zif_abapgit_repo_connector.
    METHODS fetch_remote
      RAISING
        zcx_abapgit_exception.
ENDCLASS.


CLASS zcl_abapgit_repo_oci IMPLEMENTATION.

  METHOD constructor.
    super->constructor( is_data ).
    mi_connector = ii_connector.
    IF mi_connector IS NOT BOUND.
      CREATE OBJECT mi_connector TYPE zcl_abapgit_repo_oci_connector.
    ENDIF.
  ENDMETHOD.


  METHOD fetch_remote.
    DATA ls_snapshot TYPE zif_abapgit_repo_connector=>ty_snapshot.

    IF mv_request_remote_refresh = abap_false.
      RETURN.
    ENDIF.

    ls_snapshot = mi_connector->fetch( ms_data ).
    IF ls_snapshot-resolved_revision IS INITIAL OR ls_snapshot-files IS INITIAL.
      zcx_abapgit_exception=>raise( 'OCI connector returned an empty or unresolved repository snapshot' ).
    ENDIF.

    set( iv_oci_resolved_digest = ls_snapshot-resolved_revision ).
    set_files_remote( ls_snapshot-files ).
  ENDMETHOD.


  METHOD zif_abapgit_repo~get_files_remote.
    fetch_remote( ).
    rt_files = super->get_files_remote(
      ii_obj_filter   = ii_obj_filter
      iv_ignore_files = iv_ignore_files ).
  ENDMETHOD.


  METHOD zif_abapgit_repo~get_name.
    DATA lt_parts TYPE string_table.

    rv_name = super->get_name( ).
    IF rv_name IS INITIAL AND ms_data-oci_repository IS NOT INITIAL.
      SPLIT ms_data-oci_repository AT '/' INTO TABLE lt_parts.
      READ TABLE lt_parts INTO rv_name INDEX lines( lt_parts ).
    ENDIF.
    IF rv_name IS INITIAL.
      rv_name = 'OCI repository'.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_repo~has_remote_source.
    rv_yes = abap_true.
  ENDMETHOD.

ENDCLASS.
