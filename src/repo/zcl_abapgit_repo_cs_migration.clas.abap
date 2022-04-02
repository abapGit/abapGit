CLASS zcl_abapgit_repo_cs_migration DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS run
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_not_found.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      ty_repo_ids TYPE SORTED TABLE OF zif_abapgit_persistence=>ty_repo-key WITH UNIQUE KEY table_line.

    CLASS-METHODS get_unconverted_repo_ids
      RETURNING
        VALUE(rt_repo_ids) TYPE ty_repo_ids.
    CLASS-METHODS clear_repo_metadata
      IMPORTING
        iv_repo_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_not_found.
    CLASS-METHODS convert_checksums
      IMPORTING
        iv_repo_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_not_found.

ENDCLASS.



CLASS ZCL_ABAPGIT_REPO_CS_MIGRATION IMPLEMENTATION.


  METHOD clear_repo_metadata.

    DATA lo_repo_persistence TYPE REF TO zcl_abapgit_persistence_repo.

    lo_repo_persistence ?= zcl_abapgit_persist_factory=>get_repo( ).
    lo_repo_persistence->rewrite_repo_meta( iv_repo_key ).

  ENDMETHOD.


  METHOD convert_checksums.

    DATA lo_cs TYPE REF TO zcl_abapgit_repo_checksums.
    DATA lv_xml TYPE zif_abapgit_persistence=>ty_content-data_str.
    DATA:
      BEGIN OF ls_repo_extract,
        local_checksums TYPE zif_abapgit_persistence=>ty_local_checksum_tt,
      END OF ls_repo_extract.

    lv_xml = zcl_abapgit_persistence_db=>get_instance( )->read(
      iv_type  = zcl_abapgit_persistence_db=>c_type_repo
      iv_value = iv_repo_key ).

    REPLACE ALL OCCURRENCES OF '<_--28C_TYPE_REPO_--29>' IN lv_xml WITH '<REPO>'.
    REPLACE ALL OCCURRENCES OF '</_--28C_TYPE_REPO_--29>' IN lv_xml WITH '</REPO>'.

    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML lv_xml
      RESULT repo = ls_repo_extract.

    IF lines( ls_repo_extract-local_checksums ) = 0.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_cs EXPORTING iv_repo_key = iv_repo_key.
    lo_cs->force_write( ls_repo_extract-local_checksums ).

  ENDMETHOD.


  METHOD get_unconverted_repo_ids.

    DATA lt_cs_ids TYPE ty_repo_ids.
    DATA lv_repo_id LIKE LINE OF rt_repo_ids.
    DATA lv_index TYPE i.

    SELECT value FROM (zcl_abapgit_persistence_db=>c_tabname)
      INTO TABLE rt_repo_ids
      WHERE type = zcl_abapgit_persistence_db=>c_type_repo.
    SELECT value FROM (zcl_abapgit_persistence_db=>c_tabname)
      INTO TABLE lt_cs_ids
      WHERE type = zcl_abapgit_persistence_db=>c_type_repo_csum.

    LOOP AT rt_repo_ids INTO lv_repo_id.
      lv_index = sy-tabix.
      READ TABLE lt_cs_ids TRANSPORTING NO FIELDS WITH KEY table_line = lv_repo_id.
      IF sy-subrc = 0. " Already converted
        DELETE rt_repo_ids INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD run.

    DATA lt_repo_ids TYPE ty_repo_ids.
    DATA lv_repo_id LIKE LINE OF lt_repo_ids.

    lt_repo_ids = get_unconverted_repo_ids( ).

    LOOP AT lt_repo_ids INTO lv_repo_id.
      convert_checksums( lv_repo_id ).
      clear_repo_metadata( lv_repo_id ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
