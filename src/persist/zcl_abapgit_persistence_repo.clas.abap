CLASS zcl_abapgit_persistence_repo DEFINITION
  PUBLIC
  CREATE PROTECTED
  GLOBAL FRIENDS zcl_abapgit_persist_factory .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_persist_repo .
    INTERFACES zif_abapgit_persist_repo_cs .

    METHODS constructor .
    METHODS rewrite_repo_meta
      IMPORTING
        !iv_repo_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_not_found.
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mt_meta_fields TYPE STANDARD TABLE OF abap_compname.
    DATA mo_db TYPE REF TO zcl_abapgit_persistence_db .

    METHODS from_xml
      IMPORTING
        !iv_repo_xml_string TYPE string
      RETURNING
        VALUE(rs_repo)      TYPE zif_abapgit_persistence=>ty_repo_xml
      RAISING
        zcx_abapgit_exception .
    METHODS to_xml
      IMPORTING
        !is_repo                  TYPE zif_abapgit_persistence=>ty_repo
      RETURNING
        VALUE(rv_repo_xml_string) TYPE string .
    METHODS get_next_id
      RETURNING
        VALUE(rv_next_repo_id) TYPE zif_abapgit_persistence=>ty_content-value
      RAISING
        zcx_abapgit_exception .
    METHODS get_repo_from_content
      IMPORTING
        is_content    TYPE zif_abapgit_persistence=>ty_content
      RETURNING
        VALUE(rs_result) TYPE zif_abapgit_persistence=>ty_repo
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS ZCL_ABAPGIT_PERSISTENCE_REPO IMPLEMENTATION.


  METHOD constructor.

    DATA ls_dummy_meta_mask TYPE zif_abapgit_persistence=>ty_repo_meta_mask.
    DATA ls_dummy_meta      TYPE zif_abapgit_persistence=>ty_repo_xml.
    DATA lo_type_meta_mask  TYPE REF TO cl_abap_structdescr.
    DATA lo_type_meta       TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS <ls_comp> LIKE LINE OF lo_type_meta_mask->components.

    " Collect actual list of fields in repo meta data (used in update_meta)
    lo_type_meta_mask ?= cl_abap_structdescr=>describe_by_data( ls_dummy_meta_mask ).
    lo_type_meta      ?= cl_abap_structdescr=>describe_by_data( ls_dummy_meta ).
    LOOP AT lo_type_meta_mask->components ASSIGNING <ls_comp>.
      APPEND <ls_comp>-name TO mt_meta_fields.
    ENDLOOP.

    mo_db = zcl_abapgit_persistence_db=>get_instance( ).

  ENDMETHOD.


  METHOD from_xml.

    DATA: lv_xml TYPE string.

    lv_xml = iv_repo_xml_string.

* fix downward compatibility
    REPLACE ALL OCCURRENCES OF '<_--28C_TYPE_REPO_--29>' IN lv_xml WITH '<REPO>'.
    REPLACE ALL OCCURRENCES OF '</_--28C_TYPE_REPO_--29>' IN lv_xml WITH '</REPO>'.

    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML lv_xml
      RESULT repo = rs_repo.

* automatic migration of old fields
* todo, keep for transition period until 2022-12-31, then remove all of these
    FIND FIRST OCCURRENCE OF '</HEAD_BRANCH><WRITE_PROTECT>X</WRITE_PROTECT>' IN lv_xml.
    IF sy-subrc = 0.
      rs_repo-local_settings-write_protected = abap_true.
    ENDIF.
    FIND FIRST OCCURRENCE OF '<IGNORE_SUBPACKAGES>X</IGNORE_SUBPACKAGES></REPO>' IN lv_xml.
    IF sy-subrc = 0.
      rs_repo-local_settings-ignore_subpackages = abap_true.
    ENDIF.
    FIND FIRST OCCURRENCE OF '<SERIALIZE_MASTER_LANG_ONLY>X</SERIALIZE_MASTER_LANG_ONLY>' IN lv_xml.
    IF sy-subrc = 0.
      rs_repo-local_settings-main_language_only = abap_true.
    ENDIF.

    IF rs_repo IS INITIAL.
      zcx_abapgit_exception=>raise( 'Inconsistent repo metadata' ).
    ENDIF.

  ENDMETHOD.


  METHOD get_next_id.

* todo: Lock the complete persistence in order to prevent concurrent repo-creation
* however the current approach will most likely work in almost all cases

    DATA: lt_content TYPE zif_abapgit_persistence=>ty_contents.

    FIELD-SYMBOLS: <ls_content> LIKE LINE OF lt_content.


    rv_next_repo_id = 1.

    lt_content = mo_db->list_by_type( zcl_abapgit_persistence_db=>c_type_repo ).
    LOOP AT lt_content ASSIGNING <ls_content>.
      IF <ls_content>-value >= rv_next_repo_id.
        rv_next_repo_id = <ls_content>-value + 1.
      ENDIF.
    ENDLOOP.

    SHIFT rv_next_repo_id RIGHT DELETING TRAILING space.
    TRANSLATE rv_next_repo_id USING ' 0'.

  ENDMETHOD.


  METHOD get_repo_from_content.
    MOVE-CORRESPONDING from_xml( is_content-data_str ) TO rs_result.
    IF rs_result-local_settings-write_protected = abap_false AND
       zcl_abapgit_factory=>get_environment( )->is_repo_object_changes_allowed( ) = abap_false.
      rs_result-local_settings-write_protected = abap_true.
    ENDIF.
    rs_result-key = is_content-value.
  ENDMETHOD.


  METHOD rewrite_repo_meta.

    DATA lv_old_blob TYPE string.
    DATA lv_new_blob TYPE string.
    DATA ls_repo_meta TYPE zif_abapgit_persistence=>ty_repo.

    lv_old_blob = mo_db->read(
      iv_type  = zcl_abapgit_persistence_db=>c_type_repo
      iv_value = iv_repo_key ).

    MOVE-CORRESPONDING from_xml( lv_old_blob ) TO ls_repo_meta.
    lv_new_blob = to_xml( ls_repo_meta ).

    mo_db->update(
      iv_type  = zcl_abapgit_persistence_db=>c_type_repo
      iv_value = iv_repo_key
      iv_data  = lv_new_blob ).

  ENDMETHOD.


  METHOD to_xml.

    DATA: ls_xml TYPE zif_abapgit_persistence=>ty_repo_xml.


    MOVE-CORRESPONDING is_repo TO ls_xml.

    CALL TRANSFORMATION id
      SOURCE repo = ls_xml
      RESULT XML rv_repo_xml_string.
  ENDMETHOD.


  METHOD zif_abapgit_persist_repo_cs~delete.

    mo_db->delete(
      iv_type  = zcl_abapgit_persistence_db=>c_type_repo_csum
      iv_value = iv_key ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_repo_cs~read.

    rv_cs_blob = mo_db->read(
      iv_type  = zcl_abapgit_persistence_db=>c_type_repo_csum
      iv_value = iv_key ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_repo_cs~update.

    mo_db->modify(
      iv_type  = zcl_abapgit_persistence_db=>c_type_repo_csum
      iv_value = iv_key
      iv_data  = iv_cs_blob ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_repo~add.

    DATA: ls_repo        TYPE zif_abapgit_persistence=>ty_repo,
          lv_repo_as_xml TYPE string.


    ls_repo-url          = iv_url.
    ls_repo-branch_name  = iv_branch_name.
    ls_repo-package      = iv_package.
    ls_repo-offline      = iv_offline.
    ls_repo-created_by   = sy-uname.
    GET TIME STAMP FIELD ls_repo-created_at.
    ls_repo-dot_abapgit  = is_dot_abapgit.

    ls_repo-local_settings-display_name = iv_display_name.

    lv_repo_as_xml = to_xml( ls_repo ).

    rv_key = get_next_id( ).

    mo_db->add( iv_type  = zcl_abapgit_persistence_db=>c_type_repo
                iv_value = rv_key
                iv_data  = lv_repo_as_xml ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_repo~delete.

    DATA: lo_background TYPE REF TO zcl_abapgit_persist_background.

    CREATE OBJECT lo_background.
    lo_background->delete( iv_key ).

    mo_db->delete( iv_type  = zcl_abapgit_persistence_db=>c_type_repo
                   iv_value = iv_key ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_repo~exists.

    DATA lt_keys TYPE zif_abapgit_persistence=>ty_repo_keys.
    DATA lt_content TYPE zif_abapgit_persistence=>ty_contents.

    APPEND iv_key TO lt_keys.

    lt_content = mo_db->list_by_keys(
      it_keys = lt_keys
      iv_type = zcl_abapgit_persistence_db=>c_type_repo ).

    rv_yes = boolc( lines( lt_content ) > 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_repo~list.

    DATA: lt_content TYPE zif_abapgit_persistence=>ty_contents,
          ls_content LIKE LINE OF lt_content,
          ls_repo    LIKE LINE OF rt_repos.

    lt_content = mo_db->list_by_type( zcl_abapgit_persistence_db=>c_type_repo ).

    LOOP AT lt_content INTO ls_content.
      ls_repo = get_repo_from_content( ls_content ).
      INSERT ls_repo INTO TABLE rt_repos.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_persist_repo~list_by_keys.
    DATA: lt_content TYPE zif_abapgit_persistence=>ty_contents,
          ls_content LIKE LINE OF lt_content,
          ls_repo    LIKE LINE OF rt_repos.

    lt_content = mo_db->list_by_keys(
      it_keys = it_keys
      iv_type = zcl_abapgit_persistence_db=>c_type_repo ).

    LOOP AT lt_content INTO ls_content.
      ls_repo = get_repo_from_content( ls_content ).
      INSERT ls_repo INTO TABLE rt_repos.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_abapgit_persist_repo~lock.

    mo_db->lock( iv_mode  = iv_mode
                 iv_type  = zcl_abapgit_persistence_db=>c_type_repo
                 iv_value = iv_key ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_repo~read.

    DATA lt_repo TYPE zif_abapgit_persistence=>ty_repos.

    lt_repo = zif_abapgit_persist_repo~list( ).

    READ TABLE lt_repo INTO rs_repo WITH KEY key = iv_key.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abapgit_not_found.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_persist_repo~update_metadata.

    DATA:
      lv_blob            TYPE zif_abapgit_persistence=>ty_content-data_str,
      ls_persistent_meta TYPE zif_abapgit_persistence=>ty_repo.

    FIELD-SYMBOLS <lv_field>   LIKE LINE OF mt_meta_fields.
    FIELD-SYMBOLS <lg_dst>     TYPE any.
    FIELD-SYMBOLS <lg_src>     TYPE any.
    FIELD-SYMBOLS <lv_changed> TYPE abap_bool.

    ASSERT NOT iv_key IS INITIAL.

    IF is_change_mask IS INITIAL.
      RETURN.
    ENDIF.

    " Validations
    IF is_change_mask-url = abap_true AND is_meta-url IS INITIAL.
      zcx_abapgit_exception=>raise( 'update, url empty' ).
    ENDIF.

    ls_persistent_meta = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key )->ms_data.

    " Update
    LOOP AT mt_meta_fields ASSIGNING <lv_field>.
      ASSIGN COMPONENT <lv_field> OF STRUCTURE is_change_mask TO <lv_changed>.
      ASSERT sy-subrc = 0.
      CHECK <lv_changed> = abap_true.
      ASSIGN COMPONENT <lv_field> OF STRUCTURE ls_persistent_meta TO <lg_dst>.
      ASSERT sy-subrc = 0.
      ASSIGN COMPONENT <lv_field> OF STRUCTURE is_meta TO <lg_src>.
      ASSERT sy-subrc = 0.
      <lg_dst> = <lg_src>.
    ENDLOOP.

    lv_blob = to_xml( ls_persistent_meta ).

    mo_db->update( iv_type  = zcl_abapgit_persistence_db=>c_type_repo
                   iv_value = iv_key
                   iv_data  = lv_blob ).

  ENDMETHOD.
ENDCLASS.
