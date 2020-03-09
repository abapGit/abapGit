CLASS zcl_abapgit_persistence_repo DEFINITION
  PUBLIC
  CREATE PROTECTED
  GLOBAL FRIENDS zcl_abapgit_persist_factory .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_persist_repo .

    METHODS constructor .
  PROTECTED SECTION.

    ALIASES list
      FOR zif_abapgit_persist_repo~list .
    ALIASES read
      FOR zif_abapgit_persist_repo~read .
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
      RESULT repo = rs_repo ##NO_TEXT.

* automatic migration of old fields
    FIND FIRST OCCURRENCE OF '</HEAD_BRANCH><WRITE_PROTECT>X</WRITE_PROTECT>' IN lv_xml.
    IF sy-subrc = 0.
      rs_repo-local_settings-write_protected = abap_true.
    ENDIF.
    FIND FIRST OCCURRENCE OF '<IGNORE_SUBPACKAGES>X</IGNORE_SUBPACKAGES></REPO>' IN lv_xml.
    IF sy-subrc = 0.
      rs_repo-local_settings-ignore_subpackages = abap_true.
    ENDIF.

    IF rs_repo IS INITIAL.
      zcx_abapgit_exception=>raise( 'Inconsistent repo metadata' ).
    ENDIF.

  ENDMETHOD.


  METHOD get_next_id.

* todo: Lock the complete persistence in order to prevent concurrent repo-creation
* however the current approach will most likely work in almost all cases

    DATA: lt_content TYPE zif_abapgit_persistence=>tt_content.

    FIELD-SYMBOLS: <ls_content> LIKE LINE OF lt_content.


    rv_next_repo_id = 1.

    lt_content = mo_db->list_by_type( zcl_abapgit_persistence_db=>c_type_repo ).
    LOOP AT lt_content ASSIGNING <ls_content>.
      IF <ls_content>-value >= rv_next_repo_id.
        rv_next_repo_id = <ls_content>-value + 1.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = rv_next_repo_id
      IMPORTING
        output = rv_next_repo_id.

  ENDMETHOD.


  METHOD to_xml.

    DATA: ls_xml TYPE zif_abapgit_persistence=>ty_repo_xml.


    MOVE-CORRESPONDING is_repo TO ls_xml.

    CALL TRANSFORMATION id
      SOURCE repo = ls_xml
      RESULT XML rv_repo_xml_string.
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


  METHOD zif_abapgit_persist_repo~list.

    DATA: lt_content TYPE zif_abapgit_persistence=>tt_content,
          ls_content LIKE LINE OF lt_content,
          ls_repo    LIKE LINE OF rt_repos.


    lt_content = mo_db->list_by_type( zcl_abapgit_persistence_db=>c_type_repo ).

    LOOP AT lt_content INTO ls_content.
      MOVE-CORRESPONDING from_xml( ls_content-data_str ) TO ls_repo.
      IF ls_repo-local_settings-write_protected = abap_false AND
         zcl_abapgit_factory=>get_environment( )->is_repo_object_changes_allowed( ) = abap_false.
        ls_repo-local_settings-write_protected = abap_true.
      ENDIF.
      ls_repo-key = ls_content-value.
      INSERT ls_repo INTO TABLE rt_repos.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_persist_repo~lock.

    mo_db->lock( iv_mode  = iv_mode
                 iv_type  = zcl_abapgit_persistence_db=>c_type_repo
                 iv_value = iv_key ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_repo~read.

    DATA lt_repo TYPE zif_abapgit_persistence=>tt_repo.

    lt_repo = list( ).

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

    TRY.
        ls_persistent_meta = read( iv_key ).
      CATCH zcx_abapgit_not_found.
        zcx_abapgit_exception=>raise( 'repo key not found' ).
    ENDTRY.

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
