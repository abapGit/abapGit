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


  METHOD zif_abapgit_persist_repo~update_branch_name.

    DATA: lt_content TYPE zif_abapgit_persistence=>tt_content,
          ls_content LIKE LINE OF lt_content,
          ls_repo    TYPE zif_abapgit_persistence=>ty_repo.


    ASSERT NOT iv_key IS INITIAL.

    TRY.
        ls_repo = read( iv_key ).
      CATCH zcx_abapgit_not_found.
        zcx_abapgit_exception=>raise( 'key not found' ).
    ENDTRY.

    ls_repo-branch_name = iv_branch_name.
    ls_content-data_str = to_xml( ls_repo ).

    mo_db->update( iv_type  = zcl_abapgit_persistence_db=>c_type_repo
                   iv_value = iv_key
                   iv_data  = ls_content-data_str ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_repo~update_deserialized.

    DATA: lt_content TYPE zif_abapgit_persistence=>tt_content,
          ls_content LIKE LINE OF lt_content,
          ls_repo    TYPE zif_abapgit_persistence=>ty_repo.

    ASSERT NOT iv_key IS INITIAL.

    TRY.
        ls_repo = read( iv_key ).
      CATCH zcx_abapgit_not_found.
        zcx_abapgit_exception=>raise( 'key not found' ).
    ENDTRY.

    IF iv_deserialized_at IS NOT INITIAL.
      ls_repo-deserialized_at = iv_deserialized_at.
    ENDIF.

    IF iv_deserialized_by IS NOT INITIAL.
      ls_repo-deserialized_by = iv_deserialized_by.
    ENDIF.

    ls_content-data_str = to_xml( ls_repo ).

    mo_db->update( iv_type  = zcl_abapgit_persistence_db=>c_type_repo
                   iv_value = iv_key
                   iv_data  = ls_content-data_str ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_repo~update_dot_abapgit.

    DATA: lt_content TYPE zif_abapgit_persistence=>tt_content,
          ls_content LIKE LINE OF lt_content,
          ls_repo    TYPE zif_abapgit_persistence=>ty_repo.


    ASSERT NOT iv_key IS INITIAL.

    TRY.
        ls_repo = read( iv_key ).
      CATCH zcx_abapgit_not_found.
        zcx_abapgit_exception=>raise( 'key not found' ).
    ENDTRY.

    ls_repo-dot_abapgit = is_dot_abapgit.
    ls_content-data_str = to_xml( ls_repo ).

    mo_db->update( iv_type  = zcl_abapgit_persistence_db=>c_type_repo
                   iv_value = iv_key
                   iv_data  = ls_content-data_str ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_repo~update_head_branch.

    DATA: lt_content TYPE zif_abapgit_persistence=>tt_content,
          ls_content LIKE LINE OF lt_content,
          ls_repo    TYPE zif_abapgit_persistence=>ty_repo.


    ASSERT NOT iv_key IS INITIAL.

    TRY.
        ls_repo = read( iv_key ).
      CATCH zcx_abapgit_not_found.
        zcx_abapgit_exception=>raise( 'key not found' ).
    ENDTRY.

    ls_repo-head_branch = iv_head_branch.
    ls_content-data_str = to_xml( ls_repo ).

    mo_db->update( iv_type  = zcl_abapgit_persistence_db=>c_type_repo
                   iv_value = iv_key
                   iv_data  = ls_content-data_str ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_repo~update_local_checksums.

    DATA: lt_content TYPE zif_abapgit_persistence=>tt_content,
          ls_content LIKE LINE OF lt_content,
          ls_repo    TYPE zif_abapgit_persistence=>ty_repo.


    ASSERT NOT iv_key IS INITIAL.

    TRY.
        ls_repo = read( iv_key ).
      CATCH zcx_abapgit_not_found.
        zcx_abapgit_exception=>raise( 'key not found' ).
    ENDTRY.

    ls_repo-local_checksums = it_checksums.
    ls_content-data_str = to_xml( ls_repo ).

    mo_db->update( iv_type  = zcl_abapgit_persistence_db=>c_type_repo
                   iv_value = iv_key
                   iv_data  = ls_content-data_str ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_repo~update_local_settings.

    DATA: lt_content TYPE zif_abapgit_persistence=>tt_content,
          ls_content LIKE LINE OF lt_content,
          ls_repo    TYPE zif_abapgit_persistence=>ty_repo.


    ASSERT NOT iv_key IS INITIAL.

    TRY.
        ls_repo = read( iv_key ).
      CATCH zcx_abapgit_not_found.
        zcx_abapgit_exception=>raise( 'key not found' ).
    ENDTRY.

    ls_repo-local_settings = is_settings.
    ls_content-data_str = to_xml( ls_repo ).

    mo_db->update( iv_type  = zcl_abapgit_persistence_db=>c_type_repo
                   iv_value = iv_key
                   iv_data  = ls_content-data_str ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_repo~update_offline.

    DATA: lt_content TYPE zif_abapgit_persistence=>tt_content,
          ls_content LIKE LINE OF lt_content,
          ls_repo    TYPE zif_abapgit_persistence=>ty_repo.

    ASSERT NOT iv_key IS INITIAL.

    TRY.
        ls_repo = read( iv_key ).
      CATCH zcx_abapgit_not_found.
        zcx_abapgit_exception=>raise( 'key not found' ).
    ENDTRY.

    ls_repo-offline = iv_offline.
    ls_content-data_str = to_xml( ls_repo ).

    mo_db->update( iv_type  = zcl_abapgit_persistence_db=>c_type_repo
                   iv_value = iv_key
                   iv_data  = ls_content-data_str ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_repo~update_url.

    DATA: lt_content TYPE zif_abapgit_persistence=>tt_content,
          ls_content LIKE LINE OF lt_content,
          ls_repo    TYPE zif_abapgit_persistence=>ty_repo.


    IF iv_url IS INITIAL.
      zcx_abapgit_exception=>raise( 'update, url empty' ).
    ENDIF.

    ASSERT NOT iv_key IS INITIAL.

    TRY.
        ls_repo = read( iv_key ).
      CATCH zcx_abapgit_not_found.
        zcx_abapgit_exception=>raise( 'key not found' ).
    ENDTRY.

    ls_repo-url = iv_url.
    ls_content-data_str = to_xml( ls_repo ).

    mo_db->update( iv_type  = zcl_abapgit_persistence_db=>c_type_repo
                   iv_value = iv_key
                   iv_data  = ls_content-data_str ).

  ENDMETHOD.
ENDCLASS.
