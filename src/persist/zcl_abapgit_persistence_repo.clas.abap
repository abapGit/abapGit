CLASS zcl_abapgit_persistence_repo DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
    METHODS list
      RETURNING
        VALUE(rt_repos) TYPE zif_abapgit_persistence=>tt_repo
      RAISING
        zcx_abapgit_exception .
    METHODS update_local_checksums
      IMPORTING
        !iv_key       TYPE zif_abapgit_persistence=>ty_repo-key
        !it_checksums TYPE zif_abapgit_persistence=>ty_repo_xml-local_checksums
      RAISING
        zcx_abapgit_exception .
    METHODS update_url
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
        !iv_url TYPE zif_abapgit_persistence=>ty_repo_xml-url
      RAISING
        zcx_abapgit_exception .
    METHODS update_branch_name
      IMPORTING
        !iv_key         TYPE zif_abapgit_persistence=>ty_repo-key
        !iv_branch_name TYPE zif_abapgit_persistence=>ty_repo_xml-branch_name
      RAISING
        zcx_abapgit_exception .
    METHODS update_head_branch
      IMPORTING
        !iv_key         TYPE zif_abapgit_persistence=>ty_repo-key
        !iv_head_branch TYPE zif_abapgit_persistence=>ty_repo_xml-head_branch
      RAISING
        zcx_abapgit_exception .
    METHODS update_offline
      IMPORTING
        !iv_key     TYPE zif_abapgit_persistence=>ty_repo-key
        !iv_offline TYPE zif_abapgit_persistence=>ty_repo_xml-offline
      RAISING
        zcx_abapgit_exception .
    METHODS update_dot_abapgit
      IMPORTING
        !iv_key         TYPE zif_abapgit_persistence=>ty_repo-key
        !is_dot_abapgit TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit
      RAISING
        zcx_abapgit_exception .
    METHODS add
      IMPORTING
        !iv_url         TYPE string
        !iv_branch_name TYPE string
        !iv_branch      TYPE zif_abapgit_definitions=>ty_sha1 OPTIONAL
        !iv_package     TYPE devclass
        !iv_offline     TYPE sap_bool DEFAULT abap_false
        !is_dot_abapgit TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit
      RETURNING
        VALUE(rv_key)   TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception .
    METHODS delete
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception .
    METHODS read
      IMPORTING
        !iv_key        TYPE zif_abapgit_persistence=>ty_repo-key
      RETURNING
        VALUE(rs_repo) TYPE zif_abapgit_persistence=>ty_repo
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_not_found .
    METHODS lock
      IMPORTING
        !iv_mode TYPE enqmode
        !iv_key  TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception .
    METHODS update_local_settings
      IMPORTING
        iv_key      TYPE zif_abapgit_persistence=>ty_repo-key
        is_settings TYPE zif_abapgit_persistence=>ty_repo_xml-local_settings
      RAISING
        zcx_abapgit_exception .
    METHODS update_deserialized
      IMPORTING
        iv_key             TYPE zif_abapgit_persistence=>ty_value
        iv_deserialized_at TYPE timestampl
        iv_deserialized_by TYPE xubname
      RAISING
        zcx_abapgit_exception.

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



CLASS zcl_abapgit_persistence_repo IMPLEMENTATION.


  METHOD add.

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


  METHOD constructor.
    mo_db = zcl_abapgit_persistence_db=>get_instance( ).
  ENDMETHOD.


  METHOD delete.

    DATA: lo_background TYPE REF TO zcl_abapgit_persist_background.

    CREATE OBJECT lo_background.
    lo_background->delete( iv_key ).

    mo_db->delete( iv_type  = zcl_abapgit_persistence_db=>c_type_repo
                   iv_value = iv_key ).

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


  METHOD list.

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


  METHOD lock.

    mo_db->lock( iv_mode  = iv_mode
                 iv_type  = zcl_abapgit_persistence_db=>c_type_repo
                 iv_value = iv_key ).

  ENDMETHOD.


  METHOD read.

    DATA lt_repo TYPE zif_abapgit_persistence=>tt_repo.

    lt_repo = list( ).

    READ TABLE lt_repo INTO rs_repo WITH KEY key = iv_key.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abapgit_not_found.
    ENDIF.

  ENDMETHOD.


  METHOD to_xml.

    DATA: ls_xml TYPE zif_abapgit_persistence=>ty_repo_xml.


    MOVE-CORRESPONDING is_repo TO ls_xml.

    CALL TRANSFORMATION id
      SOURCE repo = ls_xml
      RESULT XML rv_repo_xml_string.
  ENDMETHOD.


  METHOD update_branch_name.

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


  METHOD update_dot_abapgit.

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


  METHOD update_head_branch.

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

  ENDMETHOD.  "update_head_branch


  METHOD update_local_checksums.

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


  METHOD update_local_settings.

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


  METHOD update_offline.

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

  ENDMETHOD.  "update_offline


  METHOD update_url.

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

  METHOD update_deserialized.

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

ENDCLASS.
