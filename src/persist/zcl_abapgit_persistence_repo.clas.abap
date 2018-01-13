CLASS zcl_abapgit_persistence_repo DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_local_checksum,
             item  TYPE zif_abapgit_definitions=>ty_item,
             files TYPE zif_abapgit_definitions=>ty_file_signatures_tt,
           END OF ty_local_checksum.

    TYPES: ty_local_checksum_tt TYPE STANDARD TABLE OF ty_local_checksum WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_repo_xml,
             url                TYPE string,
             branch_name        TYPE string,
             sha1               TYPE zif_abapgit_definitions=>ty_sha1,
             package            TYPE devclass,
             offline            TYPE sap_bool,
             local_checksums    TYPE ty_local_checksum_tt,
             dot_abapgit        TYPE zcl_abapgit_dot_abapgit=>ty_dot_abapgit,
             head_branch        TYPE string,   " HEAD symref of the repo, master branch
             write_protect      TYPE sap_bool, " Deny destructive ops: pull, switch branch ...
             ignore_subpackages TYPE sap_bool,
           END OF ty_repo_xml.

    TYPES: BEGIN OF ty_repo,
             key TYPE zcl_abapgit_persistence_db=>ty_value.
        INCLUDE TYPE ty_repo_xml.
    TYPES: END OF ty_repo.
    TYPES: tt_repo TYPE STANDARD TABLE OF ty_repo WITH DEFAULT KEY.
    TYPES: tt_repo_keys TYPE STANDARD TABLE OF ty_repo-key WITH DEFAULT KEY.

    METHODS constructor.

    METHODS list
      RETURNING VALUE(rt_repos) TYPE tt_repo
      RAISING   zcx_abapgit_exception.

    METHODS update_sha1
      IMPORTING iv_key         TYPE ty_repo-key
                iv_branch_sha1 TYPE ty_repo_xml-sha1
      RAISING   zcx_abapgit_exception.

    METHODS update_local_checksums
      IMPORTING iv_key       TYPE ty_repo-key
                it_checksums TYPE ty_repo_xml-local_checksums
      RAISING   zcx_abapgit_exception.

    METHODS update_url
      IMPORTING iv_key TYPE ty_repo-key
                iv_url TYPE ty_repo_xml-url
      RAISING   zcx_abapgit_exception.

    METHODS update_branch_name
      IMPORTING iv_key         TYPE ty_repo-key
                iv_branch_name TYPE ty_repo_xml-branch_name
      RAISING   zcx_abapgit_exception.

    METHODS update_head_branch
      IMPORTING iv_key         TYPE ty_repo-key
                iv_head_branch TYPE ty_repo_xml-head_branch
      RAISING   zcx_abapgit_exception.

    METHODS update_offline
      IMPORTING iv_key     TYPE ty_repo-key
                iv_offline TYPE ty_repo_xml-offline
      RAISING   zcx_abapgit_exception.

    METHODS update_dot_abapgit
      IMPORTING iv_key         TYPE ty_repo-key
                is_dot_abapgit TYPE zcl_abapgit_dot_abapgit=>ty_dot_abapgit
      RAISING   zcx_abapgit_exception.

    METHODS add
      IMPORTING iv_url         TYPE string
                iv_branch_name TYPE string
                iv_branch      TYPE zif_abapgit_definitions=>ty_sha1 OPTIONAL
                iv_package     TYPE devclass
                iv_offline     TYPE sap_bool DEFAULT abap_false
                is_dot_abapgit TYPE zcl_abapgit_dot_abapgit=>ty_dot_abapgit
      RETURNING VALUE(rv_key)  TYPE ty_repo-key
      RAISING   zcx_abapgit_exception.

    METHODS delete
      IMPORTING iv_key TYPE ty_repo-key
      RAISING   zcx_abapgit_exception.

    METHODS read
      IMPORTING iv_key         TYPE ty_repo-key
      RETURNING VALUE(rs_repo) TYPE ty_repo
      RAISING   zcx_abapgit_exception
                zcx_abapgit_not_found.

    METHODS lock
      IMPORTING iv_mode TYPE enqmode
                iv_key  TYPE ty_repo-key
      RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA: mo_db TYPE REF TO zcl_abapgit_persistence_db.

    METHODS from_xml
      IMPORTING iv_repo_xml_string TYPE string
      RETURNING VALUE(rs_repo)     TYPE ty_repo_xml
      RAISING   zcx_abapgit_exception.

    METHODS to_xml
      IMPORTING is_repo                   TYPE ty_repo
      RETURNING VALUE(rv_repo_xml_string) TYPE string.

    METHODS get_next_id
      RETURNING VALUE(rv_next_repo_id) TYPE zcl_abapgit_persistence_db=>ty_content-value
      RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_PERSISTENCE_REPO IMPLEMENTATION.


  METHOD add.

    DATA: ls_repo        TYPE ty_repo,
          lv_repo_as_xml TYPE string.


    ls_repo-url          = iv_url.
    ls_repo-branch_name  = iv_branch_name.
    ls_repo-sha1         = iv_branch.
    ls_repo-package      = iv_package.
    ls_repo-offline      = iv_offline.
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

    IF rs_repo IS INITIAL.
      zcx_abapgit_exception=>raise( 'Inconsistent repo metadata' ).
    ENDIF.

  ENDMETHOD.


  METHOD get_next_id.

* todo: Lock the complete persistence in order to prevent concurrent repo-creation
* however the current approach will most likely work in almost all cases

    DATA: lt_content TYPE zcl_abapgit_persistence_db=>tt_content.

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

    DATA: lt_content TYPE zcl_abapgit_persistence_db=>tt_content,
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

    DATA lt_repo TYPE tt_repo.

    lt_repo = list( ).

    READ TABLE lt_repo INTO rs_repo WITH KEY key = iv_key.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abapgit_not_found.
    ENDIF.

  ENDMETHOD.


  METHOD to_xml.

    DATA: ls_xml TYPE ty_repo_xml.


    MOVE-CORRESPONDING is_repo TO ls_xml.

    CALL TRANSFORMATION id
      SOURCE repo = ls_xml
      RESULT XML rv_repo_xml_string.
  ENDMETHOD.


  METHOD update_branch_name.

    DATA: lt_content TYPE zcl_abapgit_persistence_db=>tt_content,
          ls_content LIKE LINE OF lt_content,
          ls_repo    TYPE ty_repo.


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

    DATA: lt_content TYPE zcl_abapgit_persistence_db=>tt_content,
          ls_content LIKE LINE OF lt_content,
          ls_repo    TYPE ty_repo.


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

    DATA: lt_content TYPE zcl_abapgit_persistence_db=>tt_content,
          ls_content LIKE LINE OF lt_content,
          ls_repo    TYPE ty_repo.


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

    DATA: lt_content TYPE zcl_abapgit_persistence_db=>tt_content,
          ls_content LIKE LINE OF lt_content,
          ls_repo    TYPE ty_repo.


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


  METHOD update_offline.

    DATA: lt_content TYPE zcl_abapgit_persistence_db=>tt_content,
          ls_content LIKE LINE OF lt_content,
          ls_repo    TYPE ty_repo.

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


  METHOD update_sha1.

    DATA: lt_content TYPE zcl_abapgit_persistence_db=>tt_content,
          ls_content LIKE LINE OF lt_content,
          ls_repo    TYPE ty_repo.


    ASSERT NOT iv_key IS INITIAL.

    TRY.
        ls_repo = read( iv_key ).
      CATCH zcx_abapgit_not_found.
        zcx_abapgit_exception=>raise( 'key not found' ).
    ENDTRY.

    ls_repo-sha1 = iv_branch_sha1.
    ls_content-data_str = to_xml( ls_repo ).

    mo_db->update( iv_type  = zcl_abapgit_persistence_db=>c_type_repo
                   iv_value = iv_key
                   iv_data  = ls_content-data_str ).

  ENDMETHOD.


  METHOD update_url.

    DATA: lt_content TYPE zcl_abapgit_persistence_db=>tt_content,
          ls_content LIKE LINE OF lt_content,
          ls_repo    TYPE ty_repo.


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
