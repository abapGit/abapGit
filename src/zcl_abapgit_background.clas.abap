CLASS zcl_abapgit_background DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      run
        RAISING zcx_abapgit_exception.

  PRIVATE SECTION.
    CLASS-METHODS:
      build_comment
        IMPORTING is_files          TYPE zif_abapgit_definitions=>ty_stage_files
        RETURNING VALUE(rv_comment) TYPE string,
      push
        IMPORTING io_repo     TYPE REF TO zcl_abapgit_repo_online
                  is_settings TYPE zcl_abapgit_persist_background=>ty_background
        RAISING   zcx_abapgit_exception,
      push_fixed
        IMPORTING io_repo     TYPE REF TO zcl_abapgit_repo_online
                  is_settings TYPE zcl_abapgit_persist_background=>ty_background
        RAISING   zcx_abapgit_exception,
      push_auto
        IMPORTING io_repo     TYPE REF TO zcl_abapgit_repo_online
                  is_settings TYPE zcl_abapgit_persist_background=>ty_background
        RAISING   zcx_abapgit_exception,
      determine_user_details
        IMPORTING iv_method      TYPE string
                  iv_changed_by  TYPE xubname
        RETURNING VALUE(rs_user) TYPE zif_abapgit_definitions=>ty_git_user.

ENDCLASS.



CLASS zcl_abapgit_background IMPLEMENTATION.


  METHOD build_comment.

    DATA: lt_objects TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          lv_str     TYPE string.

    FIELD-SYMBOLS: <ls_local> LIKE LINE OF is_files-local.


    LOOP AT is_files-local ASSIGNING <ls_local>.
      lv_str = |{ <ls_local>-item-obj_type } { <ls_local>-item-obj_name }|.
      APPEND lv_str TO lt_objects.
    ENDLOOP.

    SORT lt_objects AS TEXT.
    DELETE ADJACENT DUPLICATES FROM lt_objects.

    IF lines( lt_objects ) = 1.
      rv_comment = |BG: { lv_str }|.
    ELSE.
      rv_comment = 'BG: Multiple objects'.
      LOOP AT lt_objects INTO lv_str.
        CONCATENATE rv_comment zif_abapgit_definitions=>gc_newline lv_str INTO rv_comment.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD determine_user_details.

    DATA: lt_return             TYPE TABLE OF bapiret2,
          ls_address            TYPE bapiaddr3,
          lt_smtp               TYPE TABLE OF bapiadsmtp,
          ls_smtp               TYPE bapiadsmtp,
          lo_user_master_record TYPE REF TO zcl_abapgit_user_master_record.

*   IF the method is to use real user values, call the BAPI
    IF iv_method = zcl_abapgit_persist_background=>c_amethod-user.

      lo_user_master_record = zcl_abapgit_user_master_record=>get_instance( iv_changed_by ).

      rs_user-name = lo_user_master_record->get_name( ).
      rs_user-email = lo_user_master_record->get_email( ).

    ENDIF.

*   If no email, fall back to localhost/default email
    IF rs_user-email IS INITIAL.
      rs_user-email = |{ iv_changed_by }@localhost|.
    ENDIF.

*   If no full name maintained, just use changed by user name
    IF rs_user-name IS INITIAL.
      rs_user-name  = iv_changed_by.
    ENDIF.
  ENDMETHOD.


  METHOD push.

    IF lines( zcl_abapgit_stage_logic=>get( io_repo )-local ) = 0.
      WRITE: / 'nothing to stage' ##NO_TEXT.
      RETURN.
    ENDIF.

    CASE is_settings-amethod.
      WHEN zcl_abapgit_persist_background=>c_amethod-fixed.
        push_fixed( io_repo     = io_repo
                    is_settings = is_settings ).
      WHEN zcl_abapgit_persist_background=>c_amethod-auto
        OR zcl_abapgit_persist_background=>c_amethod-user.
        push_auto( io_repo     = io_repo
                   is_settings = is_settings ).
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( 'unknown push method' ).
    ENDCASE.

  ENDMETHOD.


  METHOD push_auto.

    TYPES: BEGIN OF ty_changed,
             filename   TYPE string,
             path       TYPE string,
             changed_by TYPE xubname,
           END OF ty_changed.

    DATA: ls_comment    TYPE zif_abapgit_definitions=>ty_comment,
          ls_files      TYPE zif_abapgit_definitions=>ty_stage_files,
          lt_changed    TYPE STANDARD TABLE OF ty_changed WITH DEFAULT KEY,
          lt_users      TYPE STANDARD TABLE OF xubname WITH DEFAULT KEY,
          ls_user_files LIKE ls_files,
          lv_changed_by TYPE xubname,
          lo_stage      TYPE REF TO zcl_abapgit_stage.

    FIELD-SYMBOLS: <ls_changed> LIKE LINE OF lt_changed,
                   <ls_remote>  LIKE LINE OF ls_files-remote,
                   <ls_local>   LIKE LINE OF ls_files-local.


    ls_files = zcl_abapgit_stage_logic=>get( io_repo ).

    LOOP AT ls_files-local ASSIGNING <ls_local>.
      lv_changed_by = zcl_abapgit_objects=>changed_by( <ls_local>-item ).
      APPEND lv_changed_by TO lt_users.
      APPEND INITIAL LINE TO lt_changed ASSIGNING <ls_changed>.
      <ls_changed>-changed_by = lv_changed_by.
      <ls_changed>-filename   = <ls_local>-file-filename.
      <ls_changed>-path       = <ls_local>-file-path.
    ENDLOOP.

    SORT lt_users ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_users.

    LOOP AT lt_users INTO lv_changed_by.
      CLEAR: ls_comment.

*     Fill user details
      ls_comment-committer = determine_user_details( iv_method     = is_settings-amethod
                                                     iv_changed_by = lv_changed_by ).

      CREATE OBJECT lo_stage
        EXPORTING
          iv_branch_name = io_repo->get_branch_name( )
          iv_branch_sha1 = io_repo->get_sha1_remote( ).

      CLEAR ls_user_files.

      LOOP AT ls_files-local ASSIGNING <ls_local>.
        READ TABLE lt_changed WITH KEY
          path = <ls_local>-file-path
          filename = <ls_local>-file-filename
          changed_by = lv_changed_by
          TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          WRITE: / 'stage' ##NO_TEXT,
            ls_comment-committer-name,
            <ls_local>-file-path,
            <ls_local>-file-filename.

          lo_stage->add( iv_path     = <ls_local>-file-path
                         iv_filename = <ls_local>-file-filename
                         iv_data     = <ls_local>-file-data ).

          APPEND <ls_local> TO ls_user_files-local.

          LOOP AT ls_files-remote ASSIGNING <ls_remote>
              WHERE filename = <ls_local>-file-filename
              AND path <> <ls_local>-file-path
              AND filename <> 'package.devc.xml'.
            WRITE: / 'rm' ##NO_TEXT,
              <ls_remote>-path,
              <ls_remote>-filename.

* rm old file when object has moved
            lo_stage->rm(
              iv_path     = <ls_remote>-path
              iv_filename = <ls_remote>-filename ).
            EXIT. " assumption: only one file
          ENDLOOP.
        ENDIF.
      ENDLOOP.

      ls_comment-comment = build_comment( ls_user_files ).

      io_repo->push( is_comment = ls_comment
                     io_stage   = lo_stage ).
    ENDLOOP.

  ENDMETHOD.


  METHOD push_fixed.

    DATA: ls_comment TYPE zif_abapgit_definitions=>ty_comment,
          ls_files   TYPE zif_abapgit_definitions=>ty_stage_files,
          lo_stage   TYPE REF TO zcl_abapgit_stage.

    FIELD-SYMBOLS: <ls_local> LIKE LINE OF ls_files-local.


    ls_files = zcl_abapgit_stage_logic=>get( io_repo ).
    ASSERT lines( ls_files-local ) > 0.

    CREATE OBJECT lo_stage
      EXPORTING
        iv_branch_name = io_repo->get_branch_name( )
        iv_branch_sha1 = io_repo->get_sha1_remote( ).

    LOOP AT ls_files-local ASSIGNING <ls_local>.
      WRITE: / 'stage' ##NO_TEXT,
        <ls_local>-file-path,
        <ls_local>-file-filename.
      lo_stage->add( iv_path     = <ls_local>-file-path
                     iv_filename = <ls_local>-file-filename
                     iv_data     = <ls_local>-file-data ).
    ENDLOOP.

    ls_comment-committer-name  = is_settings-aname.
    ls_comment-committer-email = is_settings-amail.
    ls_comment-comment         = build_comment( ls_files ).

    io_repo->push( is_comment = ls_comment
                   io_stage   = lo_stage ).

  ENDMETHOD.


  METHOD run.

    CONSTANTS: lc_enq_type TYPE c LENGTH 12 VALUE 'BACKGROUND'.

    DATA: lo_per       TYPE REF TO zcl_abapgit_persist_background,
          lo_repo      TYPE REF TO zcl_abapgit_repo_online,
          lt_list      TYPE zcl_abapgit_persist_background=>tt_background,
          ls_checks    TYPE zif_abapgit_definitions=>ty_deserialize_checks,
          lv_repo_name TYPE string.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    CALL FUNCTION 'ENQUEUE_EZABAPGIT'
      EXPORTING
        mode_zabapgit  = 'E'
        type           = lc_enq_type
        _scope         = '3'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      WRITE: / 'Another intance of the program is already running'.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_per.
    lt_list = lo_per->list( ).

    WRITE: / 'Background mode' ##NO_TEXT.

    LOOP AT lt_list ASSIGNING <ls_list>.
      lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( <ls_list>-key ).
      lv_repo_name = lo_repo->get_name( ).
      WRITE: / <ls_list>-method, lv_repo_name.

      zcl_abapgit_login_manager=>set(
        iv_uri      = lo_repo->get_url( )
        iv_username = <ls_list>-username
        iv_password = <ls_list>-password ).

      CASE <ls_list>-method.
        WHEN zcl_abapgit_persist_background=>c_method-pull.
* todo, set defaults in ls_checks
          lo_repo->deserialize( ls_checks ).
        WHEN zcl_abapgit_persist_background=>c_method-push.
          push( io_repo     = lo_repo
                is_settings = <ls_list> ).
        WHEN OTHERS.
          zcx_abapgit_exception=>raise( 'background, unknown mode' ).
      ENDCASE.
    ENDLOOP.

    IF lines( lt_list ) = 0.
      WRITE: / 'Nothing configured' ##NO_TEXT.
    ENDIF.

    CALL FUNCTION 'DEQUEUE_EZABAPGIT'
      EXPORTING
        type = lc_enq_type.

  ENDMETHOD.
ENDCLASS.
