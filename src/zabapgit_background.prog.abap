*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_BACKGROUND
*&---------------------------------------------------------------------*

CLASS lcl_background DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      run
        RAISING lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS:
      build_comment
        IMPORTING is_files          TYPE ty_stage_files
        RETURNING VALUE(rv_comment) TYPE string,
      push
        IMPORTING io_repo     TYPE REF TO lcl_repo_online
                  is_settings TYPE lcl_persistence_background=>ty_background
        RAISING   lcx_exception,
      push_fixed
        IMPORTING io_repo     TYPE REF TO lcl_repo_online
                  is_settings TYPE lcl_persistence_background=>ty_background
        RAISING   lcx_exception,
      push_auto
        IMPORTING io_repo TYPE REF TO lcl_repo_online
        RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_background IMPLEMENTATION.

  METHOD push.

    IF lines( lcl_stage_logic=>get( io_repo )-local ) = 0.
      WRITE: / 'nothing to stage' ##NO_TEXT.
      RETURN.
    ENDIF.

    CASE is_settings-amethod.
      WHEN lcl_persistence_background=>c_amethod-fixed.
        push_fixed( io_repo     = io_repo
                    is_settings = is_settings ).
      WHEN lcl_persistence_background=>c_amethod-auto.
        push_auto( io_repo ).
      WHEN OTHERS.
        lcx_exception=>raise( 'unknown push method' ).
    ENDCASE.

  ENDMETHOD.

  METHOD push_fixed.

    DATA: ls_comment TYPE ty_comment,
          ls_files   TYPE ty_stage_files,
          lo_stage   TYPE REF TO lcl_stage.

    FIELD-SYMBOLS: <ls_local> LIKE LINE OF ls_files-local.


    ls_files = lcl_stage_logic=>get( io_repo ).
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
        CONCATENATE rv_comment gc_newline lv_str INTO rv_comment.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD push_auto.

    DATA: ls_comment    TYPE ty_comment,
          ls_files      TYPE ty_stage_files,
          ls_user_files LIKE ls_files,
          lo_stage      TYPE REF TO lcl_stage.

    FIELD-SYMBOLS: <ls_local> LIKE LINE OF ls_files-local.



    ls_files = lcl_stage_logic=>get( io_repo ).

    DO.
      READ TABLE ls_files-local INDEX 1 ASSIGNING <ls_local>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      CLEAR ls_comment.
      ls_comment-committer-name  = lcl_objects=>changed_by( <ls_local>-item ).
      ls_comment-committer-email = |{ ls_comment-committer-name }@localhost|.

      CREATE OBJECT lo_stage
        EXPORTING
          iv_branch_name = io_repo->get_branch_name( )
          iv_branch_sha1 = io_repo->get_sha1_remote( ).

      CLEAR ls_user_files.

      LOOP AT ls_files-local ASSIGNING <ls_local>.
        IF lcl_objects=>changed_by( <ls_local>-item ) = ls_comment-committer-name.
          WRITE: / 'stage' ##NO_TEXT,
            ls_comment-committer-name,
            <ls_local>-file-path,
            <ls_local>-file-filename.

          lo_stage->add( iv_path     = <ls_local>-file-path
                         iv_filename = <ls_local>-file-filename
                         iv_data     = <ls_local>-file-data ).

          APPEND <ls_local> TO ls_user_files-local.
        ENDIF.
      ENDLOOP.

      ls_comment-comment  = build_comment( ls_user_files ).

      io_repo->push( is_comment = ls_comment
                     io_stage   = lo_stage ).

      ls_files = lcl_stage_logic=>get( io_repo ).

    ENDDO.

  ENDMETHOD.

  METHOD run.

    CONSTANTS: c_enq_type TYPE c LENGTH 12 VALUE 'BACKGROUND'.

    DATA: lo_per       TYPE REF TO lcl_persistence_background,
          lo_repo      TYPE REF TO lcl_repo_online,
          lt_list      TYPE lcl_persistence_background=>tt_background,
          lv_repo_name TYPE string.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    CALL FUNCTION 'ENQUEUE_EZABAPGIT'
      EXPORTING
        mode_zabapgit  = 'E'
        type           = c_enq_type
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
      lo_repo ?= lcl_app=>repo_srv( )->get( <ls_list>-key ).
      lv_repo_name = lo_repo->get_name( ).
      WRITE: / <ls_list>-method, lv_repo_name.

      lcl_login_manager=>set(
        iv_uri      = lo_repo->get_url( )
        iv_username = <ls_list>-username
        iv_password = <ls_list>-password ).

      CASE <ls_list>-method.
        WHEN lcl_persistence_background=>c_method-pull.
          lo_repo->deserialize( ).
        WHEN lcl_persistence_background=>c_method-push.
          push( io_repo     = lo_repo
                is_settings = <ls_list> ).
        WHEN OTHERS.
          lcx_exception=>raise( 'background, unknown mode' ).
      ENDCASE.
    ENDLOOP.

    IF lines( lt_list ) = 0.
      WRITE: / 'Nothing configured' ##NO_TEXT.
    ENDIF.

    CALL FUNCTION 'DEQUEUE_EZABAPGIT'
      EXPORTING
        type = c_enq_type.

  ENDMETHOD.

ENDCLASS.
