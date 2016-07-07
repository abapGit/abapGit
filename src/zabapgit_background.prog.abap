*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_BACKGROUND
*&---------------------------------------------------------------------*

CLASS lcl_background DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: run
      RAISING lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS: push
      IMPORTING io_repo TYPE REF TO lcl_repo_online
      RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_background IMPLEMENTATION.

  METHOD push.

    DATA: ls_comment TYPE ty_comment,
          ls_files   TYPE ty_stage_files,
          lo_stage   TYPE REF TO lcl_stage.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ls_files-local.


    ls_files = lcl_stage_logic=>get( io_repo ).
    IF lines( ls_files-local ) = 0.
      WRITE: / 'nothing to stage' ##NO_TEXT.
      RETURN.
    ENDIF.

    ls_comment-username = 'foobar' ##NO_TEXT.
    ls_comment-email    = 'foo@bar.com' ##NO_TEXT.
    ls_comment-comment  = 'background mode' ##NO_TEXT.

    CREATE OBJECT lo_stage
      EXPORTING
        iv_branch_name = io_repo->get_branch_name( )
        iv_branch_sha1 = io_repo->get_sha1_remote( ).

    LOOP AT ls_files-local ASSIGNING <ls_file>.
      WRITE: / 'stage', <ls_file>-file-path, <ls_file>-file-filename ##NO_TEXT.
      lo_stage->add( iv_path     = <ls_file>-file-path
                     iv_filename = <ls_file>-file-filename
                     iv_data     = <ls_file>-file-data ).
    ENDLOOP.

    io_repo->push( is_comment = ls_comment
                   io_stage   = lo_stage ).

  ENDMETHOD.

  METHOD run.

    DATA: lo_per       TYPE REF TO lcl_persistence_background,
          lo_repo      TYPE REF TO lcl_repo_online,
          lt_list      TYPE lcl_persistence_background=>tt_background,
          lv_repo_name TYPE string.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


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
          push( lo_repo ).
        WHEN OTHERS.
          _raise 'background, unknown mode'.
      ENDCASE.
    ENDLOOP.

    IF lines( lt_list ) = 0.
      WRITE: / 'Nothing configured' ##NO_TEXT.
    ENDIF.

  ENDMETHOD.

ENDCLASS.