*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_STAGE
*&---------------------------------------------------------------------*

CLASS lcl_stage_logic DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_stage_files,
             local  TYPE ty_files_item_tt,
             remote TYPE ty_files_tt,
           END OF ty_stage_files.

    CLASS-METHODS:
      get
        IMPORTING io_repo         TYPE REF TO lcl_repo_online
        RETURNING VALUE(rs_files) TYPE ty_stage_files
        RAISING   lcx_exception,
      count
        IMPORTING io_repo         TYPE REF TO lcl_repo_online
        RETURNING VALUE(rv_count) TYPE i
        RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS:
      remove_ignored
        IMPORTING io_repo  TYPE REF TO lcl_repo_online
        CHANGING  cs_files TYPE ty_stage_files,
      remove_identical
        CHANGING cs_files TYPE ty_stage_files.

ENDCLASS.

CLASS lcl_stage_logic IMPLEMENTATION.

  METHOD get.
    rs_files-local  = io_repo->get_files_local( ).
    rs_files-remote = io_repo->get_files_remote( ).
    remove_identical( CHANGING cs_files = rs_files ).
    remove_ignored( EXPORTING io_repo = io_repo
                    CHANGING cs_files = rs_files ).
  ENDMETHOD.

  METHOD count.

    DATA: ls_files TYPE ty_stage_files.

    ls_files = get( io_repo ).

    rv_count = lines( ls_files-remote ) + lines( ls_files-local ).

  ENDMETHOD.

  METHOD remove_ignored.

    DATA: lv_index TYPE i.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF cs_files-remote.


    LOOP AT cs_files-remote ASSIGNING <ls_remote>.
      lv_index = sy-tabix.

      IF io_repo->get_dot_abapgit( )->is_ignored(
          iv_path = <ls_remote>-path
          iv_filename = <ls_remote>-filename ) = abap_true.
        DELETE cs_files-remote INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD remove_identical.

    DATA: lv_index  TYPE i,
          ls_remote LIKE LINE OF cs_files-remote.

    FIELD-SYMBOLS: <ls_local> LIKE LINE OF cs_files-local.


    LOOP AT cs_files-local ASSIGNING <ls_local>.
      lv_index = sy-tabix.

      READ TABLE cs_files-remote INTO ls_remote
        WITH KEY path = <ls_local>-file-path
        filename = <ls_local>-file-filename.
      IF sy-subrc = 0.
        DELETE cs_files-remote INDEX sy-tabix.
        IF ls_remote-data = <ls_local>-file-data.
          DELETE cs_files-local INDEX lv_index.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.