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

CLASS lcl_stage IMPLEMENTATION.

  METHOD constructor.
    DATA: ls_files     TYPE lcl_stage_logic=>ty_stage_files,
          lo_repo      TYPE REF TO lcl_repo_online,
          ls_work_file LIKE LINE OF mt_workarea.

    FIELD-SYMBOLS: <ls_local>  LIKE LINE OF ls_files-local,
                   <ls_remote> LIKE LINE OF ls_files-remote.

    mv_repo_key = iv_repo_key.
    lo_repo    ?= lcl_app=>repo_srv( )->get( iv_repo_key ).
    ls_files    = lcl_stage_logic=>get( lo_repo ).

    " Unify structures
    LOOP AT ls_files-local ASSIGNING <ls_local>.
      ls_work_file-type     = c_wftype-local.
      ls_work_file-file     = <ls_local>-file.
      APPEND ls_work_file TO mt_workarea.
      mv_local_cnt = mv_local_cnt + 1.
    ENDLOOP.

    LOOP AT ls_files-remote ASSIGNING <ls_remote>.
      ls_work_file-type     = c_wftype-remote.
      ls_work_file-file     = <ls_remote>.
      APPEND ls_work_file TO mt_workarea.
    ENDLOOP.

  ENDMETHOD.        "constructor

  METHOD lookup.
    DATA ls_stage LIKE LINE OF mt_stage.

    READ TABLE mt_stage INTO ls_stage
      WITH KEY file-path     = iv_path
               file-filename = iv_filename.
    IF sy-subrc = 0.
      rv_method = ls_stage-method.
    ENDIF.

  ENDMETHOD.        "lookup

  METHOD get_all.
    rt_stage = mt_stage.
  ENDMETHOD.        "get_all

  METHOD append.

    DATA: ls_stage LIKE LINE OF mt_stage,
          ls_file  TYPE ty_file.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF mt_stage.

    ls_file = find_work_file( iv_path = iv_path iv_filename = iv_filename ).

    READ TABLE mt_stage WITH KEY
      file-path     = ls_file-path
      file-filename = ls_file-filename
      ASSIGNING <ls_stage>.
    IF sy-subrc = 0.
      <ls_stage>-file-data = ls_file-data.
      <ls_stage>-method    = iv_method.
    ELSE.
      ls_stage-file   = ls_file.
      ls_stage-method = iv_method.
      INSERT ls_stage INTO TABLE mt_stage.
    ENDIF.

  ENDMETHOD.        "append

  METHOD method_description.

    CASE iv_method.
      WHEN c_method-add.
        rv_description = 'add'.
      WHEN c_method-rm.
        rv_description = 'rm'.
      WHEN c_method-ignore.
        rv_description = 'ignore' ##NO_TEXT.
      WHEN OTHERS.
        _raise 'unknown staging method type'.
    ENDCASE.

  ENDMETHOD.        "method_description

  METHOD add.
    append( iv_path     = iv_path
            iv_filename = iv_filename
            iv_method   = c_method-add ).
  ENDMETHOD.        "add

  METHOD reset.
    DELETE mt_stage WHERE file-path     = iv_path
                    AND   file-filename = iv_filename.
    ASSERT sy-subrc = 0.
  ENDMETHOD.        "reset

  METHOD rm.
    append( iv_path     = iv_path
            iv_filename = iv_filename
            iv_method   = c_method-rm ).
  ENDMETHOD.        "rm

  METHOD ignore.
    append( iv_path     = iv_path
            iv_filename = iv_filename
            iv_method   = c_method-ignore ).
  ENDMETHOD.        "ignore

  METHOD count.
    rv_count = lines( mt_stage ).
  ENDMETHOD.        "count

  METHOD find_work_file.
    DATA ls_work_file LIKE LINE OF mt_workarea.

    READ TABLE mt_workarea INTO ls_work_file
      WITH KEY file-path     = iv_path
               file-filename = iv_filename.
    IF sy-subrc = 0.
      rs_file = ls_work_file-file.
    ELSE.
      _raise 'File not found in workarea'.
    ENDIF.

  ENDMETHOD.        "check_work_file_exists

  METHOD update_and_add_dot_abapgit.

    FIELD-SYMBOLS <ls_dot_abapgit> LIKE LINE OF mt_workarea.

    READ TABLE mt_workarea ASSIGNING <ls_dot_abapgit>
      WITH KEY file-path     = gc_root_dir
               file-filename = gc_dot_abapgit.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO mt_workarea ASSIGNING <ls_dot_abapgit>.
      <ls_dot_abapgit>-type          = c_wftype-local.
      <ls_dot_abapgit>-file-path     = gc_root_dir.
      <ls_dot_abapgit>-file-filename = gc_dot_abapgit.
    ENDIF.

    <ls_dot_abapgit>-file-data = iv_data.

    add( iv_path     = gc_root_dir
         iv_filename = gc_dot_abapgit ).

  ENDMETHOD.        "update_and_add_dot_abapgit

ENDCLASS.