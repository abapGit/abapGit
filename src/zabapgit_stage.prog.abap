*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_STAGE
*&---------------------------------------------------------------------*

"! keep the list of staged files, independent of repository and branch
CLASS lcl_stage DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: ty_method TYPE c LENGTH 1.

    CONSTANTS: BEGIN OF c_method,
                 add    TYPE ty_method VALUE 'A',
                 rm     TYPE ty_method VALUE 'R',
                 ignore TYPE ty_method VALUE 'I',
                 skip   TYPE ty_method VALUE '?',
               END OF c_method.

    TYPES: BEGIN OF ty_stage,
             file   TYPE ty_file,
             method TYPE ty_method,
           END OF ty_stage.

    TYPES: ty_stage_tt TYPE SORTED TABLE OF ty_stage
      WITH UNIQUE KEY file-path file-filename.

    CLASS-METHODS method_description
      IMPORTING iv_method             TYPE ty_method
      RETURNING VALUE(rv_description) TYPE string
      RAISING   lcx_exception.

    METHODS:
      constructor
        IMPORTING iv_branch_name  TYPE string
                  iv_branch_sha1  TYPE ty_sha1
                  iv_merge_source TYPE ty_sha1 OPTIONAL,
      get_branch_name
        RETURNING VALUE(rv_branch) TYPE string,
      get_branch_sha1
        RETURNING VALUE(rv_branch) TYPE ty_sha1,
      add
        IMPORTING iv_path     TYPE ty_file-path
                  iv_filename TYPE ty_file-filename
                  iv_data     TYPE xstring
        RAISING   lcx_exception,
      reset
        IMPORTING iv_path     TYPE ty_file-path
                  iv_filename TYPE ty_file-filename
        RAISING   lcx_exception,
      reset_all
        RAISING   lcx_exception,
      rm
        IMPORTING iv_path     TYPE ty_file-path
                  iv_filename TYPE ty_file-filename
        RAISING   lcx_exception,
      ignore
        IMPORTING iv_path     TYPE ty_file-path
                  iv_filename TYPE ty_file-filename
        RAISING   lcx_exception,
*      lookup
*        IMPORTING iv_path          TYPE ty_file-path
*                  iv_filename      TYPE ty_file-filename
*        RETURNING VALUE(rv_method) TYPE ty_method,
      get_merge_source
        RETURNING VALUE(rv_source) TYPE ty_sha1,
      count
        RETURNING VALUE(rv_count) TYPE i,
      get_all
        RETURNING VALUE(rt_stage) TYPE ty_stage_tt.

  PRIVATE SECTION.
    DATA: mt_stage        TYPE ty_stage_tt,
          mv_branch_name  TYPE string,
          mv_branch_sha1  TYPE ty_sha1,
          mv_merge_source TYPE ty_sha1.

    METHODS:
      append
        IMPORTING iv_path     TYPE ty_file-path
                  iv_filename TYPE ty_file-filename
                  iv_method   TYPE ty_method
                  iv_data     TYPE xstring OPTIONAL
        RAISING   lcx_exception.

ENDCLASS.   "lcl_stage DEFINITION

CLASS lcl_stage IMPLEMENTATION.

  METHOD constructor.
    mv_branch_name = iv_branch_name.
    mv_branch_sha1 = iv_branch_sha1.
    mv_merge_source = iv_merge_source.
  ENDMETHOD.

  METHOD get_branch_name.
    rv_branch = mv_branch_name.
  ENDMETHOD.

  METHOD get_merge_source.
    rv_source = mv_merge_source.
  ENDMETHOD.

  METHOD get_branch_sha1.
    rv_branch = mv_branch_sha1.
  ENDMETHOD.

*  METHOD lookup.
*
*    DATA ls_stage LIKE LINE OF mt_stage.
*
*
*    READ TABLE mt_stage INTO ls_stage
*      WITH KEY file-path     = iv_path
*               file-filename = iv_filename.
*    IF sy-subrc = 0.
*      rv_method = ls_stage-method.
*    ENDIF.
*
*  ENDMETHOD.        "lookup

  METHOD get_all.
    rt_stage = mt_stage.
  ENDMETHOD.        "get_all

  METHOD append.

    DATA: ls_stage LIKE LINE OF mt_stage.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF mt_stage.


    READ TABLE mt_stage WITH KEY
      file-path     = iv_path
      file-filename = iv_filename
      ASSIGNING <ls_stage>.
    IF sy-subrc = 0.
      <ls_stage>-file-data = iv_data.
      <ls_stage>-method    = iv_method.
    ELSE.
      ls_stage-file-path     = iv_path.
      ls_stage-file-filename = iv_filename.
      ls_stage-file-data     = iv_data.
      ls_stage-method        = iv_method.
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
        lcx_exception=>raise( 'unknown staging method type' ).
    ENDCASE.

  ENDMETHOD.        "method_description

  METHOD add.
    append( iv_path     = iv_path
            iv_filename = iv_filename
            iv_method   = c_method-add
            iv_data     = iv_data ).
  ENDMETHOD.        "add

  METHOD reset.
    DELETE mt_stage WHERE file-path     = iv_path
                    AND   file-filename = iv_filename.
    ASSERT sy-subrc = 0.
  ENDMETHOD.        "reset

  METHOD reset_all.
    CLEAR mt_stage.
  ENDMETHOD.  "reset_all

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

ENDCLASS.
