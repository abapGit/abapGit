CLASS zcl_abapgit_stage DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_method TYPE c LENGTH 1 .
    TYPES:
      BEGIN OF ty_stage,
        file   TYPE zif_abapgit_definitions=>ty_file,
        method TYPE ty_method,
      END OF ty_stage .
    TYPES:
      ty_stage_tt TYPE SORTED TABLE OF ty_stage
        WITH UNIQUE KEY file-path file-filename .

    CONSTANTS:
      BEGIN OF c_method,
        add    TYPE ty_method VALUE 'A',
        rm     TYPE ty_method VALUE 'R',
        ignore TYPE ty_method VALUE 'I',
        skip   TYPE ty_method VALUE '?',
      END OF c_method .

    CLASS-METHODS method_description
      IMPORTING
        !iv_method            TYPE ty_method
      RETURNING
        VALUE(rv_description) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      IMPORTING
        !iv_branch_name  TYPE string
        !iv_branch_sha1  TYPE zif_abapgit_definitions=>ty_sha1
        !iv_merge_source TYPE zif_abapgit_definitions=>ty_sha1 OPTIONAL .
    METHODS get_branch_name
      RETURNING
        VALUE(rv_branch) TYPE string .
    METHODS get_branch_sha1
      RETURNING
        VALUE(rv_branch) TYPE zif_abapgit_definitions=>ty_sha1 .
    METHODS add
      IMPORTING
        !iv_path     TYPE zif_abapgit_definitions=>ty_file-path
        !iv_filename TYPE zif_abapgit_definitions=>ty_file-filename
        !iv_data     TYPE xstring
      RAISING
        zcx_abapgit_exception .
    METHODS reset
      IMPORTING
        !iv_path     TYPE zif_abapgit_definitions=>ty_file-path
        !iv_filename TYPE zif_abapgit_definitions=>ty_file-filename
      RAISING
        zcx_abapgit_exception .
    METHODS rm
      IMPORTING
        !iv_path     TYPE zif_abapgit_definitions=>ty_file-path
        !iv_filename TYPE zif_abapgit_definitions=>ty_file-filename
      RAISING
        zcx_abapgit_exception .
    METHODS ignore
      IMPORTING
        !iv_path     TYPE zif_abapgit_definitions=>ty_file-path
        !iv_filename TYPE zif_abapgit_definitions=>ty_file-filename
      RAISING
        zcx_abapgit_exception .
    METHODS get_merge_source
      RETURNING
        VALUE(rv_source) TYPE zif_abapgit_definitions=>ty_sha1 .
    METHODS count
      RETURNING
        VALUE(rv_count) TYPE i .
    METHODS get_all
      RETURNING
        VALUE(rt_stage) TYPE ty_stage_tt .
  PRIVATE SECTION.
    DATA: mt_stage        TYPE ty_stage_tt,
          mv_branch_name  TYPE string,
          mv_branch_sha1  TYPE zif_abapgit_definitions=>ty_sha1,
          mv_merge_source TYPE zif_abapgit_definitions=>ty_sha1.

    METHODS:
      append
        IMPORTING iv_path     TYPE zif_abapgit_definitions=>ty_file-path
                  iv_filename TYPE zif_abapgit_definitions=>ty_file-filename
                  iv_method   TYPE ty_method
                  iv_data     TYPE xstring OPTIONAL
        RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_STAGE IMPLEMENTATION.


  METHOD add.
    append( iv_path     = iv_path
            iv_filename = iv_filename
            iv_method   = c_method-add
            iv_data     = iv_data ).
  ENDMETHOD.        "add


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


  METHOD constructor.
    mv_branch_name  = iv_branch_name.
    mv_branch_sha1  = iv_branch_sha1.
    mv_merge_source = iv_merge_source.
  ENDMETHOD.


  METHOD count.
    rv_count = lines( mt_stage ).
  ENDMETHOD.        "count


  METHOD get_all.
    rt_stage = mt_stage.
  ENDMETHOD.        "get_all


  METHOD get_branch_name.
    rv_branch = mv_branch_name.
  ENDMETHOD.


  METHOD get_branch_sha1.
    rv_branch = mv_branch_sha1.
  ENDMETHOD.


  METHOD get_merge_source.
    rv_source = mv_merge_source.
  ENDMETHOD.


  METHOD ignore.
    append( iv_path     = iv_path
            iv_filename = iv_filename
            iv_method   = c_method-ignore ).
  ENDMETHOD.        "ignore


  METHOD method_description.

    CASE iv_method.
      WHEN c_method-add.
        rv_description = 'add'.
      WHEN c_method-rm.
        rv_description = 'rm'.
      WHEN c_method-ignore.
        rv_description = 'ignore' ##NO_TEXT.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( 'unknown staging method type' ).
    ENDCASE.

  ENDMETHOD.        "method_description


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
ENDCLASS.
