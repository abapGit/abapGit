*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_GIT_HELPERS
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_git_utils
*----------------------------------------------------------------------*
CLASS lcl_git_utils DEFINITION FINAL. " > Maybe better move to lcl_git_pack ??
  PUBLIC SECTION.

    CLASS-METHODS get_null
      RETURNING VALUE(rv_c) TYPE char1.

    CLASS-METHODS pkt_string
      IMPORTING iv_string     TYPE string
      RETURNING VALUE(rv_pkt) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS length_utf8_hex
      IMPORTING iv_data       TYPE xstring
      RETURNING VALUE(rv_len) TYPE i
      RAISING   lcx_exception.

ENDCLASS. "lcl_git_utils

CLASS lcl_git_utils IMPLEMENTATION.

  METHOD get_null.

    DATA: lv_x(4) TYPE x VALUE '00000000',
          lv_z(2) TYPE c.

    FIELD-SYMBOLS <lv_y> TYPE c.


    ASSIGN lv_x TO <lv_y> CASTING.
    lv_z = <lv_y>.
    rv_c = lv_z(1).

  ENDMETHOD.                    "get_null

  METHOD length_utf8_hex.

    DATA: lv_xstring TYPE xstring,
          lv_string  TYPE string,
          lv_char4   TYPE c LENGTH 4,
          lv_x       TYPE x LENGTH 2,
          lo_obj     TYPE REF TO cl_abap_conv_in_ce,
          lv_len     TYPE int4.

* hmm, can this be done easier?

    lv_xstring = iv_data(4).

    lo_obj = cl_abap_conv_in_ce=>create(
        input    = lv_xstring
        encoding = 'UTF-8' ).
    lv_len = xstrlen( lv_xstring ).

    TRY.
        lo_obj->read( EXPORTING n    = lv_len
                      IMPORTING data = lv_string ).
      CATCH cx_sy_conversion_codepage.
        lcx_exception=>raise( 'error converting to hex, LENGTH_UTF8_HEX' ).
    ENDTRY.

    lv_char4 = lv_string.
    TRANSLATE lv_char4 TO UPPER CASE.
    lv_x = lv_char4.
    rv_len = lv_x.

  ENDMETHOD.                    "length_utf8_hex

  METHOD pkt_string.

    DATA: lv_x   TYPE x,
          lv_len TYPE i.


    lv_len = strlen( iv_string ).

    IF lv_len >= 255.
      lcx_exception=>raise( 'PKT, todo' ).
    ENDIF.

    lv_x = lv_len + 4.

    rv_pkt = rv_pkt && '00' && lv_x && iv_string.

  ENDMETHOD.                    "pkt_string

ENDCLASS. "lcl_git_utils

*----------------------------------------------------------------------*
*       CLASS lcl_git_branch_list DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_git_branch_list DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES: ty_git_branch_type TYPE char2.
    TYPES: BEGIN OF ty_git_branch,
             sha1         TYPE ty_sha1,
             name         TYPE string,
             type         TYPE ty_git_branch_type,
             is_head      TYPE abap_bool,
             display_name TYPE string,
           END OF ty_git_branch.
    TYPES: ty_git_branch_list_tt TYPE STANDARD TABLE OF ty_git_branch WITH DEFAULT KEY.

    CONSTANTS: BEGIN OF c_type,
                 branch TYPE ty_git_branch_type VALUE 'HD',
                 tag    TYPE ty_git_branch_type VALUE 'TG',
                 other  TYPE ty_git_branch_type VALUE 'ZZ',
               END OF c_type.
    CONSTANTS c_head_name   TYPE string VALUE 'HEAD'.

    METHODS constructor
      IMPORTING iv_data TYPE string
      RAISING   lcx_exception.

    METHODS find_by_name
      IMPORTING iv_branch_name   TYPE clike
      RETURNING VALUE(rs_branch) TYPE ty_git_branch
      RAISING   lcx_exception.

    METHODS get_head " For potential future use
      RETURNING VALUE(rs_branch) TYPE ty_git_branch
      RAISING   lcx_exception.

    METHODS get_head_symref
      RETURNING VALUE(rv_head_symref) TYPE string.

    METHODS get_branches_only
      RETURNING VALUE(rt_branches) TYPE ty_git_branch_list_tt
      RAISING   lcx_exception.

    METHODS get_tags_only " For potential future use
      RETURNING VALUE(rt_branches) TYPE ty_git_branch_list_tt
      RAISING   lcx_exception.

    CLASS-METHODS is_ignored
      IMPORTING iv_branch_name   TYPE clike
      RETURNING VALUE(rv_ignore) TYPE abap_bool.

    CLASS-METHODS get_display_name
      IMPORTING iv_branch_name         TYPE clike
      RETURNING VALUE(rv_display_name) TYPE string.

    CLASS-METHODS get_type
      IMPORTING iv_branch_name TYPE clike
      RETURNING VALUE(rv_type) TYPE ty_git_branch_type.

    CLASS-METHODS complete_heads_branch_name
      IMPORTING iv_branch_name TYPE clike
      RETURNING VALUE(rv_name) TYPE string.

    CLASS-METHODS normalize_branch_name
      IMPORTING iv_branch_name TYPE clike
      RETURNING VALUE(rv_name) TYPE string.

  PRIVATE SECTION.
    DATA mt_branches    TYPE ty_git_branch_list_tt.
    DATA mv_head_symref TYPE string.

    CLASS-METHODS parse_branch_list
      IMPORTING iv_data        TYPE string
      EXPORTING et_list        TYPE ty_git_branch_list_tt
                ev_head_symref TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS parse_head_params
      IMPORTING iv_data        TYPE string
      EXPORTING ev_head_symref TYPE string.

ENDCLASS. "lcl_git_branch_list

*----------------------------------------------------------------------*
*       CLASS lcl_git_branch_list IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_git_branch_list IMPLEMENTATION.

  METHOD constructor.
    parse_branch_list(
      EXPORTING iv_data        = iv_data
      IMPORTING et_list        = me->mt_branches
                ev_head_symref = me->mv_head_symref ).
  ENDMETHOD.  "create

  METHOD get_head_symref.
    rv_head_symref = mv_head_symref.
  ENDMETHOD.  " get_head_symref.

  METHOD find_by_name.

    IF iv_branch_name IS INITIAL.
      lcx_exception=>raise( 'Branch name empty' ).
    ENDIF.

    READ TABLE mt_branches INTO rs_branch
      WITH KEY name = iv_branch_name.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'Branch not found' ).
    ENDIF.

  ENDMETHOD.  "find_by_name

  METHOD get_head.

    IF mv_head_symref IS NOT INITIAL.
      rs_branch = find_by_name( mv_head_symref ).
    ELSE.
      rs_branch = find_by_name( c_head_name ).
    ENDIF.

  ENDMETHOD.  "get_head

  METHOD parse_branch_list.

    DATA: lt_result      TYPE TABLE OF string,
          lv_hash        TYPE ty_sha1,
          lv_name        TYPE string,
          lv_head_params TYPE string,
          lv_char        TYPE c,
          lv_data        LIKE LINE OF lt_result.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF et_list.

    CLEAR: et_list, ev_head_symref.

    SPLIT iv_data AT gc_newline INTO TABLE lt_result.

    LOOP AT lt_result INTO lv_data.
      IF sy-tabix = 1.
        CONTINUE. " current loop
      ELSEIF sy-tabix = 2 AND strlen( lv_data ) > 49.
        lv_hash = lv_data+8.
        lv_name = lv_data+49.
        lv_char = lcl_git_utils=>get_null( ).

        SPLIT lv_name AT lv_char INTO lv_name lv_head_params.
        parse_head_params(
          EXPORTING iv_data        = lv_head_params
          IMPORTING ev_head_symref = ev_head_symref ).
      ELSEIF sy-tabix > 2 AND strlen( lv_data ) > 45.
        lv_hash = lv_data+4.
        lv_name = lv_data+45.
      ELSEIF sy-tabix = 2 AND strlen( lv_data ) = 8 AND lv_data(8) = '00000000'.
        lcx_exception=>raise( 'No branches, create branch manually by adding file' ).
      ELSE.
        CONTINUE.
      ENDIF.

      CHECK is_ignored( lv_name ) = abap_false.
      ASSERT lv_name IS NOT INITIAL.

      APPEND INITIAL LINE TO et_list ASSIGNING <ls_branch>.
      <ls_branch>-sha1         = lv_hash.
      <ls_branch>-name         = lv_name.
      <ls_branch>-display_name = get_display_name( lv_name ).
      <ls_branch>-type         = get_type( lv_name ).
      IF <ls_branch>-name = c_head_name OR <ls_branch>-name = ev_head_symref.
        <ls_branch>-is_head    = abap_true.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "parse_branch_list

  METHOD parse_head_params.

    DATA: ls_match    TYPE match_result,
          ls_submatch TYPE submatch_result.

    FIND FIRST OCCURRENCE OF REGEX '\ssymref=HEAD:([^\s]+)' IN iv_data RESULTS ls_match.
    READ TABLE ls_match-submatches INTO ls_submatch INDEX 1.
    IF sy-subrc IS INITIAL.
      ev_head_symref = iv_data+ls_submatch-offset(ls_submatch-length).
    ENDIF.

  ENDMETHOD.  "parse_head_params

  METHOD is_ignored.

    IF iv_branch_name = 'refs/heads/gh-pages'. " Github pages
      rv_ignore = abap_true.
    ENDIF.

    IF iv_branch_name CP 'refs/pull/*'
        OR iv_branch_name CP 'refs/merge-requests/*'
        OR iv_branch_name CP 'refs/keep-around/*'
        OR iv_branch_name CP 'refs/tmp/*'.
      rv_ignore = abap_true.
    ENDIF.

  ENDMETHOD.  "is_ignored

  METHOD get_display_name.
    rv_display_name = iv_branch_name.

    IF rv_display_name CP 'refs/heads/*'.
      REPLACE FIRST OCCURRENCE OF 'refs/heads/' IN rv_display_name WITH ''.
    ELSEIF rv_display_name CP 'refs/tags/*'.
      REPLACE FIRST OCCURRENCE OF 'refs/' IN rv_display_name WITH ''.
    ENDIF.

  ENDMETHOD.  "get_display_name

  METHOD get_type.
    rv_type = c_type-other.

    IF iv_branch_name CP 'refs/heads/*' OR iv_branch_name = c_head_name.
      rv_type = c_type-branch.
      RETURN.
    ENDIF.

    IF iv_branch_name CP 'refs/tags/*'.
      rv_type = c_type-tag.
    ENDIF.

  ENDMETHOD.  "get_type

  METHOD complete_heads_branch_name.
    IF iv_branch_name CP 'refs/heads/*'.
      rv_name = iv_branch_name.
    ELSE.
      rv_name = 'refs/heads/' && iv_branch_name.
    ENDIF.
  ENDMETHOD.  "complete_heads_branch_name

  METHOD get_branches_only.
    FIELD-SYMBOLS <branch> LIKE LINE OF mt_branches.

    LOOP AT mt_branches ASSIGNING <branch>.
      IF <branch>-type = c_type-branch.
        APPEND <branch> TO rt_branches.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.  "get_branches_only

  METHOD get_tags_only.
    FIELD-SYMBOLS <branch> LIKE LINE OF mt_branches.

    LOOP AT mt_branches ASSIGNING <branch>.
      IF <branch>-type = c_type-tag.
        APPEND <branch> TO rt_branches.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.  "get_tags_only

  METHOD normalize_branch_name.

    rv_name = iv_branch_name. " Force convert to string
    REPLACE ALL OCCURRENCES OF ` ` IN rv_name WITH '-'. " Disallow space in branch name

  ENDMETHOD.  " normalize_branch_name.

ENDCLASS. "lcl_git_branch_list
