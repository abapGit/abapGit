CLASS zcl_abapgit_git_branch_list DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !iv_data TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS find_by_name
      IMPORTING
        !iv_branch_name  TYPE clike
      RETURNING
        VALUE(rs_branch) TYPE zif_abapgit_definitions=>ty_git_branch
      RAISING
        zcx_abapgit_exception .
    METHODS get_head   " For potential future use
      RETURNING
        VALUE(rs_branch) TYPE zif_abapgit_definitions=>ty_git_branch
      RAISING
        zcx_abapgit_exception .
    METHODS get_head_symref
      RETURNING
        VALUE(rv_head_symref) TYPE string .
    METHODS get_branches_only
      RETURNING
        VALUE(rt_branches) TYPE zif_abapgit_definitions=>ty_git_branch_list_tt
      RAISING
        zcx_abapgit_exception .
    METHODS get_tags_only   " For potential future use
      RETURNING
        VALUE(rt_branches) TYPE zif_abapgit_definitions=>ty_git_branch_list_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS is_ignored
      IMPORTING
        !iv_branch_name  TYPE clike
      RETURNING
        VALUE(rv_ignore) TYPE abap_bool .
    CLASS-METHODS get_display_name
      IMPORTING
        !iv_branch_name        TYPE clike
      RETURNING
        VALUE(rv_display_name) TYPE string .
    CLASS-METHODS get_type
      IMPORTING
        !iv_branch_name TYPE clike
      RETURNING
        VALUE(rv_type)  TYPE zif_abapgit_definitions=>ty_git_branch_type .
    CLASS-METHODS complete_heads_branch_name
      IMPORTING
        !iv_branch_name TYPE clike
      RETURNING
        VALUE(rv_name)  TYPE string .
    CLASS-METHODS normalize_branch_name
      IMPORTING
        !iv_branch_name TYPE clike
      RETURNING
        VALUE(rv_name)  TYPE string .
  PRIVATE SECTION.

    DATA mt_branches TYPE zif_abapgit_definitions=>ty_git_branch_list_tt .
    DATA mv_head_symref TYPE string .

    CLASS-METHODS parse_branch_list
      IMPORTING
        !iv_data        TYPE string
      EXPORTING
        !et_list        TYPE zif_abapgit_definitions=>ty_git_branch_list_tt
        !ev_head_symref TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS parse_head_params
      IMPORTING
        !iv_data              TYPE string
      RETURNING
        VALUE(rv_head_symref) TYPE string .
ENDCLASS.



CLASS ZCL_ABAPGIT_GIT_BRANCH_LIST IMPLEMENTATION.


  METHOD complete_heads_branch_name.
    IF iv_branch_name CP 'refs/heads/*'.
      rv_name = iv_branch_name.
    ELSE.
      rv_name = 'refs/heads/' && iv_branch_name.
    ENDIF.
  ENDMETHOD.  "complete_heads_branch_name


  METHOD constructor.
    parse_branch_list(
      EXPORTING iv_data        = iv_data
      IMPORTING et_list        = me->mt_branches
                ev_head_symref = me->mv_head_symref ).
  ENDMETHOD.  "create


  METHOD find_by_name.

    IF iv_branch_name IS INITIAL.
      zcx_abapgit_exception=>raise( 'Branch name empty' ).
    ENDIF.

    READ TABLE mt_branches INTO rs_branch
      WITH KEY name = iv_branch_name.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Branch not found' ).
    ENDIF.

  ENDMETHOD.  "find_by_name


  METHOD get_branches_only.
    FIELD-SYMBOLS <ls_branch> LIKE LINE OF mt_branches.

    LOOP AT mt_branches ASSIGNING <ls_branch>.
      IF <ls_branch>-type = zif_abapgit_definitions=>c_git_branch_type-branch.
        APPEND <ls_branch> TO rt_branches.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.  "get_branches_only


  METHOD get_display_name.
    rv_display_name = iv_branch_name.

    IF rv_display_name CP 'refs/heads/*'.
      REPLACE FIRST OCCURRENCE OF 'refs/heads/' IN rv_display_name WITH ''.
    ELSEIF rv_display_name CP 'refs/tags/*'.
      REPLACE FIRST OCCURRENCE OF 'refs/' IN rv_display_name WITH ''.
    ENDIF.

  ENDMETHOD.  "get_display_name


  METHOD get_head.

    IF mv_head_symref IS NOT INITIAL.
      rs_branch = find_by_name( mv_head_symref ).
    ELSE.
      rs_branch = find_by_name( zif_abapgit_definitions=>c_head_name ).
    ENDIF.

  ENDMETHOD.  "get_head


  METHOD get_head_symref.
    rv_head_symref = mv_head_symref.
  ENDMETHOD.  " get_head_symref.


  METHOD get_tags_only.
    FIELD-SYMBOLS <ls_branch> LIKE LINE OF mt_branches.

    LOOP AT mt_branches ASSIGNING <ls_branch>.
      IF <ls_branch>-type = zif_abapgit_definitions=>c_git_branch_type-tag.
        APPEND <ls_branch> TO rt_branches.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.  "get_tags_only


  METHOD get_type.
    rv_type = zif_abapgit_definitions=>c_git_branch_type-other.

    IF iv_branch_name CP 'refs/heads/*' OR iv_branch_name = zif_abapgit_definitions=>c_head_name.
      rv_type = zif_abapgit_definitions=>c_git_branch_type-branch.
      RETURN.
    ENDIF.

    IF iv_branch_name CP 'refs/tags/*'.
      rv_type = zif_abapgit_definitions=>c_git_branch_type-tag.
    ENDIF.

  ENDMETHOD.  "get_type


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


  METHOD normalize_branch_name.

    rv_name = iv_branch_name. " Force convert to string
    REPLACE ALL OCCURRENCES OF ` ` IN rv_name WITH '-'. " Disallow space in branch name

  ENDMETHOD.  " normalize_branch_name.


  METHOD parse_branch_list.

    DATA: lt_result      TYPE TABLE OF string,
          lv_hash        TYPE zif_abapgit_definitions=>ty_sha1,
          lv_name        TYPE string,
          lv_head_params TYPE string,
          lv_char        TYPE c,
          lv_data        LIKE LINE OF lt_result.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF et_list.

    CLEAR: et_list, ev_head_symref.

    SPLIT iv_data AT zif_abapgit_definitions=>gc_newline INTO TABLE lt_result.

    LOOP AT lt_result INTO lv_data.
      IF sy-tabix = 1.
        CONTINUE. " current loop
      ELSEIF sy-tabix = 2 AND strlen( lv_data ) > 49.
        lv_hash = lv_data+8.
        lv_name = lv_data+49.
        lv_char = zcl_abapgit_git_utils=>get_null( ).

        SPLIT lv_name AT lv_char INTO lv_name lv_head_params.
        ev_head_symref = parse_head_params( lv_head_params ).
      ELSEIF sy-tabix > 2 AND strlen( lv_data ) > 45.
        lv_hash = lv_data+4.
        lv_name = lv_data+45.
      ELSEIF sy-tabix = 2 AND strlen( lv_data ) = 8 AND lv_data(8) = '00000000'.
        zcx_abapgit_exception=>raise( 'No branches, create branch manually by adding file' ).
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
      IF <ls_branch>-name = zif_abapgit_definitions=>c_head_name OR <ls_branch>-name = ev_head_symref.
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
      rv_head_symref = iv_data+ls_submatch-offset(ls_submatch-length).
    ENDIF.

  ENDMETHOD.  "parse_head_params
ENDCLASS.
