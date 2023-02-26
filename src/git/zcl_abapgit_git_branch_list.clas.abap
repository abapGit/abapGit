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
        VALUE(rs_branch) TYPE zif_abapgit_git_definitions=>ty_git_branch
      RAISING
        zcx_abapgit_exception .
    METHODS get_head_symref
      RETURNING
        VALUE(rv_head_symref) TYPE string .
    METHODS get_all
      RETURNING
        VALUE(rt_branches) TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
      RAISING
        zcx_abapgit_exception .
    METHODS get_branches_only
      RETURNING
        VALUE(rt_branches) TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
      RAISING
        zcx_abapgit_exception .
    METHODS get_tags_only             " For potential future use
      RETURNING
        VALUE(rt_tags) TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_display_name
      IMPORTING
        !iv_branch_name        TYPE clike
      RETURNING
        VALUE(rv_display_name) TYPE string .
    CLASS-METHODS get_type
      IMPORTING
        !iv_branch_name       TYPE clike
        !it_result            TYPE string_table OPTIONAL
        !iv_current_row_index TYPE sy-tabix OPTIONAL
      RETURNING
        VALUE(rv_type)        TYPE zif_abapgit_git_definitions=>ty_git_branch_type .
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
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt .
    DATA mv_head_symref TYPE string .

    CLASS-METHODS skip_first_pkt
      IMPORTING
        !iv_data       TYPE string
      RETURNING
        VALUE(rv_data) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS find_tag_by_name
      IMPORTING
        !iv_branch_name  TYPE string
      RETURNING
        VALUE(rs_branch) TYPE zif_abapgit_git_definitions=>ty_git_branch
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS parse_branch_list
      IMPORTING
        !iv_data        TYPE string
      EXPORTING
        !et_list        TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
        !ev_head_symref TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS parse_head_params
      IMPORTING
        !iv_data              TYPE string
      RETURNING
        VALUE(rv_head_symref) TYPE string .
ENDCLASS.



CLASS zcl_abapgit_git_branch_list IMPLEMENTATION.


  METHOD complete_heads_branch_name.
    IF iv_branch_name CP zif_abapgit_definitions=>c_git_branch-heads.
      rv_name = iv_branch_name.
    ELSE.
      rv_name = zif_abapgit_definitions=>c_git_branch-heads_prefix && iv_branch_name.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.

    parse_branch_list(
      EXPORTING
        iv_data        = iv_data
      IMPORTING
        et_list        = mt_branches
        ev_head_symref = mv_head_symref ).

  ENDMETHOD.


  METHOD find_by_name.

    IF iv_branch_name IS INITIAL.
      zcx_abapgit_exception=>raise( 'Branch name empty' ).
    ENDIF.

    IF iv_branch_name CP zif_abapgit_definitions=>c_git_branch-tags.
      rs_branch = find_tag_by_name( iv_branch_name ).
    ELSE.

      READ TABLE mt_branches INTO rs_branch
        WITH TABLE KEY name_key
        COMPONENTS name = iv_branch_name.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Branch { get_display_name( iv_branch_name )
          } not found. Use 'Branch' > 'Switch' to select a different branch| ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD find_tag_by_name.

    READ TABLE mt_branches INTO rs_branch
        WITH TABLE KEY name_key
        COMPONENTS name = zcl_abapgit_git_tag=>add_peel( iv_branch_name ).
    IF sy-subrc <> 0.

      READ TABLE mt_branches INTO rs_branch
        WITH TABLE KEY name_key
        COMPONENTS name = iv_branch_name.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Branch not found' ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD get_all.

    rt_branches = mt_branches.

  ENDMETHOD.


  METHOD get_branches_only.
    FIELD-SYMBOLS <ls_branch> LIKE LINE OF mt_branches.

    LOOP AT mt_branches ASSIGNING <ls_branch>.
      IF <ls_branch>-type = zif_abapgit_definitions=>c_git_branch_type-branch.
        APPEND <ls_branch> TO rt_branches.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_display_name.
    rv_display_name = iv_branch_name.

    IF rv_display_name CP zif_abapgit_definitions=>c_git_branch-heads.
      REPLACE FIRST OCCURRENCE OF zif_abapgit_definitions=>c_git_branch-heads_prefix IN rv_display_name WITH ''.
    ELSEIF rv_display_name CP zif_abapgit_definitions=>c_git_branch-tags.
      rv_display_name = zcl_abapgit_git_tag=>remove_tag_prefix( zcl_abapgit_git_tag=>remove_peel( rv_display_name ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_head_symref.
    rv_head_symref = mv_head_symref.
  ENDMETHOD.


  METHOD get_tags_only.
    FIELD-SYMBOLS <ls_branch> LIKE LINE OF mt_branches.

    LOOP AT mt_branches ASSIGNING <ls_branch>
        WHERE type = zif_abapgit_definitions=>c_git_branch_type-lightweight_tag
        OR type = zif_abapgit_definitions=>c_git_branch_type-annotated_tag.
      APPEND <ls_branch> TO rt_tags.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_type.

    FIELD-SYMBOLS: <lv_result> TYPE LINE OF string_table.

    rv_type = zif_abapgit_definitions=>c_git_branch_type-other.

    IF iv_branch_name CP zif_abapgit_definitions=>c_git_branch-heads OR
       iv_branch_name = zif_abapgit_definitions=>c_head_name.
      rv_type = zif_abapgit_definitions=>c_git_branch_type-branch.

    ELSEIF iv_branch_name CP zif_abapgit_definitions=>c_git_branch-tags.

      READ TABLE it_result ASSIGNING <lv_result>
                           INDEX iv_current_row_index + 1.
      IF sy-subrc = 0 AND <lv_result> CP '*' && zcl_abapgit_git_tag=>add_peel( iv_branch_name ).
        rv_type = zif_abapgit_definitions=>c_git_branch_type-annotated_tag.
      ELSE.
        rv_type = zif_abapgit_definitions=>c_git_branch_type-lightweight_tag.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD normalize_branch_name.

    rv_name = iv_branch_name. " Force convert to string
    REPLACE ALL OCCURRENCES OF ` ` IN rv_name WITH '-'. " Disallow space in branch name

  ENDMETHOD.


  METHOD parse_branch_list.

    DATA: lt_result            TYPE TABLE OF string,
          lv_hash              TYPE zif_abapgit_git_definitions=>ty_sha1,
          lv_name              TYPE string,
          lv_head_params       TYPE string,
          lv_char              TYPE c,
          lv_data              LIKE LINE OF lt_result,
          lv_current_row_index TYPE syst-tabix.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF et_list.

    CLEAR: et_list, ev_head_symref.

    lv_data = skip_first_pkt( iv_data ).
    SPLIT lv_data AT cl_abap_char_utilities=>newline INTO TABLE lt_result.

    LOOP AT lt_result INTO lv_data.
      lv_current_row_index = sy-tabix.

      IF sy-tabix = 1 AND strlen( lv_data ) > 12 AND lv_data(4) = '0000' AND lv_data+8(3) = 'ERR'.
        lv_name = lv_data+8.
        zcx_abapgit_exception=>raise( lv_name ).
      ELSEIF sy-tabix = 1 AND strlen( lv_data ) > 49.
        lv_hash = lv_data+8.
        lv_name = lv_data+49.
        lv_char = zcl_abapgit_git_utils=>get_null( ).

        SPLIT lv_name AT lv_char INTO lv_name lv_head_params.
        ev_head_symref = parse_head_params( lv_head_params ).
        IF ev_head_symref IS INITIAL AND lv_name CS 'refs/heads/'.
          ev_head_symref = lv_name.
        ENDIF.
      ELSEIF sy-tabix > 1 AND strlen( lv_data ) > 45.
        lv_hash = lv_data+4.
        lv_name = lv_data+45.
      ELSEIF sy-tabix = 1 AND strlen( lv_data ) = 8 AND lv_data(8) = '00000000'.
        zcx_abapgit_exception=>raise( 'No branches, create branch manually by adding file' ).
      ELSE.
        CONTINUE.
      ENDIF.

      ASSERT lv_name IS NOT INITIAL.

      APPEND INITIAL LINE TO et_list ASSIGNING <ls_branch>.
      <ls_branch>-sha1         = lv_hash.
      <ls_branch>-name         = lv_name.
      <ls_branch>-display_name = get_display_name( lv_name ).
      <ls_branch>-type         = get_type( iv_branch_name       = lv_name
                                           it_result            = lt_result
                                           iv_current_row_index = lv_current_row_index ).
      IF <ls_branch>-name = zif_abapgit_definitions=>c_head_name OR <ls_branch>-name = ev_head_symref.
        <ls_branch>-is_head = abap_true.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD parse_head_params.

    DATA: ls_match    TYPE match_result,
          ls_submatch LIKE LINE OF ls_match-submatches.

    FIND FIRST OCCURRENCE OF REGEX '\ssymref=HEAD:([^\s]+)' IN iv_data RESULTS ls_match.
    READ TABLE ls_match-submatches INTO ls_submatch INDEX 1.
    IF sy-subrc IS INITIAL.
      rv_head_symref = iv_data+ls_submatch-offset(ls_submatch-length).
    ENDIF.

  ENDMETHOD.


  METHOD skip_first_pkt.

    DATA: lv_hex    TYPE x LENGTH 1,
          lv_length TYPE i.

* channel
    ASSERT iv_data(2) = '00'.

    lv_hex = to_upper( iv_data+2(2) ).
    lv_length = lv_hex.

    rv_data = iv_data+lv_length.

  ENDMETHOD.
ENDCLASS.
