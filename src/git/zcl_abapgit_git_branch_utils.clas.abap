CLASS zcl_abapgit_git_branch_utils DEFINITION PUBLIC.
  PUBLIC SECTION.

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
    CLASS-METHODS get_description
      IMPORTING
        !iv_branch_name       TYPE clike
      RETURNING
        VALUE(rv_description) TYPE string.
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
ENDCLASS.

CLASS zcl_abapgit_git_branch_utils IMPLEMENTATION.

  METHOD complete_heads_branch_name.
    IF iv_branch_name CP zif_abapgit_git_definitions=>c_git_branch-heads.
      rv_name = iv_branch_name.
    ELSE.
      rv_name = zif_abapgit_git_definitions=>c_git_branch-heads_prefix && iv_branch_name.
    ENDIF.
  ENDMETHOD.

  METHOD get_description.

    CASE get_type( iv_branch_name ).
      WHEN zif_abapgit_git_definitions=>c_git_branch_type-branch.
        rv_description = 'Branch'.
      WHEN zif_abapgit_git_definitions=>c_git_branch_type-lightweight_tag.
        rv_description = 'Tag'.
      WHEN zif_abapgit_git_definitions=>c_git_branch_type-annotated_tag.
        rv_description = 'Annotated Tag'.
      WHEN OTHERS.
        rv_description = 'Branch'.
    ENDCASE.

    rv_description = |{ rv_description } "{ get_display_name( iv_branch_name ) }"|.

  ENDMETHOD.

  METHOD get_display_name.
    rv_display_name = iv_branch_name.

    IF rv_display_name CP zif_abapgit_git_definitions=>c_git_branch-heads.
      REPLACE FIRST OCCURRENCE OF zif_abapgit_git_definitions=>c_git_branch-heads_prefix IN rv_display_name WITH ''.
    ELSEIF rv_display_name CP zif_abapgit_git_definitions=>c_git_branch-tags.
      rv_display_name = zcl_abapgit_git_tag=>remove_tag_prefix( zcl_abapgit_git_tag=>remove_peel( rv_display_name ) ).
    ENDIF.

  ENDMETHOD.

  METHOD get_type.

    FIELD-SYMBOLS: <lv_result> TYPE LINE OF string_table.

    rv_type = zif_abapgit_git_definitions=>c_git_branch_type-other.

    IF iv_branch_name CP zif_abapgit_git_definitions=>c_git_branch-heads OR
       iv_branch_name = zif_abapgit_git_definitions=>c_head_name.
      rv_type = zif_abapgit_git_definitions=>c_git_branch_type-branch.

    ELSEIF iv_branch_name CP zif_abapgit_git_definitions=>c_git_branch-tags.

      READ TABLE it_result ASSIGNING <lv_result>
                           INDEX iv_current_row_index + 1.
      IF sy-subrc = 0 AND <lv_result> CP '*' && zcl_abapgit_git_tag=>add_peel( iv_branch_name ).
        rv_type = zif_abapgit_git_definitions=>c_git_branch_type-annotated_tag.
      ELSE.
        rv_type = zif_abapgit_git_definitions=>c_git_branch_type-lightweight_tag.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD normalize_branch_name.

    rv_name = iv_branch_name. " Force convert to string
    REPLACE ALL OCCURRENCES OF ` ` IN rv_name WITH '-'. " Disallow space in branch name

  ENDMETHOD.

ENDCLASS.
