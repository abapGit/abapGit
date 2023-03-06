CLASS zcl_abapgit_item_state DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS local
      RETURNING
        VALUE(rv_state) TYPE zif_abapgit_git_definitions=>ty_item_state.
    METHODS remote
      RETURNING
        VALUE(rv_state) TYPE zif_abapgit_git_definitions=>ty_item_state.
    METHODS is_reassigned
      RETURNING
        VALUE(rv_is_reassigned) TYPE abap_bool.
    METHODS is_unchanged
      RETURNING
        VALUE(rv_is_unchanged) TYPE abap_bool.
    METHODS sum_with_repo_item
      IMPORTING
        !is_repo_item TYPE zif_abapgit_definitions=>ty_repo_item.
    METHODS sum_with_status_item
      IMPORTING
        !is_status_item TYPE zif_abapgit_definitions=>ty_result.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_lstate TYPE zif_abapgit_git_definitions=>ty_item_state.
    DATA mv_rstate TYPE zif_abapgit_git_definitions=>ty_item_state.
    DATA mv_is_reassigned TYPE abap_bool.

    CLASS-METHODS reduce
      IMPORTING
        iv_prev       TYPE zif_abapgit_git_definitions=>ty_item_state
        iv_cur        TYPE zif_abapgit_git_definitions=>ty_item_state
      RETURNING
        VALUE(rv_new) TYPE zif_abapgit_git_definitions=>ty_item_state.
ENDCLASS.



CLASS ZCL_ABAPGIT_ITEM_STATE IMPLEMENTATION.


  METHOD is_reassigned.
    rv_is_reassigned = mv_is_reassigned.
  ENDMETHOD.


  METHOD is_unchanged.
    rv_is_unchanged = boolc( mv_is_reassigned = abap_false
      AND mv_lstate = zif_abapgit_definitions=>c_state-unchanged
      AND mv_rstate = zif_abapgit_definitions=>c_state-unchanged ).
  ENDMETHOD.


  METHOD local.
    rv_state = mv_lstate.
  ENDMETHOD.


  METHOD reduce.

    rv_new = iv_prev.
    IF rv_new = iv_cur OR iv_cur IS INITIAL.
      RETURN. " No change
    ELSEIF rv_new IS INITIAL.
      rv_new = iv_cur.
    ELSE.
      rv_new = zif_abapgit_definitions=>c_state-mixed.
    ENDIF.

  ENDMETHOD.


  METHOD remote.
    rv_state = mv_rstate.
  ENDMETHOD.


  METHOD sum_with_repo_item.

    mv_lstate = reduce(
      iv_prev = mv_lstate
      iv_cur  = is_repo_item-lstate ).
    mv_rstate = reduce(
      iv_prev = mv_rstate
      iv_cur  = is_repo_item-rstate ).
    mv_is_reassigned = boolc( mv_is_reassigned = abap_true OR is_repo_item-packmove = abap_true ).

  ENDMETHOD.


  METHOD sum_with_status_item.

    mv_lstate = reduce(
      iv_prev = mv_lstate
      iv_cur  = is_status_item-lstate ).
    mv_rstate = reduce(
      iv_prev = mv_rstate
      iv_cur  = is_status_item-rstate ).
    mv_is_reassigned = boolc( mv_is_reassigned = abap_true OR is_status_item-packmove = abap_true ).

  ENDMETHOD.
ENDCLASS.
