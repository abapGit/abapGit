CLASS ltcl_state_test DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_sum_with_status FOR TESTING.
    METHODS test_sum_with_ritem FOR TESTING.
ENDCLASS.

CLASS ltcl_state_test IMPLEMENTATION.
  METHOD test_sum_with_status.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_result.
    DATA lo_cut TYPE REF TO zcl_abapgit_item_state.

    CREATE OBJECT lo_cut.

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->local( )
      exp = zif_abapgit_definitions=>c_state-unchanged ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->remote( )
      exp = zif_abapgit_definitions=>c_state-unchanged ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_reassigned( )
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_unchanged( )
      exp = abap_true ).

    ls_item-lstate = zif_abapgit_definitions=>c_state-added.
    ls_item-rstate = zif_abapgit_definitions=>c_state-deleted.
    lo_cut->sum_with_status_item( ls_item ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->local( )
      exp = zif_abapgit_definitions=>c_state-added ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->remote( )
      exp = zif_abapgit_definitions=>c_state-deleted ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_reassigned( )
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_unchanged( )
      exp = abap_false ).

    " Similar state
    ls_item-lstate = zif_abapgit_definitions=>c_state-added.
    ls_item-rstate = zif_abapgit_definitions=>c_state-deleted.
    lo_cut->sum_with_status_item( ls_item ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->local( )
      exp = zif_abapgit_definitions=>c_state-added ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->remote( )
      exp = zif_abapgit_definitions=>c_state-deleted ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_reassigned( )
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_unchanged( )
      exp = abap_false ).

    ls_item-lstate = zif_abapgit_definitions=>c_state-unchanged.
    ls_item-rstate = zif_abapgit_definitions=>c_state-added.
    lo_cut->sum_with_status_item( ls_item ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->local( )
      exp = zif_abapgit_definitions=>c_state-added ). " Unchanged does not update state
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->remote( )
      exp = zif_abapgit_definitions=>c_state-mixed ). " Mixed
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_reassigned( )
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_unchanged( )
      exp = abap_false ).

    ls_item-lstate = zif_abapgit_definitions=>c_state-unchanged.
    ls_item-rstate = zif_abapgit_definitions=>c_state-added.
    ls_item-packmove = abap_true.
    lo_cut->sum_with_status_item( ls_item ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->local( )
      exp = zif_abapgit_definitions=>c_state-added ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->remote( )
      exp = zif_abapgit_definitions=>c_state-mixed ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_reassigned( )
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_unchanged( )
      exp = abap_false ).

    ls_item-lstate = zif_abapgit_definitions=>c_state-unchanged.
    ls_item-rstate = zif_abapgit_definitions=>c_state-added.
    ls_item-packmove = abap_false.
    lo_cut->sum_with_status_item( ls_item ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->local( )
      exp = zif_abapgit_definitions=>c_state-added ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->remote( )
      exp = zif_abapgit_definitions=>c_state-mixed ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_reassigned( )
      exp = abap_true ). " does not reset
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_unchanged( )
      exp = abap_false ).

  ENDMETHOD.

  METHOD test_sum_with_ritem.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_repo_item.
    DATA lo_cut TYPE REF TO zcl_abapgit_item_state.

    CREATE OBJECT lo_cut.

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->local( )
      exp = zif_abapgit_definitions=>c_state-unchanged ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->remote( )
      exp = zif_abapgit_definitions=>c_state-unchanged ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_reassigned( )
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_unchanged( )
      exp = abap_true ).

    ls_item-lstate = zif_abapgit_definitions=>c_state-added.
    ls_item-rstate = zif_abapgit_definitions=>c_state-deleted.
    lo_cut->sum_with_repo_item( ls_item ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->local( )
      exp = zif_abapgit_definitions=>c_state-added ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->remote( )
      exp = zif_abapgit_definitions=>c_state-deleted ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_reassigned( )
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_unchanged( )
      exp = abap_false ).

    " Similar state
    ls_item-lstate = zif_abapgit_definitions=>c_state-added.
    ls_item-rstate = zif_abapgit_definitions=>c_state-deleted.
    lo_cut->sum_with_repo_item( ls_item ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->local( )
      exp = zif_abapgit_definitions=>c_state-added ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->remote( )
      exp = zif_abapgit_definitions=>c_state-deleted ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_reassigned( )
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_unchanged( )
      exp = abap_false ).

    ls_item-lstate = zif_abapgit_definitions=>c_state-unchanged.
    ls_item-rstate = zif_abapgit_definitions=>c_state-added.
    lo_cut->sum_with_repo_item( ls_item ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->local( )
      exp = zif_abapgit_definitions=>c_state-added ). " Unchanged does not update state
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->remote( )
      exp = zif_abapgit_definitions=>c_state-mixed ). " Mixed
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_reassigned( )
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_unchanged( )
      exp = abap_false ).

    ls_item-lstate = zif_abapgit_definitions=>c_state-unchanged.
    ls_item-rstate = zif_abapgit_definitions=>c_state-added.
    ls_item-packmove = abap_true.
    lo_cut->sum_with_repo_item( ls_item ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->local( )
      exp = zif_abapgit_definitions=>c_state-added ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->remote( )
      exp = zif_abapgit_definitions=>c_state-mixed ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_reassigned( )
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_unchanged( )
      exp = abap_false ).

    ls_item-lstate = zif_abapgit_definitions=>c_state-unchanged.
    ls_item-rstate = zif_abapgit_definitions=>c_state-added.
    ls_item-packmove = abap_false.
    lo_cut->sum_with_repo_item( ls_item ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->local( )
      exp = zif_abapgit_definitions=>c_state-added ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->remote( )
      exp = zif_abapgit_definitions=>c_state-mixed ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_reassigned( )
      exp = abap_true ). " does not reset
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_unchanged( )
      exp = abap_false ).

  ENDMETHOD.
ENDCLASS.
