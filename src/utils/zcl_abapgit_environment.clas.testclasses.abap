CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_abapgit_environment DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mi_cut TYPE REF TO zif_abapgit_environment.

    METHODS:
      setup,
      is_cloud FOR TESTING,
      is_merged FOR TESTING,
      is_repo_object_changes_allowed FOR TESTING,
      is_restart_required FOR TESTING.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mi_cut TYPE zcl_abapgit_environment.

  ENDMETHOD.

  METHOD is_cloud.

    mi_cut->is_cloud( ).
* no assertions, just make sure the dynamic code runs

  ENDMETHOD.

  METHOD is_merged.

    mi_cut->is_merged( ).
* no assertions, just make sure the dynamic code runs

  ENDMETHOD.

  METHOD is_repo_object_changes_allowed.

    mi_cut->is_repo_object_changes_allowed( ).
* no assertions, just make sure the dynamic code runs

  ENDMETHOD.

  METHOD is_restart_required.

    mi_cut->is_restart_required( ).
* no assertions, just make sure the dynamic code runs

  ENDMETHOD.

ENDCLASS.
