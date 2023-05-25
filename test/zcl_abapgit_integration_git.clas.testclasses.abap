CLASS lcl_memory_settings DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_persist_settings.
ENDCLASS.

CLASS lcl_memory_settings IMPLEMENTATION.
  METHOD zif_abapgit_persist_settings~modify.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_persist_settings~read.
    CREATE OBJECT ro_settings.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL CRITICAL FINAL.

  PRIVATE SECTION.
    METHODS test01 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test01.

    CONSTANTS lc_url TYPE string VALUE 'https://github.com/larshp/Empty.git'.
    DATA lv_ref TYPE string.
    DATA li_memory TYPE REF TO lcl_memory_settings.
    DATA ls_pull TYPE zcl_abapgit_git_porcelain=>ty_pull_result.


    CREATE OBJECT li_memory.
    zcl_abapgit_persist_injector=>set_settings( li_memory ).

    lv_ref = zcl_abapgit_git_transport=>branches( lc_url )->get_head_symref( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_ref
      exp = 'refs/heads/main' ).

    ls_pull = zcl_abapgit_git_porcelain=>pull_by_branch(
      iv_url          = lc_url
      iv_branch_name  = lv_ref ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_pull-files )
      exp = 3 ).

  ENDMETHOD.

ENDCLASS.
