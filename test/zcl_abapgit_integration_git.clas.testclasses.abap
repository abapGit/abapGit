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

CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS test01 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test01.

    CONSTANTS lc_url TYPE string VALUE 'https://github.com/abapGit/abapGit.git'.
    DATA lv_ref TYPE string.
    DATA li_memory TYPE REF TO lcl_memory_settings.

    CREATE OBJECT li_memory.
    zcl_abapgit_persist_injector=>set_settings( li_memory ).

    lv_ref = zcl_abapgit_git_transport=>branches( lc_url )->get_head_symref( ).

    cl_abap_unit_assert=>assert_equals(
       act = lv_ref
       exp = 'refs/heads/main' ).

**    TRY.
*    zcl_abapgit_git_porcelain=>pull_by_branch(
*        iv_url          =
*        iv_branch_name  = 'refs/head/main'
**        iv_deepen_level = 1
*           ).
**      CATCH zcx_abapgit_exception.
**    ENDTRY.

  ENDMETHOD.

ENDCLASS.