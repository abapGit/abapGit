CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL CRITICAL FINAL.

  PRIVATE SECTION.
    METHODS test01 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test01.

    zcl_abapgit_gitv2_porcelain=>list_branches( 'https://github.com/abapGit/abapGit.git' ).

  ENDMETHOD.

ENDCLASS.
