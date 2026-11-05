CLASS ltcl_create_branch DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      name_without_prefix FOR TESTING.

    DATA mi_cut TYPE REF TO zif_abapgit_repo_online.

ENDCLASS.

CLASS ltcl_create_branch IMPLEMENTATION.

  METHOD setup.

    DATA ls_data TYPE zif_abapgit_persistence=>ty_repo.

    ls_data-key = '1'.
    ls_data-url = 'https://github.com/abapGit/abapGit.git'.

    CREATE OBJECT mi_cut TYPE zcl_abapgit_repo_online
      EXPORTING
        is_data = ls_data.

  ENDMETHOD.

  METHOD name_without_prefix.

    " a caller without the GUI must get an exception, not a short dump
    TRY.
        mi_cut->create_branch( 'feature' ).
        cl_abap_unit_assert=>fail( 'Exception expected' ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
