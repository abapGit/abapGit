CLASS zcl_abapgit_migrations DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS run
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_not_found.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS migrate_offline_repos.
ENDCLASS.



CLASS zcl_abapgit_migrations IMPLEMENTATION.


  METHOD migrate_offline_repos.

    DATA:
      lt_repos TYPE zif_abapgit_repo_srv=>ty_repo_list,
      li_repo  LIKE LINE OF lt_repos,
      lo_dot   TYPE REF TO zcl_abapgit_dot_abapgit.

    TRY.
        " Get offline repos only
        lt_repos = zcl_abapgit_repo_srv=>get_instance( )->list( abap_true ).

        LOOP AT lt_repos INTO li_repo.
          lo_dot = li_repo->get_dot_abapgit( ).
          " Move repo name from URL fields to .abapGit.xml
          IF li_repo->ms_data-url IS NOT INITIAL AND lo_dot->get_name( ) IS INITIAL.
            lo_dot->set_name( li_repo->ms_data-url ).
            li_repo->set_dot_abapgit( lo_dot ).
          ENDIF.
        ENDLOOP.
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD run.

    " Migrate STDTEXT to TABLE
    zcl_abapgit_persist_migrate=>run( ).

    " Create ZIF_APACK_MANIFEST interface
    zcl_abapgit_apack_migration=>run( ).

    " Migrate checksums from repo metadata to separate DB object
    zcl_abapgit_repo_cs_migration=>run( ).

    " Migrate offline repo metadata
    migrate_offline_repos( ).

  ENDMETHOD.
ENDCLASS.
