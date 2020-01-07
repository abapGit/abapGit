CLASS zcl_abapgit_migrations DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS run
      RAISING zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS local_dot_abapgit
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_MIGRATIONS IMPLEMENTATION.


  METHOD local_dot_abapgit.

    DATA: lt_repos       TYPE zif_abapgit_definitions=>ty_repo_ref_tt,
          lo_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit.

    FIELD-SYMBOLS: <lo_repo> LIKE LINE OF lt_repos.


    lt_repos = zcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_repos ASSIGNING <lo_repo>.
      lo_dot_abapgit = <lo_repo>->get_dot_abapgit( ).
      IF lo_dot_abapgit->get_data( ) IS INITIAL.
        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            titel = 'Migration'
            txt1  = 'Automatic migration of .abapgit.xml removed'
            txt2  = 'Remove all repos and install latest abapGit version'.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD run.

    " Migrate STDTEXT to TABLE
    zcl_abapgit_persist_migrate=>run( ).

    " Create ZIF_APACK_MANIFEST interface
    zcl_abapgit_apack_migration=>run( ).

    " local .abapgit.xml state, issue #630
    local_dot_abapgit( ).

  ENDMETHOD.
ENDCLASS.
