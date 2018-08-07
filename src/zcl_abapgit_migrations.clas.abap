CLASS zcl_abapgit_migrations DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS run
      RAISING zcx_abapgit_exception.

  PRIVATE SECTION.

    CLASS-METHODS rebuild_local_checksums_161112
      RAISING
        zcx_abapgit_exception .
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


  METHOD rebuild_local_checksums_161112.

    DATA: lt_repos     TYPE zif_abapgit_definitions=>ty_repo_ref_tt,
          lv_repo_list TYPE string,
          lv_index     TYPE i.

    FIELD-SYMBOLS: <lo_repo> LIKE LINE OF lt_repos.


    lt_repos = zcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_repos ASSIGNING <lo_repo>.
      lv_index = sy-tabix.

      IF <lo_repo>->is_offline( ) = abap_true. " Skip local repos
        DELETE lt_repos INDEX lv_index.
        CONTINUE.
      ENDIF.

      " Ignore empty repos or repos with file checksums
      IF lines( <lo_repo>->get_local_checksums( ) ) = 0
          OR lines( <lo_repo>->get_local_checksums_per_file( ) ) > 0.
        DELETE lt_repos INDEX lv_index.
        CONTINUE.
      ENDIF.

      lv_repo_list = lv_repo_list && `, ` && <lo_repo>->get_name( ).
    ENDLOOP.

    IF lines( lt_repos ) = 0.
      RETURN. " All OK
    ENDIF.

    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Migration'
        txt1  = 'Automatic migration of local checksums removed'
        txt2  = 'Remove all repos and install latest abapGit version'.

  ENDMETHOD.


  METHOD run.

    " Migrate STDTEXT to TABLE
    zcl_abapgit_persist_migrate=>run( ).

    " Rebuild local file checksums
    rebuild_local_checksums_161112( ).

    " local .abapgit.xml state, issue #630
    local_dot_abapgit( ).

  ENDMETHOD.  " run.
ENDCLASS.
