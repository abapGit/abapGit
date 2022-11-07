CLASS zcl_abapgit_repo_offline DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_repo
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS set_name
      IMPORTING
        !iv_url TYPE string
      RAISING
        zcx_abapgit_exception .

    METHODS zif_abapgit_repo~get_name
        REDEFINITION .
    METHODS has_remote_source
        REDEFINITION .
  PROTECTED SECTION.

    METHODS reset_remote
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_repo_offline IMPLEMENTATION.


  METHOD has_remote_source.
    rv_yes = boolc( lines( mt_remote ) > 0 ).
  ENDMETHOD.


  METHOD reset_remote.

    DATA lt_backup LIKE mt_remote.

    " online repo has online source to renew data from, offline does not
    " so offline repo preserves the remote
    " in case of partial pull failure the user will immediately see the new difference
    " UI will detect "pullable" content based on mt_status
    " in the uniform way both for online and offline repos
    " for more details see discussion in 2096 and 1953

    lt_backup = mt_remote.
    super->reset_remote( ).
    set_files_remote( lt_backup ).

  ENDMETHOD.


  METHOD set_name.
    set( iv_url = iv_url ).
  ENDMETHOD.


  METHOD zif_abapgit_repo~get_name.
    rv_name = super->get_name( ).

    IF rv_name IS INITIAL.
      rv_name = ms_data-url.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
