CLASS zcl_abapgit_git_url DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS get_commit_display_url
      IMPORTING
        !io_repo      TYPE REF TO zcl_abapgit_repo_online
      RETURNING
        VALUE(rv_url) TYPE string
      RAISING
        zcx_abapgit_exception .

    METHODS validate_url
      IMPORTING
        !iv_url TYPE string
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.

    METHODS get_default_commit_display_url
      IMPORTING
        !iv_repo_url         TYPE string
        !iv_hash             TYPE zif_abapgit_git_definitions=>ty_sha1
      RETURNING
        VALUE(rv_commit_url) TYPE string
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_git_url IMPLEMENTATION.


  METHOD get_commit_display_url.

    DATA li_exit TYPE REF TO zif_abapgit_exit.

    rv_url = get_default_commit_display_url(
      iv_repo_url = io_repo->get_url( )
      iv_hash     = io_repo->get_current_remote( ) ).

    li_exit = zcl_abapgit_exit=>get_instance( ).
    li_exit->adjust_display_commit_url(
      EXPORTING
        iv_repo_url    = io_repo->get_url( )
        iv_repo_name   = io_repo->get_name( )
        iv_repo_key    = io_repo->get_key( )
        iv_commit_hash = io_repo->get_current_remote( )
      CHANGING
        cv_display_url = rv_url ).

    IF rv_url IS INITIAL.
      zcx_abapgit_exception=>raise( |provider not yet supported| ).
    ENDIF.

  ENDMETHOD.


  METHOD get_default_commit_display_url.

    DATA ls_result TYPE match_result.
    FIELD-SYMBOLS <ls_provider_match> TYPE submatch_result.

    rv_commit_url = iv_repo_url.

    FIND REGEX '^http(?:s)?:\/\/(?:www\.)?(github\.com|bitbucket\.org|gitlab\.com)\/'
      IN rv_commit_url
      RESULTS ls_result.
    IF sy-subrc = 0.
      READ TABLE ls_result-submatches INDEX 1 ASSIGNING <ls_provider_match>.
      CASE rv_commit_url+<ls_provider_match>-offset(<ls_provider_match>-length).
        WHEN 'github.com'.
          REPLACE REGEX '\.git$' IN rv_commit_url WITH space.
          rv_commit_url = rv_commit_url && |/commit/| && iv_hash.
        WHEN 'bitbucket.org'.
          REPLACE REGEX '\.git$' IN rv_commit_url WITH space.
          rv_commit_url = rv_commit_url && |/commits/| && iv_hash.
        WHEN 'gitlab.com'.
          REPLACE REGEX '\.git$' IN rv_commit_url WITH space.
          rv_commit_url = rv_commit_url && |/-/commit/| && iv_hash.
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD validate_url.

    DATA lv_provider TYPE string.

    lv_provider = zcl_abapgit_url=>host( to_lower( iv_url ) ).

    " Provider-specific check for URLs that don't work
    IF lv_provider CS 'gitlab.com'.
      FIND REGEX '\.git$' IN iv_url IGNORING CASE.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Repo URL for GitLab must end in ".git"' ).
      ENDIF.
    ELSEIF lv_provider CS 'dev.azure.com'.
      FIND REGEX '\.git$' IN iv_url IGNORING CASE.
      IF sy-subrc = 0.
        zcx_abapgit_exception=>raise( 'Repo URL for Azure DevOps must not end in ".git"' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
