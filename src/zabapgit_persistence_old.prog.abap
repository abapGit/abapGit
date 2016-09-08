*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PERSISTENCE_OLD
*&---------------------------------------------------------------------*

CLASS lcl_persistence_migrate DEFINITION DEFERRED.

*----------------------------------------------------------------------*
*       CLASS lcl_persistence DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_persistence DEFINITION FINAL FRIENDS lcl_persistence_migrate.

* this class is obsolete, use LCL_PERSISTENCE_REPO instead

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_repo_persi,
             url         TYPE string,
             branch_name TYPE string,
             sha1        TYPE ty_sha1,
             package     TYPE devclass,
             offline     TYPE sap_bool,
           END OF ty_repo_persi.
    TYPES: ty_repos_persi_tt TYPE STANDARD TABLE OF ty_repo_persi WITH DEFAULT KEY.

    METHODS list
      RETURNING VALUE(rt_repos) TYPE ty_repos_persi_tt
      RAISING   lcx_exception.

    METHODS update
      IMPORTING iv_url         TYPE ty_repo_persi-url
                iv_branch_name TYPE ty_repo_persi-branch_name
                iv_branch      TYPE ty_sha1
      RAISING   lcx_exception.

    METHODS add
      IMPORTING iv_url         TYPE string
                iv_branch_name TYPE string
                iv_branch      TYPE ty_sha1 OPTIONAL
                iv_package     TYPE devclass
                iv_offline     TYPE sap_bool DEFAULT abap_false
      RAISING   lcx_exception.

    METHODS delete
      IMPORTING iv_url         TYPE ty_repo_persi-url
                iv_branch_name TYPE ty_repo_persi-branch_name
      RAISING   lcx_exception.

    METHODS read_text_online
      RETURNING VALUE(rt_repos) TYPE ty_repos_persi_tt
      RAISING   lcx_exception.

    METHODS save_text_online
      IMPORTING it_repos TYPE ty_repos_persi_tt
      RAISING   lcx_exception.

    METHODS header_online
      RETURNING VALUE(rs_header) TYPE thead.

    METHODS read_text_offline
      RETURNING VALUE(rt_repos) TYPE ty_repos_persi_tt
      RAISING   lcx_exception.

    METHODS save_text_offline
      IMPORTING it_repos TYPE ty_repos_persi_tt
      RAISING   lcx_exception.

    METHODS header_offline
      RETURNING VALUE(rs_header) TYPE thead.

    METHODS read_text
      IMPORTING is_header       TYPE thead
      RETURNING VALUE(rt_lines) TYPE tlinetab
      RAISING   lcx_exception.

    METHODS save_text
      IMPORTING is_header TYPE thead
                it_lines  TYPE tlinetab
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_persistence DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_persistence IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_persistence IMPLEMENTATION.

  METHOD save_text.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header   = is_header
      TABLES
        lines    = it_lines
      EXCEPTIONS
        id       = 1
        language = 2
        name     = 3
        object   = 4
        OTHERS   = 5.
    IF sy-subrc <> 0.
      ROLLBACK WORK.                                   "#EC CI_ROLLBACK
      lcx_exception=>raise( 'error from SAVE_TEXT' ).
    ENDIF.

  ENDMETHOD.                    "save_text

  METHOD header_online.
    rs_header-tdid     = 'ST'.
    rs_header-tdspras  = gc_english.
    rs_header-tdname   = 'ZABAPGIT'.
    rs_header-tdobject = 'TEXT'.
  ENDMETHOD.                    "header

  METHOD header_offline.
    rs_header-tdid     = 'ST'.
    rs_header-tdspras  = gc_english.
    rs_header-tdname   = 'ZABAPGIT_OFFLINE'.
    rs_header-tdobject = 'TEXT'.
  ENDMETHOD.                    "header_offline

  METHOD delete.

    DATA: lt_repos TYPE ty_repos_persi_tt.


    lt_repos = list( ).

    DELETE lt_repos WHERE url = iv_url AND branch_name = iv_branch_name.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'repo not found, delete' ).
    ENDIF.

    save_text_online( lt_repos ).
    save_text_offline( lt_repos ).

  ENDMETHOD.                    "delete

  METHOD save_text_online.

    DATA: lt_lines  TYPE TABLE OF tline.

    FIELD-SYMBOLS: <ls_repo> LIKE LINE OF it_repos,
                   <ls_line> LIKE LINE OF lt_lines.


    LOOP AT it_repos ASSIGNING <ls_repo> WHERE offline = abap_false.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-url.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-branch_name.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-sha1.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-package.
    ENDLOOP.

    save_text( is_header = header_online( )
               it_lines  = lt_lines ).

    COMMIT WORK.

  ENDMETHOD.                    "save_text

  METHOD save_text_offline.

    DATA: lt_lines  TYPE TABLE OF tline.

    FIELD-SYMBOLS: <ls_repo> LIKE LINE OF it_repos,
                   <ls_line> LIKE LINE OF lt_lines.


    LOOP AT it_repos ASSIGNING <ls_repo> WHERE offline = abap_true.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-url.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-package.
    ENDLOOP.

    save_text( is_header = header_offline( )
               it_lines  = lt_lines ).

    COMMIT WORK.

  ENDMETHOD.                    "save_text_offline

  METHOD add.

    DATA: lt_repos TYPE ty_repos_persi_tt.

    FIELD-SYMBOLS: <ls_repo> LIKE LINE OF lt_repos.


    ASSERT NOT iv_url IS INITIAL.
    ASSERT NOT iv_package IS INITIAL.

    lt_repos = list( ).

    READ TABLE lt_repos WITH KEY url = iv_url branch_name = iv_branch_name
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lcx_exception=>raise( 'already inserted' ).
    ENDIF.

    APPEND INITIAL LINE TO lt_repos ASSIGNING <ls_repo>.
    <ls_repo>-url         = iv_url.
    <ls_repo>-branch_name = iv_branch_name.
    <ls_repo>-sha1        = iv_branch.
    <ls_repo>-package     = iv_package.
    <ls_repo>-offline     = iv_offline.

    save_text_online( lt_repos ).
    save_text_offline( lt_repos ).

  ENDMETHOD.                    "insert

  METHOD update.

    DATA: lt_repos TYPE ty_repos_persi_tt.

    FIELD-SYMBOLS: <ls_repo> LIKE LINE OF lt_repos.


    IF iv_branch IS INITIAL.
      lcx_exception=>raise( 'update, sha empty' ).
    ENDIF.

    lt_repos = list( ).

    READ TABLE lt_repos ASSIGNING <ls_repo>
      WITH KEY url = iv_url branch_name = iv_branch_name.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'persist update, repo not found' ).
    ENDIF.

    <ls_repo>-sha1 = iv_branch.

    save_text_online( lt_repos ).

  ENDMETHOD.                    "update

  METHOD list.
    CLEAR rt_repos.
    APPEND LINES OF read_text_online( ) TO rt_repos.
    APPEND LINES OF read_text_offline( ) TO rt_repos.
  ENDMETHOD.                    "list

  METHOD read_text.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = is_header-tdid
        language                = is_header-tdspras
        name                    = is_header-tdname
        object                  = is_header-tdobject
      TABLES
        lines                   = rt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc = 4.
      RETURN.
    ELSEIF sy-subrc <> 0.
      lcx_exception=>raise( 'Error from READ_TEXT' ).
    ENDIF.

  ENDMETHOD.                    "read_text

  METHOD read_text_online.

    DATA: lt_lines TYPE TABLE OF tline,
          lv_step  TYPE i,
          ls_repo  TYPE ty_repo_persi.

    FIELD-SYMBOLS: <ls_line> LIKE LINE OF lt_lines.


    lt_lines = read_text( header_online( ) ).
    IF lines( lt_lines ) = 0.
      RETURN.
    ENDIF.

    IF lines( lt_lines ) MOD 4 <> 0.
* if this happens, delete text ZABAPGIT in SO10 or edit the text
* manually, so it contains the right information
      lcx_exception=>raise( 'Persistence, text broken' ).
    ENDIF.

    CLEAR ls_repo.
    LOOP AT lt_lines ASSIGNING <ls_line>.
      lv_step = lv_step + 1.
      CASE lv_step.
        WHEN 4.
          ls_repo-package = <ls_line>-tdline.

          IF ls_repo-url IS INITIAL OR ls_repo-branch_name IS INITIAL.
            lcx_exception=>raise( 'Persistence, text broken 2' ).
          ENDIF.
          APPEND ls_repo TO rt_repos.
          CLEAR ls_repo.
          lv_step = 0.
        WHEN 3.
          ls_repo-sha1 = <ls_line>-tdline.
        WHEN 2.
          ls_repo-branch_name = <ls_line>-tdline.
        WHEN 1.
          ls_repo-url = <ls_line>-tdline.
        WHEN OTHERS.
          ASSERT 1 = 0.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.                    "list

  METHOD read_text_offline.

    DATA: lt_lines TYPE TABLE OF tline,
          ls_repo  TYPE ty_repo_persi.

    FIELD-SYMBOLS: <ls_line> LIKE LINE OF lt_lines.


    lt_lines = read_text( header_offline( ) ).
    IF lines( lt_lines ) = 0.
      RETURN.
    ENDIF.

    IF lines( lt_lines ) MOD 2 <> 0.
* if this happens, delete text ZABAPGIT in SO10 or edit the text
* manually, so it contains the right information
      lcx_exception=>raise( 'Persistence, text broken' ).
    ENDIF.

    CLEAR ls_repo.
    LOOP AT lt_lines ASSIGNING <ls_line>.
      IF <ls_line>-tdline IS INITIAL.
        lcx_exception=>raise( 'Persistence, text broken' ).
      ENDIF.
      IF ls_repo-url IS INITIAL.
        ls_repo-url = <ls_line>-tdline.
        CONTINUE. " current loop
      ENDIF.

      ls_repo-package = <ls_line>-tdline.
      ls_repo-offline = abap_true.
      APPEND ls_repo TO rt_repos.
      CLEAR ls_repo.
    ENDLOOP.

  ENDMETHOD.                    "list

ENDCLASS.                    "lcl_persistence IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_user DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_user DEFINITION FINAL FRIENDS lcl_persistence_migrate.

* this class is obsolete, use LCL_PERSISTENCE_USER instead

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_user,
             user     LIKE sy-uname,
             username TYPE string,
             email    TYPE string,
           END OF ty_user.

    TYPES: ty_user_tt TYPE STANDARD TABLE OF ty_user WITH DEFAULT KEY.

    CLASS-METHODS set_username
      IMPORTING iv_user     TYPE xubname DEFAULT sy-uname
                iv_username TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS get_username
      IMPORTING iv_user            TYPE xubname DEFAULT sy-uname
      RETURNING VALUE(rv_username) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS set_email
      IMPORTING iv_user  TYPE xubname DEFAULT sy-uname
                iv_email TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS get_email
      IMPORTING iv_user         TYPE xubname DEFAULT sy-uname
      RETURNING VALUE(rv_email) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS list
      RETURNING VALUE(rt_data) TYPE ty_user_tt
      RAISING   lcx_exception.

    CLASS-METHODS read
      IMPORTING iv_name         TYPE tdobname
      RETURNING VALUE(rv_value) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS save
      IMPORTING iv_name  TYPE tdobname
                iv_value TYPE string
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_user DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_user IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_user IMPLEMENTATION.

* this class is obsolete, use LCL_PERSISTENCE_USER instead

  METHOD read.

    DATA: lt_lines TYPE TABLE OF tline,
          ls_line  LIKE LINE OF lt_lines.


    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ST'
        language                = gc_english
        name                    = iv_name
        object                  = 'TEXT'
      TABLES
        lines                   = lt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 4 AND sy-subrc <> 0.
      lcx_exception=>raise( 'error from READ_TEXT' ).
    ENDIF.

    READ TABLE lt_lines INTO ls_line INDEX 1.
    IF sy-subrc = 0.
      rv_value = ls_line-tdline.
    ENDIF.

  ENDMETHOD.                    "get_details

  METHOD save.

    DATA: ls_header TYPE thead,
          lt_lines  TYPE TABLE OF tline,
          ls_line   LIKE LINE OF lt_lines.


    ls_line-tdformat = '*'.
    ls_line-tdline = iv_value.
    APPEND ls_line TO lt_lines.

    ls_header-tdid       = 'ST'.
    ls_header-tdspras    = gc_english.
    ls_header-tdname     = iv_name.
    ls_header-tdobject   = 'TEXT'.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header   = ls_header
      TABLES
        lines    = lt_lines
      EXCEPTIONS
        id       = 1
        language = 2
        name     = 3
        object   = 4
        OTHERS   = 5.
    IF sy-subrc <> 0.
      ROLLBACK WORK.                                   "#EC CI_ROLLBACK
      lcx_exception=>raise( 'error from SAVE_TEXT' ).
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.                    "change

  METHOD set_username.

    DATA: lv_name TYPE tdobname.


    CONCATENATE 'ZABAPGIT_USERNAME_' iv_user INTO lv_name.

    save( iv_name  = lv_name
          iv_value = iv_username ).

  ENDMETHOD.                    "set_username

  METHOD get_username.

    DATA: lv_name TYPE tdobname.


    CONCATENATE 'ZABAPGIT_USERNAME_' iv_user INTO lv_name.

    rv_username = read( lv_name ).

  ENDMETHOD.                    "get_username

  METHOD set_email.

    DATA: lv_name TYPE tdobname.


    CONCATENATE 'ZABAPGIT_EMAIL_' iv_user INTO lv_name.

    save( iv_name  = lv_name
          iv_value = iv_email ).

  ENDMETHOD.                    "set_email

  METHOD list.

    DATA: lt_stxh TYPE STANDARD TABLE OF stxh WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_output> LIKE LINE OF rt_data,
                   <ls_stxh>   LIKE LINE OF lt_stxh.


    SELECT * FROM stxh INTO TABLE lt_stxh
      WHERE tdobject = 'TEXT'
      AND tdname LIKE 'ZABAPGIT_USERNAME_%'.

    LOOP AT lt_stxh ASSIGNING <ls_stxh>.
      APPEND INITIAL LINE TO rt_data ASSIGNING <ls_output>.

      <ls_output>-user     = <ls_stxh>-tdname+18.
      <ls_output>-username = get_username( <ls_output>-user ).
      <ls_output>-email    = get_email( <ls_output>-user ).
    ENDLOOP.

  ENDMETHOD.

  METHOD get_email.

    DATA: lv_name TYPE tdobname.


    CONCATENATE 'ZABAPGIT_EMAIL_' iv_user INTO lv_name.

    rv_email = read( lv_name ).

  ENDMETHOD.                    "get_email

ENDCLASS.                    "lcl_user IMPLEMENTATION