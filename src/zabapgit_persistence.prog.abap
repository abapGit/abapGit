*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PERSISTENCE
*&---------------------------------------------------------------------*

CLASS lcl_persist_migrate DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: run RAISING zcx_abapgit_exception.

  PRIVATE SECTION.
    CONSTANTS:
      c_text TYPE string VALUE 'Generated by abapGit' ##NO_TEXT.

    CLASS-METHODS:
      migrate_settings
        RAISING zcx_abapgit_exception,
      table_create
        RAISING zcx_abapgit_exception,
      table_exists
        RETURNING VALUE(rv_exists) TYPE abap_bool,
      lock_create
        RAISING zcx_abapgit_exception,
      lock_exists
        RETURNING VALUE(rv_exists) TYPE abap_bool,
      settings_exists
        RETURNING VALUE(rv_exists) TYPE abap_bool.

ENDCLASS.

CLASS lcl_persist_settings DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS modify
      IMPORTING
        io_settings TYPE REF TO zcl_abapgit_settings
      RAISING
        zcx_abapgit_exception.
    METHODS read
      RETURNING
        VALUE(ro_settings) TYPE REF TO zcl_abapgit_settings.

  PRIVATE SECTION.
    DATA: mo_settings TYPE REF TO zcl_abapgit_settings.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_persistence_user DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_persistence_user DEFINITION FINAL CREATE PRIVATE FRIENDS lcl_app.

  PUBLIC SECTION.

    TYPES: tt_favorites TYPE zcl_abapgit_persistence_repo=>tt_repo_keys.

    METHODS set_default_git_user_name
      IMPORTING iv_username TYPE string
      RAISING   zcx_abapgit_exception.

    METHODS get_default_git_user_name
      RETURNING VALUE(rv_username) TYPE string
      RAISING   zcx_abapgit_exception.

    METHODS set_default_git_user_email
      IMPORTING iv_email TYPE string
      RAISING   zcx_abapgit_exception.

    METHODS get_default_git_user_email
      RETURNING VALUE(rv_email) TYPE string
      RAISING   zcx_abapgit_exception.

    METHODS set_repo_show
      IMPORTING iv_key TYPE zcl_abapgit_persistence_repo=>ty_repo-key
      RAISING   zcx_abapgit_exception.

    METHODS get_repo_show
      RETURNING VALUE(rv_key) TYPE zcl_abapgit_persistence_repo=>ty_repo-key
      RAISING   zcx_abapgit_exception.

    METHODS set_repo_git_user_name
      IMPORTING iv_url      TYPE zcl_abapgit_persistence_repo=>ty_repo-url
                iv_username TYPE string
      RAISING   zcx_abapgit_exception.

    METHODS get_repo_git_user_name
      IMPORTING iv_url             TYPE zcl_abapgit_persistence_repo=>ty_repo-url
      RETURNING VALUE(rv_username) TYPE string
      RAISING   zcx_abapgit_exception.

    METHODS set_repo_login
      IMPORTING iv_url   TYPE zcl_abapgit_persistence_repo=>ty_repo-url
                iv_login TYPE string
      RAISING   zcx_abapgit_exception.

    METHODS get_repo_login
      IMPORTING iv_url          TYPE zcl_abapgit_persistence_repo=>ty_repo-url
      RETURNING VALUE(rv_login) TYPE string
      RAISING   zcx_abapgit_exception.

    METHODS set_repo_git_user_email
      IMPORTING iv_url   TYPE zcl_abapgit_persistence_repo=>ty_repo-url
                iv_email TYPE string
      RAISING   zcx_abapgit_exception.

    METHODS get_repo_git_user_email
      IMPORTING iv_url          TYPE zcl_abapgit_persistence_repo=>ty_repo-url
      RETURNING VALUE(rv_email) TYPE string
      RAISING   zcx_abapgit_exception.

    METHODS set_repo_last_change_seen
      IMPORTING iv_url     TYPE zcl_abapgit_persistence_repo=>ty_repo-url
                iv_version TYPE string
      RAISING   zcx_abapgit_exception.

    METHODS get_repo_last_change_seen
      IMPORTING iv_url            TYPE zcl_abapgit_persistence_repo=>ty_repo-url
      RETURNING VALUE(rv_version) TYPE string
      RAISING   zcx_abapgit_exception.

    METHODS toggle_hide_files
      RETURNING VALUE(rv_hide) TYPE abap_bool
      RAISING   zcx_abapgit_exception.

    METHODS get_hide_files
      RETURNING VALUE(rv_hide) TYPE abap_bool
      RAISING   zcx_abapgit_exception.

    METHODS toggle_changes_only
      RETURNING VALUE(rv_changes_only) TYPE abap_bool
      RAISING   zcx_abapgit_exception.

    METHODS get_changes_only
      RETURNING VALUE(rv_changes_only) TYPE abap_bool
      RAISING   zcx_abapgit_exception.

    METHODS toggle_diff_unified
      RETURNING VALUE(rv_diff_unified) TYPE abap_bool
      RAISING   zcx_abapgit_exception.

    METHODS get_diff_unified
      RETURNING VALUE(rv_diff_unified) TYPE abap_bool
      RAISING   zcx_abapgit_exception.

    METHODS get_favorites
      RETURNING VALUE(rt_favorites) TYPE tt_favorites
      RAISING   zcx_abapgit_exception.

    METHODS toggle_favorite
      IMPORTING iv_repo_key TYPE zcl_abapgit_persistence_repo=>ty_repo-key
      RAISING   zcx_abapgit_exception.

    METHODS is_favorite_repo
      IMPORTING iv_repo_key   TYPE zcl_abapgit_persistence_repo=>ty_repo-key
      RETURNING VALUE(rv_yes) TYPE abap_bool
      RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA: mv_user TYPE xubname.

    TYPES:
      BEGIN OF ty_repo_config,
        url              TYPE zcl_abapgit_persistence_repo=>ty_repo-url,
        login            TYPE string,
        git_user         TYPE zif_abapgit_definitions=>ty_git_user,
        last_change_seen TYPE string,
      END OF ty_repo_config.

    TYPES: ty_repo_config_tt TYPE STANDARD TABLE OF ty_repo_config WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_user,
        default_git_user TYPE zif_abapgit_definitions=>ty_git_user,
        repo_show        TYPE zcl_abapgit_persistence_repo=>ty_repo-key,
        hide_files       TYPE abap_bool,
        changes_only     TYPE abap_bool,
        diff_unified     TYPE abap_bool,
        favorites        TYPE tt_favorites,
        repo_config      TYPE ty_repo_config_tt,
      END OF ty_user.

    METHODS constructor
      IMPORTING iv_user TYPE xubname DEFAULT sy-uname.

    METHODS from_xml
      IMPORTING iv_xml         TYPE string
      RETURNING VALUE(rs_user) TYPE ty_user
      RAISING   zcx_abapgit_exception.

    METHODS to_xml
      IMPORTING is_user       TYPE ty_user
      RETURNING VALUE(rv_xml) TYPE string.

    METHODS read
      RETURNING VALUE(rs_user) TYPE ty_user
      RAISING   zcx_abapgit_exception.

    METHODS update
      IMPORTING is_user TYPE ty_user
      RAISING   zcx_abapgit_exception.

    METHODS read_repo_config
      IMPORTING iv_url                TYPE zcl_abapgit_persistence_repo=>ty_repo-url
      RETURNING VALUE(rs_repo_config) TYPE ty_repo_config
      RAISING   zcx_abapgit_exception.

    METHODS update_repo_config
      IMPORTING iv_url         TYPE zcl_abapgit_persistence_repo=>ty_repo-url
                is_repo_config TYPE ty_repo_config
      RAISING   zcx_abapgit_exception.

ENDCLASS.             "lcl_persistence_user DEFINITION

CLASS lcl_persistence_user IMPLEMENTATION.

  METHOD constructor.
    mv_user = iv_user.
  ENDMETHOD.

  METHOD from_xml.

    DATA: lv_xml TYPE string.

    lv_xml = iv_xml.

* fix downward compatibility
    REPLACE ALL OCCURRENCES OF '<_--28C_TYPE_USER_--29>' IN lv_xml WITH '<USER>'.
    REPLACE ALL OCCURRENCES OF '</_--28C_TYPE_USER_--29>' IN lv_xml WITH '</USER>'.

    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML lv_xml
      RESULT user = rs_user ##NO_TEXT.
  ENDMETHOD.

  METHOD to_xml.
    CALL TRANSFORMATION id
      SOURCE user = is_user
      RESULT XML rv_xml.
  ENDMETHOD.

  METHOD read.

    DATA: lv_xml TYPE string.

    TRY.
        lv_xml = zcl_abapgit_persistence_db=>get_instance( )->read(
          iv_type  = zcl_abapgit_persistence_db=>c_type_user
          iv_value = mv_user ).
      CATCH zcx_abapgit_not_found.
        RETURN.
    ENDTRY.

    rs_user = from_xml( lv_xml ).

  ENDMETHOD.

  METHOD set_repo_show.

    DATA: ls_user TYPE ty_user.


    ls_user = read( ).
    ls_user-repo_show = iv_key.
    update( ls_user ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.

  METHOD get_repo_show.

    rv_key = read( )-repo_show.

  ENDMETHOD.

  METHOD update.

    DATA: lv_xml TYPE string.

    lv_xml = to_xml( is_user ).

    zcl_abapgit_persistence_db=>get_instance( )->modify(
      iv_type  = zcl_abapgit_persistence_db=>c_type_user
      iv_value = mv_user
      iv_data  = lv_xml ).

  ENDMETHOD.

  METHOD set_default_git_user_name.

    DATA: ls_user TYPE ty_user.


    ls_user = read( ).

    ls_user-default_git_user-name = iv_username.

    update( ls_user ).

  ENDMETHOD.

  METHOD get_default_git_user_name.

    rv_username = read( )-default_git_user-name.

  ENDMETHOD.

  METHOD set_default_git_user_email.

    DATA: ls_user TYPE ty_user.


    ls_user = read( ).
    ls_user-default_git_user-email = iv_email.
    update( ls_user ).

  ENDMETHOD.

  METHOD get_default_git_user_email.

    rv_email = read( )-default_git_user-email.

  ENDMETHOD.

  METHOD read_repo_config.
    DATA: lt_repo_config TYPE ty_repo_config_tt,
          lv_key         TYPE string.

    lv_key         = to_lower( iv_url ).
    lt_repo_config = read( )-repo_config.
    READ TABLE lt_repo_config INTO rs_repo_config WITH KEY url = lv_key.

  ENDMETHOD.  "read_repo_config

  METHOD update_repo_config.
    DATA: ls_user TYPE ty_user,
          lv_key  TYPE string.
    FIELD-SYMBOLS <repo_config> TYPE ty_repo_config.

    ls_user = read( ).
    lv_key  = to_lower( iv_url ).

    READ TABLE ls_user-repo_config ASSIGNING <repo_config> WITH KEY url = lv_key.
    IF sy-subrc IS NOT INITIAL.
      APPEND INITIAL LINE TO ls_user-repo_config ASSIGNING <repo_config>.
    ENDIF.
    <repo_config>     = is_repo_config.
    <repo_config>-url = lv_key.

    update( ls_user ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.  "update_repo_config

  METHOD set_repo_git_user_name.

    DATA: ls_repo_config TYPE ty_repo_config.

    ls_repo_config               = read_repo_config( iv_url ).
    ls_repo_config-git_user-name = iv_username.
    update_repo_config( iv_url = iv_url is_repo_config = ls_repo_config ).

  ENDMETHOD.  "set_repo_username

  METHOD get_repo_git_user_name.

    rv_username = read_repo_config( iv_url )-git_user-name.

  ENDMETHOD.  "get_repo_username

  METHOD set_repo_login.

    DATA: ls_repo_config TYPE ty_repo_config.

    ls_repo_config       = read_repo_config( iv_url ).
    ls_repo_config-login = iv_login.
    update_repo_config( iv_url = iv_url is_repo_config = ls_repo_config ).

  ENDMETHOD.  "set_repo_login

  METHOD get_repo_login.

    rv_login = read_repo_config( iv_url )-login.

  ENDMETHOD.  "get_repo_login

  METHOD set_repo_git_user_email.

    DATA: ls_repo_config TYPE ty_repo_config.

    ls_repo_config                = read_repo_config( iv_url ).
    ls_repo_config-git_user-email = iv_email.
    update_repo_config( iv_url = iv_url is_repo_config = ls_repo_config ).

  ENDMETHOD.  "set_repo_email

  METHOD get_repo_git_user_email.

    rv_email = read_repo_config( iv_url )-git_user-email.

  ENDMETHOD.  "get_repo_email

  METHOD set_repo_last_change_seen.

    DATA: ls_repo_config TYPE ty_repo_config.

    ls_repo_config                  = read_repo_config( iv_url ).
    ls_repo_config-last_change_seen = iv_version.
    update_repo_config( iv_url = iv_url is_repo_config = ls_repo_config ).

  ENDMETHOD.  "set_last_change_seen

  METHOD get_repo_last_change_seen.

    rv_version = read_repo_config( iv_url )-last_change_seen.

  ENDMETHOD.  "get_last_change_seen

  METHOD toggle_hide_files.

    DATA ls_user TYPE ty_user.

    ls_user = read( ).
    ls_user-hide_files = boolc( ls_user-hide_files = abap_false ).
    update( ls_user ).

    rv_hide = ls_user-hide_files.

  ENDMETHOD. "toggle_hide_files

  METHOD get_hide_files.

    rv_hide = read( )-hide_files.

  ENDMETHOD. "get_hide_files

  METHOD toggle_changes_only.

    DATA ls_user TYPE ty_user.

    ls_user = read( ).
    ls_user-changes_only = boolc( ls_user-changes_only = abap_false ).
    update( ls_user ).

    rv_changes_only = ls_user-changes_only.

  ENDMETHOD. "toggle_changes_only

  METHOD get_changes_only.

    rv_changes_only = read( )-changes_only.

  ENDMETHOD. "get_changes_only

  METHOD toggle_diff_unified.

    DATA ls_user TYPE ty_user.

    ls_user = read( ).
    ls_user-diff_unified = boolc( ls_user-diff_unified = abap_false ).
    update( ls_user ).

    rv_diff_unified = ls_user-diff_unified.

  ENDMETHOD. "toggle_diff_unified

  METHOD get_diff_unified.

    rv_diff_unified = read( )-diff_unified.

  ENDMETHOD. "get_diff_unified

  METHOD get_favorites.

    rt_favorites = read( )-favorites.

  ENDMETHOD.  "get_favorites

  METHOD toggle_favorite.

    DATA: ls_user TYPE ty_user.

    ls_user = read( ).

    READ TABLE ls_user-favorites TRANSPORTING NO FIELDS
      WITH KEY table_line = iv_repo_key.

    IF sy-subrc = 0.
      DELETE ls_user-favorites INDEX sy-tabix.
    ELSE.
      APPEND iv_repo_key TO ls_user-favorites.
    ENDIF.

    update( ls_user ).

  ENDMETHOD.  " toggle_favorite.

  METHOD is_favorite_repo.

    DATA: lt_favorites TYPE tt_favorites.

    lt_favorites = get_favorites( ).

    READ TABLE lt_favorites TRANSPORTING NO FIELDS
      WITH KEY table_line = iv_repo_key.

    rv_yes = boolc( sy-subrc = 0 ).

  ENDMETHOD.  " is_favorite_repo.

ENDCLASS.


CLASS lcl_persist_migrate IMPLEMENTATION.

  METHOD run.

    IF table_exists( ) = abap_false.
      table_create( ).
    ENDIF.

    IF lock_exists( ) = abap_false.
      lock_create( ).
    ENDIF.

    IF settings_exists( ) = abap_false.
      migrate_settings( ).
    ENDIF.

  ENDMETHOD.

  METHOD settings_exists.

    TRY.
        zcl_abapgit_persistence_db=>get_instance( )->read(
          iv_type  = 'SETTINGS'
          iv_value = '' ).
        rv_exists = abap_true.
      CATCH zcx_abapgit_not_found.
        rv_exists = abap_false.
    ENDTRY.
  ENDMETHOD.

  METHOD migrate_settings.

    DATA: lr_settings                    TYPE REF TO zcl_abapgit_settings.
    DATA: lr_persist_settings            TYPE REF TO lcl_persist_settings.
    DATA: lv_critical_tests_as_string    TYPE string.
    DATA: lv_critical_tests_as_boolean   TYPE abap_bool.
    DATA: lv_max_lines_as_string         TYPE string.
    DATA: lv_flag                        TYPE abap_bool.
    DATA: lv_max_lines_as_integer        TYPE i.
    DATA: lv_s_param_value               TYPE string.
    DATA: lv_i_param_value               TYPE i.
    DATA: lv_adt_jump_enabled_as_string  TYPE string.
    DATA: lv_adt_jump_enabled_as_boolean TYPE abap_bool.


    CREATE OBJECT lr_persist_settings.
    CREATE OBJECT lr_settings.
    lr_settings->set_defaults( ).

    TRY.
        lr_settings->set_proxy_url(
          zcl_abapgit_persistence_db=>get_instance( )->read(
            iv_type  = 'SETTINGS'
            iv_value = 'PROXY_URL' ) ).
      CATCH zcx_abapgit_not_found.
    ENDTRY.

    TRY.
        lr_settings->set_proxy_port(
          zcl_abapgit_persistence_db=>get_instance( )->read(
            iv_type  = 'SETTINGS'
            iv_value = 'PROXY_PORT' ) ).
      CATCH zcx_abapgit_not_found.
    ENDTRY.

    TRY.
        lv_flag = zcl_abapgit_persistence_db=>get_instance( )->read(
          iv_type  = 'SETTINGS'
          iv_value = 'PROXY_AUTH' ).
        lr_settings->set_proxy_authentication( lv_flag ).
      CATCH zcx_abapgit_not_found.
    ENDTRY.

    TRY.
        lv_critical_tests_as_string = zcl_abapgit_persistence_db=>get_instance( )->read(
           iv_type  = 'SETTINGS'
           iv_value = 'CRIT_TESTS' ).
        lv_critical_tests_as_boolean = lv_critical_tests_as_string.
        lr_settings->set_run_critical_tests( lv_critical_tests_as_boolean ).
      CATCH zcx_abapgit_not_found.
    ENDTRY.

    TRY.
        lv_max_lines_as_string = zcl_abapgit_persistence_db=>get_instance( )->read(
           iv_type  = 'SETTINGS'
           iv_value = 'MAX_LINES' ).
        lv_max_lines_as_integer = lv_max_lines_as_string.
        lr_settings->set_max_lines( lv_max_lines_as_integer ).
      CATCH zcx_abapgit_not_found cx_sy_conversion_no_number.
    ENDTRY.

    TRY.
        lv_adt_jump_enabled_as_string = zcl_abapgit_persistence_db=>get_instance( )->read(
           iv_type  = 'SETTINGS'
           iv_value = 'ADT_JUMP' ).
        lv_adt_jump_enabled_as_boolean = lv_adt_jump_enabled_as_string.
        lr_settings->set_adt_jump_enanbled( lv_adt_jump_enabled_as_boolean ).
      CATCH zcx_abapgit_not_found.
    ENDTRY.

    TRY.
        lv_s_param_value = zcl_abapgit_persistence_db=>get_instance( )->read(
           iv_type  = 'SETTINGS'
           iv_value = 'COMMENT_LEN' ).
        lv_i_param_value = lv_s_param_value.
        lr_settings->set_commitmsg_comment_length( lv_i_param_value ).
      CATCH zcx_abapgit_not_found cx_sy_conversion_no_number.
    ENDTRY.

    TRY.
        lv_s_param_value = zcl_abapgit_persistence_db=>get_instance( )->read(
           iv_type  = 'SETTINGS'
           iv_value = 'BODY_SIZE' ).
        lv_i_param_value = lv_s_param_value.
        lr_settings->set_commitmsg_body_size( lv_i_param_value ).
      CATCH zcx_abapgit_not_found cx_sy_conversion_no_number.
    ENDTRY.

    lr_persist_settings->modify( io_settings = lr_settings ).

    TRY.
        zcl_abapgit_persistence_db=>get_instance( )->delete(
          iv_type  = 'SETTINGS'
          iv_value = 'PROXY_URL' ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        zcl_abapgit_persistence_db=>get_instance( )->delete(
          iv_type  = 'SETTINGS'
          iv_value = 'PROXY_PORT' ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        zcl_abapgit_persistence_db=>get_instance( )->delete(
          iv_type  = 'SETTINGS'
          iv_value = 'PROXY_AUTH' ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        zcl_abapgit_persistence_db=>get_instance( )->delete(
           iv_type  = 'SETTINGS'
           iv_value = 'CRIT_TESTS' ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        zcl_abapgit_persistence_db=>get_instance( )->delete(
           iv_type  = 'SETTINGS'
           iv_value = 'MAX_LINES' ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        zcl_abapgit_persistence_db=>get_instance( )->delete(
           iv_type  = 'SETTINGS'
           iv_value = 'ADT_JUMP' ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        zcl_abapgit_persistence_db=>get_instance( )->delete(
           iv_type  = 'SETTINGS'
           iv_value = 'COMMENT_LEN' ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        zcl_abapgit_persistence_db=>get_instance( )->delete(
           iv_type  = 'SETTINGS'
           iv_value = 'BODY_SIZE' ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.

  METHOD lock_exists.

    DATA: lv_viewname TYPE dd25l-viewname.


    SELECT SINGLE viewname FROM dd25l INTO lv_viewname
      WHERE viewname = zcl_abapgit_persistence_db=>c_lock.
    rv_exists = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lock_create.

    DATA: lv_obj_name TYPE tadir-obj_name,
          ls_dd25v    TYPE dd25v,
          lt_dd26e    TYPE STANDARD TABLE OF dd26e WITH DEFAULT KEY,
          lt_dd27p    TYPE STANDARD TABLE OF dd27p WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_dd26e> LIKE LINE OF lt_dd26e,
                   <ls_dd27p> LIKE LINE OF lt_dd27p.


    ls_dd25v-viewname   = zcl_abapgit_persistence_db=>c_lock.
    ls_dd25v-aggtype    = 'E'.
    ls_dd25v-roottab    = zcl_abapgit_persistence_db=>c_tabname.
    ls_dd25v-ddlanguage = zif_abapgit_definitions=>gc_english.
    ls_dd25v-ddtext     = c_text.

    APPEND INITIAL LINE TO lt_dd26e ASSIGNING <ls_dd26e>.
    <ls_dd26e>-viewname   = zcl_abapgit_persistence_db=>c_lock.
    <ls_dd26e>-tabname    = zcl_abapgit_persistence_db=>c_tabname.
    <ls_dd26e>-tabpos     = '0001'.
    <ls_dd26e>-fortabname = zcl_abapgit_persistence_db=>c_tabname.
    <ls_dd26e>-enqmode    = 'E'.

    APPEND INITIAL LINE TO lt_dd27p ASSIGNING <ls_dd27p>.
    <ls_dd27p>-viewname  = zcl_abapgit_persistence_db=>c_lock.
    <ls_dd27p>-objpos    = '0001'.
    <ls_dd27p>-viewfield = 'TYPE'.
    <ls_dd27p>-tabname   = zcl_abapgit_persistence_db=>c_tabname.
    <ls_dd27p>-fieldname = 'TYPE'.
    <ls_dd27p>-keyflag   = abap_true.

    APPEND INITIAL LINE TO lt_dd27p ASSIGNING <ls_dd27p>.
    <ls_dd27p>-viewname  = zcl_abapgit_persistence_db=>c_lock.
    <ls_dd27p>-objpos    = '0002'.
    <ls_dd27p>-viewfield = 'VALUE'.
    <ls_dd27p>-tabname   = zcl_abapgit_persistence_db=>c_tabname.
    <ls_dd27p>-fieldname = 'VALUE'.
    <ls_dd27p>-keyflag   = abap_true.

    CALL FUNCTION 'DDIF_ENQU_PUT'
      EXPORTING
        name              = zcl_abapgit_persistence_db=>c_lock
        dd25v_wa          = ls_dd25v
      TABLES
        dd26e_tab         = lt_dd26e
        dd27p_tab         = lt_dd27p
      EXCEPTIONS
        enqu_not_found    = 1
        name_inconsistent = 2
        enqu_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'migrate, error from DDIF_ENQU_PUT' ).
    ENDIF.

    lv_obj_name = zcl_abapgit_persistence_db=>c_lock.
    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'
        wi_tadir_object   = 'ENQU'
        wi_tadir_obj_name = lv_obj_name
        wi_set_genflag    = abap_true
        wi_test_modus     = abap_false
        wi_tadir_devclass = '$TMP'
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'migrate, error from TR_TADIR_INTERFACE' ).
    ENDIF.

    CALL FUNCTION 'DDIF_ENQU_ACTIVATE'
      EXPORTING
        name        = zcl_abapgit_persistence_db=>c_lock
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'migrate, error from DDIF_ENQU_ACTIVATE' ).
    ENDIF.

  ENDMETHOD.

  METHOD table_exists.

    DATA: lv_tabname TYPE dd02l-tabname.

    SELECT SINGLE tabname FROM dd02l INTO lv_tabname
      WHERE tabname = zcl_abapgit_persistence_db=>c_tabname.
    rv_exists = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD table_create.

    DATA: lv_rc       LIKE sy-subrc,
          lv_obj_name TYPE tadir-obj_name,
          ls_dd02v    TYPE dd02v,
          ls_dd09l    TYPE dd09l,
          lt_dd03p    TYPE STANDARD TABLE OF dd03p WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_dd03p> LIKE LINE OF lt_dd03p.

    ls_dd02v-tabname    = zcl_abapgit_persistence_db=>c_tabname.
    ls_dd02v-ddlanguage = zif_abapgit_definitions=>gc_english.
    ls_dd02v-tabclass   = 'TRANSP'.
    ls_dd02v-ddtext     = c_text.
    ls_dd02v-contflag   = 'A'.
    ls_dd02v-exclass    = '1'.

    ls_dd09l-tabname  = zcl_abapgit_persistence_db=>c_tabname.
    ls_dd09l-as4local = 'A'.
    ls_dd09l-tabkat   = '1'.
    ls_dd09l-tabart   = 'APPL1'.
    ls_dd09l-bufallow = 'N'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = zcl_abapgit_persistence_db=>c_tabname.
    <ls_dd03p>-fieldname = 'TYPE'.
    <ls_dd03p>-position  = '0001'.
    <ls_dd03p>-keyflag   = 'X'.
    <ls_dd03p>-datatype  = 'CHAR'.
    <ls_dd03p>-leng      = '000012'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = zcl_abapgit_persistence_db=>c_tabname.
    <ls_dd03p>-fieldname = 'VALUE'.
    <ls_dd03p>-position  = '0002'.
    <ls_dd03p>-keyflag   = 'X'.
    <ls_dd03p>-datatype  = 'CHAR'.
    <ls_dd03p>-leng      = '000012'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = zcl_abapgit_persistence_db=>c_tabname.
    <ls_dd03p>-fieldname = 'DATA_STR'.
    <ls_dd03p>-position  = '0003'.
    <ls_dd03p>-datatype  = 'STRG'.

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name              = zcl_abapgit_persistence_db=>c_tabname
        dd02v_wa          = ls_dd02v
        dd09l_wa          = ls_dd09l
      TABLES
        dd03p_tab         = lt_dd03p
      EXCEPTIONS
        tabl_not_found    = 1
        name_inconsistent = 2
        tabl_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'migrate, error from DDIF_TABL_PUT' ).
    ENDIF.

    lv_obj_name = zcl_abapgit_persistence_db=>c_tabname.
    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'
        wi_tadir_object   = 'TABL'
        wi_tadir_obj_name = lv_obj_name
        wi_set_genflag    = abap_true
        wi_test_modus     = abap_false
        wi_tadir_devclass = '$TMP'
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'migrate, error from TR_TADIR_INTERFACE' ).
    ENDIF.

    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
      EXPORTING
        name        = zcl_abapgit_persistence_db=>c_tabname
        auth_chk    = abap_false
      IMPORTING
        rc          = lv_rc
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0 OR lv_rc <> 0.
      zcx_abapgit_exception=>raise( 'migrate, error from DDIF_TABL_ACTIVATE' ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_persist_settings IMPLEMENTATION.

  METHOD modify.

    DATA: settings TYPE string.
    settings = io_settings->get_settings_xml( ).

    zcl_abapgit_persistence_db=>get_instance( )->modify(
      iv_type       = zcl_abapgit_persistence_db=>c_type_settings
      iv_value      = ''
      iv_data       = settings ).

    " Settings have been modified: Update Buffered Settings
    IF mo_settings IS BOUND.
      mo_settings->set_xml_settings( settings ).
    ENDIF.

  ENDMETHOD.

  METHOD read.

    IF mo_settings IS BOUND.
      " Return Buffered Settings
      ro_settings = mo_settings.
      RETURN.
    ENDIF.

    " Settings have changed or have not yet been loaded
    CREATE OBJECT ro_settings.

    TRY.

        ro_settings->set_xml_settings(
          zcl_abapgit_persistence_db=>get_instance( )->read(
            iv_type  = zcl_abapgit_persistence_db=>c_type_settings
            iv_value = '' ) ).

      CATCH zcx_abapgit_not_found zcx_abapgit_exception.

        ro_settings->set_defaults( ).

    ENDTRY.

    mo_settings = ro_settings.

  ENDMETHOD.

ENDCLASS.
