CLASS zcl_abapgit_persistence_user DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_persist_user .

    TYPES tt_favorites TYPE zif_abapgit_persistence=>tt_repo_keys .

    CLASS-METHODS get_instance
      IMPORTING
        !iv_user       TYPE xubname DEFAULT sy-uname
      RETURNING
        VALUE(ri_user) TYPE REF TO zif_abapgit_persist_user .
    METHODS constructor
      IMPORTING
        !iv_user TYPE xubname DEFAULT sy-uname .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_repo_config,
        url              TYPE zif_abapgit_persistence=>ty_repo-url,
        login            TYPE string,
        git_user         TYPE zif_abapgit_definitions=>ty_git_user,
        last_change_seen TYPE string,
      END OF ty_repo_config .
    TYPES:
      ty_repo_config_tt TYPE STANDARD TABLE OF ty_repo_config WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_user,
        default_git_user TYPE zif_abapgit_definitions=>ty_git_user,
        repo_show        TYPE zif_abapgit_persistence=>ty_repo-key,
        hide_files       TYPE abap_bool,
        changes_only     TYPE abap_bool,
        show_order_by    TYPE abap_bool,
        diff_unified     TYPE abap_bool,
        favorites        TYPE tt_favorites,
        repo_config      TYPE ty_repo_config_tt,
        settings         TYPE zif_abapgit_definitions=>ty_s_user_settings,
      END OF ty_user .

    DATA mv_user TYPE xubname .
    CLASS-DATA gi_current_user TYPE REF TO zif_abapgit_persist_user .

    METHODS from_xml
      IMPORTING
        !iv_xml        TYPE string
      RETURNING
        VALUE(rs_user) TYPE ty_user
      RAISING
        zcx_abapgit_exception .
    METHODS read
      RETURNING
        VALUE(rs_user) TYPE ty_user
      RAISING
        zcx_abapgit_exception .
    METHODS read_repo_config
      IMPORTING
        !iv_url               TYPE zif_abapgit_persistence=>ty_repo-url
      RETURNING
        VALUE(rs_repo_config) TYPE ty_repo_config
      RAISING
        zcx_abapgit_exception .
    METHODS to_xml
      IMPORTING
        !is_user      TYPE ty_user
      RETURNING
        VALUE(rv_xml) TYPE string .
    METHODS update
      IMPORTING
        !is_user TYPE ty_user
      RAISING
        zcx_abapgit_exception .
    METHODS update_repo_config
      IMPORTING
        !iv_url         TYPE zif_abapgit_persistence=>ty_repo-url
        !is_repo_config TYPE ty_repo_config
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_PERSISTENCE_USER IMPLEMENTATION.


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


  METHOD get_instance.

    IF iv_user = sy-uname ##USER_OK.
      IF gi_current_user IS NOT BOUND.
        CREATE OBJECT gi_current_user TYPE zcl_abapgit_persistence_user.
      ENDIF.
      ri_user = gi_current_user.
    ELSE.
      CREATE OBJECT ri_user TYPE zcl_abapgit_persistence_user
        EXPORTING
          iv_user = iv_user.
    ENDIF.

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


  METHOD read_repo_config.
    DATA: lt_repo_config TYPE ty_repo_config_tt,
          lv_key         TYPE string.

    lv_key         = to_lower( iv_url ).
    lt_repo_config = read( )-repo_config.
    READ TABLE lt_repo_config INTO rs_repo_config WITH KEY url = lv_key.

  ENDMETHOD.


  METHOD to_xml.
    CALL TRANSFORMATION id
      SOURCE user = is_user
      RESULT XML rv_xml.
  ENDMETHOD.


  METHOD update.

    DATA: lv_xml TYPE string.

    lv_xml = to_xml( is_user ).

    zcl_abapgit_persistence_db=>get_instance( )->modify(
      iv_type  = zcl_abapgit_persistence_db=>c_type_user
      iv_value = mv_user
      iv_data  = lv_xml ).

  ENDMETHOD.


  METHOD update_repo_config.

    DATA: ls_user TYPE ty_user,
          lv_key  TYPE string.

    FIELD-SYMBOLS <ls_repo_config> TYPE ty_repo_config.


    ls_user = read( ).
    lv_key  = to_lower( iv_url ).

    READ TABLE ls_user-repo_config ASSIGNING <ls_repo_config> WITH KEY url = lv_key.
    IF sy-subrc IS NOT INITIAL.
      APPEND INITIAL LINE TO ls_user-repo_config ASSIGNING <ls_repo_config>.
    ENDIF.
    <ls_repo_config>     = is_repo_config.
    <ls_repo_config>-url = lv_key.

    update( ls_user ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_changes_only.

    rv_changes_only = read( )-changes_only.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_default_git_user_email.

    rv_email = read( )-default_git_user-email.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_default_git_user_name.

    rv_username = read( )-default_git_user-name.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_diff_unified.

    rv_diff_unified = read( )-diff_unified.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_favorites.

    rt_favorites = read( )-favorites.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_hide_files.

    rv_hide = read( )-hide_files.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_repo_git_user_email.

    rv_email = read_repo_config( iv_url )-git_user-email.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_repo_git_user_name.

    rv_username = read_repo_config( iv_url )-git_user-name.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_repo_last_change_seen.

    rv_version = read_repo_config( iv_url )-last_change_seen.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_repo_login.

    rv_login = read_repo_config( iv_url )-login.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_repo_show.

    DATA lo_repo TYPE REF TO zcl_abapgit_repo.

    rv_key = read( )-repo_show.

    " Check if repo exists
    TRY.
        lo_repo = zcl_abapgit_repo_srv=>get_instance( )->get( rv_key ).
      CATCH zcx_abapgit_exception.
        " remove invalid key
        CLEAR rv_key.
        zif_abapgit_persist_user~set_repo_show( rv_key ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_settings.

    DATA: ls_user TYPE ty_user.

    ls_user = read( ).

    rs_user_settings = ls_user-settings.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~is_favorite_repo.

    DATA: lt_favorites TYPE tt_favorites.

    lt_favorites = zif_abapgit_persist_user~get_favorites( ).

    READ TABLE lt_favorites TRANSPORTING NO FIELDS
      WITH KEY table_line = iv_repo_key.

    rv_yes = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~set_default_git_user_email.

    DATA: ls_user TYPE ty_user.


    ls_user = read( ).
    ls_user-default_git_user-email = iv_email.
    update( ls_user ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~set_default_git_user_name.

    DATA: ls_user TYPE ty_user.


    ls_user = read( ).

    ls_user-default_git_user-name = iv_username.

    update( ls_user ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~set_repo_git_user_email.

    DATA: ls_repo_config TYPE ty_repo_config.

    ls_repo_config                = read_repo_config( iv_url ).
    ls_repo_config-git_user-email = iv_email.
    update_repo_config( iv_url = iv_url
                        is_repo_config = ls_repo_config ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~set_repo_git_user_name.

    DATA: ls_repo_config TYPE ty_repo_config.

    ls_repo_config               = read_repo_config( iv_url ).
    ls_repo_config-git_user-name = iv_username.
    update_repo_config( iv_url = iv_url
                        is_repo_config = ls_repo_config ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~set_repo_last_change_seen.

    DATA: ls_repo_config TYPE ty_repo_config.

    ls_repo_config                  = read_repo_config( iv_url ).
    ls_repo_config-last_change_seen = iv_version.
    update_repo_config( iv_url = iv_url
                        is_repo_config = ls_repo_config ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~set_repo_login.

    DATA: ls_repo_config TYPE ty_repo_config.

    ls_repo_config       = read_repo_config( iv_url ).
    ls_repo_config-login = iv_login.
    update_repo_config( iv_url = iv_url
                        is_repo_config = ls_repo_config ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~set_repo_show.

    DATA: ls_user TYPE ty_user.


    ls_user = read( ).
    ls_user-repo_show = iv_key.
    update( ls_user ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~set_settings.

    DATA: ls_user TYPE ty_user.

    ls_user = read( ).
    ls_user-settings = is_user_settings.
    update( ls_user ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~toggle_changes_only.

    DATA ls_user TYPE ty_user.

    ls_user = read( ).
    ls_user-changes_only = boolc( ls_user-changes_only = abap_false ).
    update( ls_user ).

    rv_changes_only = ls_user-changes_only.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~toggle_diff_unified.

    DATA ls_user TYPE ty_user.

    ls_user = read( ).
    ls_user-diff_unified = boolc( ls_user-diff_unified = abap_false ).
    update( ls_user ).

    rv_diff_unified = ls_user-diff_unified.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~toggle_favorite.

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

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~toggle_hide_files.

    DATA ls_user TYPE ty_user.

    ls_user = read( ).
    ls_user-hide_files = boolc( ls_user-hide_files = abap_false ).
    update( ls_user ).

    rv_hide = ls_user-hide_files.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_show_order_by.

    rv_show_order_by = read( )-show_order_by.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~toggle_show_order_by.

    DATA ls_user TYPE ty_user.

    ls_user = read( ).
    ls_user-show_order_by = boolc( ls_user-show_order_by = abap_false ).
    update( ls_user ).

    rv_show_order_by = ls_user-show_order_by.

  ENDMETHOD.

ENDCLASS.
