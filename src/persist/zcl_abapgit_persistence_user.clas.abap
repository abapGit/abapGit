CLASS zcl_abapgit_persistence_user DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_persist_user .

    CLASS-METHODS get_instance
      IMPORTING
        !iv_user       TYPE sy-uname DEFAULT sy-uname
      RETURNING
        VALUE(ri_user) TYPE REF TO zif_abapgit_persist_user
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      IMPORTING
        !iv_user TYPE sy-uname DEFAULT sy-uname
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_repo_config,
        url              TYPE zif_abapgit_persistence=>ty_repo-url,
        login            TYPE string,
        git_user         TYPE zif_abapgit_git_definitions=>ty_git_user,
        last_change_seen TYPE string,
      END OF ty_repo_config .
    TYPES:
      ty_repo_configs TYPE STANDARD TABLE OF ty_repo_config WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_user,
        default_git_user TYPE zif_abapgit_git_definitions=>ty_git_user,
        repo_show        TYPE zif_abapgit_persistence=>ty_repo-key,
        hide_files       TYPE abap_bool,
        changes_only     TYPE abap_bool,
        order_by         TYPE string,
        order_descending TYPE abap_bool,
        diff_first       TYPE abap_bool,
        diff_unified     TYPE abap_bool,
        favorites        TYPE zif_abapgit_persist_user=>ty_favorites,
        repo_config      TYPE ty_repo_configs,
        settings         TYPE zif_abapgit_definitions=>ty_s_user_settings,
        show_folders     TYPE abap_bool,
        list_settings    TYPE zif_abapgit_definitions=>ty_list_settings,
      END OF ty_user .

    DATA mv_user TYPE sy-uname .
    DATA ms_user TYPE ty_user.
    CLASS-DATA gi_current_user TYPE REF TO zif_abapgit_persist_user .

    METHODS from_xml
      IMPORTING
        !iv_xml        TYPE string
      RETURNING
        VALUE(rs_user) TYPE ty_user
      RAISING
        zcx_abapgit_exception .
    METHODS read
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
      RAISING
        zcx_abapgit_exception .
    METHODS update_repo_config
      IMPORTING
        !iv_url         TYPE zif_abapgit_persistence=>ty_repo-url
        !is_repo_config TYPE ty_repo_config
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_persistence_user IMPLEMENTATION.


  METHOD constructor.
    mv_user = iv_user.
    read( ).
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
      RESULT user = rs_user.
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

    ms_user = from_xml( lv_xml ).

  ENDMETHOD.


  METHOD read_repo_config.
    DATA lv_url TYPE string.
    lv_url = to_lower( iv_url ).
    READ TABLE ms_user-repo_config INTO rs_repo_config WITH KEY url = lv_url.
  ENDMETHOD.


  METHOD to_xml.
    CALL TRANSFORMATION id
      SOURCE user = is_user
      RESULT XML rv_xml.
  ENDMETHOD.


  METHOD update.

    DATA: lv_xml TYPE string.

    lv_xml = to_xml( ms_user ).

    zcl_abapgit_persistence_db=>get_instance( )->modify(
      iv_type  = zcl_abapgit_persistence_db=>c_type_user
      iv_value = mv_user
      iv_data  = lv_xml ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD update_repo_config.

    DATA: lv_key  TYPE string.

    FIELD-SYMBOLS <ls_repo_config> TYPE ty_repo_config.

    lv_key  = to_lower( iv_url ).

    READ TABLE ms_user-repo_config ASSIGNING <ls_repo_config> WITH KEY url = lv_key.
    IF sy-subrc IS NOT INITIAL.
      APPEND INITIAL LINE TO ms_user-repo_config ASSIGNING <ls_repo_config>.
    ENDIF.
    <ls_repo_config>     = is_repo_config.
    <ls_repo_config>-url = lv_key.

    update( ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_changes_only.

    rv_changes_only = ms_user-changes_only.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_default_git_user_email.

    rv_email = ms_user-default_git_user-email.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_default_git_user_name.

    rv_username = ms_user-default_git_user-name.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_diff_first.
    rv_diff_first = ms_user-diff_first.
  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_diff_unified.

    rv_diff_unified = ms_user-diff_unified.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_favorites.

    rt_favorites = ms_user-favorites.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_hide_files.

    rv_hide = ms_user-hide_files.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_list_settings.

    rs_list_settings = ms_user-list_settings.

    IF rs_list_settings IS INITIAL.
      " for performance reasons, set "only favorites" as a default
      IF zcl_abapgit_repo_srv=>get_instance( )->list_favorites( ) IS NOT INITIAL.
        rs_list_settings-only_favorites = abap_true.
      ENDIF.

      rs_list_settings-order_by = |NAME|.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_order_by.
    rv_order_by = ms_user-order_by.
  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_order_descending.
    rv_order_descending = ms_user-order_descending.
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

    rv_key = ms_user-repo_show.

    IF rv_key IS INITIAL.
      RETURN.
    ENDIF.

    " Check if repo exists
    TRY.
        zcl_abapgit_persistence_db=>get_instance( )->read(
          iv_type  = zcl_abapgit_persistence_db=>c_type_repo
          iv_value = rv_key ).
      CATCH zcx_abapgit_not_found.
        " remove invalid key
        CLEAR rv_key.
        zif_abapgit_persist_user~set_repo_show( rv_key ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_settings.

    rs_user_settings = ms_user-settings.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~get_show_folders.

    rv_folders = ms_user-show_folders.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~is_favorite_repo.

    READ TABLE ms_user-favorites TRANSPORTING NO FIELDS
      WITH KEY table_line = iv_repo_key.

    rv_yes = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~set_default_git_user_email.

    ms_user-default_git_user-email = iv_email.
    update( ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~set_default_git_user_name.

    ms_user-default_git_user-name = iv_username.
    update( ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~set_diff_first.
    ms_user-diff_first = iv_diff_first.
    update( ).
    rv_diff_first = ms_user-diff_first.
  ENDMETHOD.


  METHOD zif_abapgit_persist_user~set_list_settings.
    ms_user-list_settings = is_list_settings.
    update( ).
  ENDMETHOD.


  METHOD zif_abapgit_persist_user~set_order_by.
    ms_user-order_by = iv_order_by.
    update( ).
    rv_order_by = ms_user-order_by.
  ENDMETHOD.


  METHOD zif_abapgit_persist_user~set_order_descending.
    ms_user-order_descending = iv_order_descending.
    update( ).
    rv_order_descending = ms_user-order_descending.
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

    ms_user-repo_show = iv_key.
    update( ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~set_settings.

    ms_user-settings = is_user_settings.
    update( ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~toggle_changes_only.

    ms_user-changes_only = boolc( ms_user-changes_only = abap_false ).
    update( ).

    rv_changes_only = ms_user-changes_only.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~toggle_diff_unified.

    ms_user-diff_unified = boolc( ms_user-diff_unified = abap_false ).
    update( ).

    rv_diff_unified = ms_user-diff_unified.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~toggle_favorite.

    READ TABLE ms_user-favorites TRANSPORTING NO FIELDS
      WITH KEY table_line = iv_repo_key.

    IF sy-subrc = 0.
      DELETE ms_user-favorites INDEX sy-tabix.
    ELSE.
      APPEND iv_repo_key TO ms_user-favorites.
    ENDIF.

    update( ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~toggle_hide_files.

    ms_user-hide_files = boolc( ms_user-hide_files = abap_false ).
    update( ).

    rv_hide = ms_user-hide_files.

  ENDMETHOD.


  METHOD zif_abapgit_persist_user~toggle_show_folders.
    ms_user-show_folders = boolc( ms_user-show_folders = abap_false ).
    update( ).

    rv_folders = ms_user-show_folders.
  ENDMETHOD.
ENDCLASS.
