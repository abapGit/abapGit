CLASS ltcl_user DEFINITION
  FOR TESTING
  RISK LEVEL CRITICAL
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    CONSTANTS:
      c_abap_user TYPE sy-uname VALUE 'ABAPGIT_TEST',
      c_git_user  TYPE string VALUE 'abapgit_tester',
      c_repo_url  TYPE string VALUE 'https://github.com/abapGit/abapGit'.

    DATA:
      mi_user TYPE REF TO zif_abapgit_persist_user.

    METHODS:
      set_get_git_user   FOR TESTING RAISING zcx_abapgit_exception,
      set_get_repo_show  FOR TESTING RAISING zcx_abapgit_exception,
      set_get_settings   FOR TESTING RAISING zcx_abapgit_exception,
      set_get_repo_login FOR TESTING RAISING zcx_abapgit_exception,
      teardown RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_user IMPLEMENTATION.

  METHOD set_get_git_user.

    DATA: lv_user TYPE string.

    mi_user = zcl_abapgit_persistence_user=>get_instance( c_abap_user ).
    mi_user->set_default_git_user_name( c_git_user ).

    FREE mi_user.

    mi_user = zcl_abapgit_persistence_user=>get_instance( c_abap_user ).
    lv_user = mi_user->get_default_git_user_name( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_user
      exp = c_git_user ).

  ENDMETHOD.

  METHOD set_get_repo_show.

    DATA: lv_key      TYPE zif_abapgit_persistence=>ty_repo-key,
          lv_repo_key TYPE zif_abapgit_persistence=>ty_repo-key.

    SELECT MIN( value ) FROM (zcl_abapgit_persistence_db=>c_tabname) INTO lv_repo_key
      WHERE type = zcl_abapgit_persistence_db=>c_type_repo.
    IF sy-subrc <> 0.
      RETURN. " can't test
    ENDIF.

    mi_user = zcl_abapgit_persistence_user=>get_instance( c_abap_user ).
    mi_user->set_repo_show( lv_repo_key ).

    FREE mi_user.

    mi_user = zcl_abapgit_persistence_user=>get_instance( c_abap_user ).
    lv_key = mi_user->get_repo_show( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_key
      exp = lv_repo_key ).

  ENDMETHOD.

  METHOD set_get_repo_login.

    DATA: lv_login TYPE string.

    mi_user = zcl_abapgit_persistence_user=>get_instance( c_abap_user ).
    mi_user->set_repo_login( iv_url = c_repo_url
                             iv_login = c_git_user ).

    FREE mi_user.

    mi_user = zcl_abapgit_persistence_user=>get_instance( c_abap_user ).
    lv_login = mi_user->get_repo_login( c_repo_url ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_login
      exp = c_git_user ).

  ENDMETHOD.

  METHOD set_get_settings.

    DATA: ls_settings TYPE zif_abapgit_definitions=>ty_s_user_settings.

    ls_settings-show_default_repo = abap_true.

    mi_user = zcl_abapgit_persistence_user=>get_instance( c_abap_user ).
    mi_user->set_settings( ls_settings ).

    FREE mi_user.

    mi_user = zcl_abapgit_persistence_user=>get_instance( c_abap_user ).
    ls_settings = mi_user->get_settings( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_settings-show_default_repo
      exp = abap_true ).

  ENDMETHOD.

  METHOD teardown.
    " Delete test user settings
    zcl_abapgit_persistence_db=>get_instance( )->delete(
      iv_type  = zcl_abapgit_persistence_db=>c_type_user
      iv_value = c_abap_user ).
    CALL FUNCTION 'DB_COMMIT'.
  ENDMETHOD.
ENDCLASS.
