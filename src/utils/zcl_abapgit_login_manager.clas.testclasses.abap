CLASS ltcl_login_manager DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    CONSTANTS: c_username TYPE string VALUE 'Aladdin',
               c_password TYPE string VALUE 'OpenSesame'.

    METHODS: setup,
      teardown,
      encoding FOR TESTING
        RAISING zcx_abapgit_exception,
      auto_login_background_mode FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_login_manager IMPLEMENTATION.

  METHOD setup.
    zcl_abapgit_login_manager=>clear( ).
  ENDMETHOD.

  METHOD teardown.
    zcl_abapgit_login_manager=>clear( ).
  ENDMETHOD.

  METHOD encoding.

    DATA: lv_auth TYPE string.

    lv_auth = zcl_abapgit_login_manager=>set(
      iv_uri      = 'https://github.com/abapGit/abapGit.git'
      iv_username = c_username
      iv_password = c_password ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_auth
      exp = 'Basic QWxhZGRpbjpPcGVuU2VzYW1l' ).

  ENDMETHOD.



  METHOD auto_login_background_mode.
    "here we try to simulate that we have different git projects on same server
    "and BOT users + access tokens per project (for example in gitlab) have been generated to be able to auto commit in abapGit background mode


    "GIVEN
    DATA: lv_auth_a TYPE string,
          lv_auth_b TYPE string.

    CONSTANTS: lc_project_a TYPE string VALUE 'https://local.gitlab.com/projectA.git',
               lc_project_b TYPE string VALUE 'https://local.gitlab.com/projectB.git'.

    CONSTANTS: c_username_a TYPE string VALUE 'project_a_bot',
               c_password_a TYPE string VALUE 'Op3cSesasdfPOpojasdf',
               c_username_b TYPE string VALUE 'project_b_bot',
               c_password_b TYPE string VALUE 'loinonwasdfPOpvunwef'.

    "WHEN


    zcl_abapgit_login_manager=>set(  iv_uri      = lc_project_a
                                     iv_username = c_username_a
                                     iv_password = c_password_a ).


    zcl_abapgit_login_manager=>set(  iv_uri      = lc_project_b
                                     iv_username = c_username_b
                                     iv_password = c_password_b ).

    lv_auth_a = zcl_abapgit_login_manager=>load( lc_project_a ).
    lv_auth_b = zcl_abapgit_login_manager=>load( lc_project_b ).

    "THEN
    cl_abap_unit_assert=>assert_differs(
      EXPORTING
        act              =  lv_auth_a
        exp              =  lv_auth_b
        msg              = 'Corresponding user/pass combination was not found for projectA or projectB'  ).


    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act              =  zcl_abapgit_login_manager=>get( lc_project_a )
        exp              =  lv_auth_a
        msg              = 'Corresponding user/pass combination was not found for projectA'  ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act              =  zcl_abapgit_login_manager=>get( lc_project_b )
        exp              =  lv_auth_b
        msg              = 'Corresponding user/pass combination was not found for projectB'  ).

  ENDMETHOD.

ENDCLASS.
