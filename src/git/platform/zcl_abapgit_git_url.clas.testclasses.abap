CLASS ltcl_repo_online DEFINITION DEFERRED.
CLASS zcl_abapgit_git_url DEFINITION LOCAL FRIENDS ltcl_repo_online.

CLASS ltcl_repo_online DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      test_repo_commit_show_urls FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_repo_online IMPLEMENTATION.

  METHOD test_repo_commit_show_urls.

    TYPES: BEGIN OF ty_show_url_test,
             repo_url TYPE string,
             show_url TYPE string,
           END OF  ty_show_url_test,
           ty_show_url_tests TYPE STANDARD TABLE OF ty_show_url_test WITH DEFAULT KEY.

    DATA: ls_provider_urls TYPE ty_show_url_test,
          lt_test_urls     TYPE ty_show_url_tests,
          lv_testhash      TYPE zif_abapgit_git_definitions=>ty_sha1 VALUE 'my-SHA1-hash',
          lo_cut           TYPE REF TO zcl_abapgit_git_url,
          lv_show_url      TYPE zif_abapgit_persistence=>ty_repo-url.

    FIELD-SYMBOLS <ls_provider_urls> TYPE ty_show_url_test.

    CREATE OBJECT lo_cut.

    ls_provider_urls-repo_url = |https://github.com/abapGit/abapGit.git|.
    ls_provider_urls-show_url = |https://github.com/abapGit/abapGit/commit/{ lv_testhash }|.
    APPEND ls_provider_urls TO lt_test_urls.
    ls_provider_urls-repo_url = |http://github.com/abapGit/abapGit.git|.
    ls_provider_urls-show_url = |http://github.com/abapGit/abapGit/commit/{ lv_testhash }|.
    APPEND ls_provider_urls TO lt_test_urls.
    ls_provider_urls-repo_url = |https://bitbucket.org/abapGit/abapGit.git|.
    ls_provider_urls-show_url = |https://bitbucket.org/abapGit/abapGit/commits/{ lv_testhash }|.
    APPEND ls_provider_urls TO lt_test_urls.
    ls_provider_urls-repo_url = |https://gitlab.com/abapGit/abapGit.git|.
    ls_provider_urls-show_url = |https://gitlab.com/abapGit/abapGit/-/commit/{ lv_testhash }|.
    APPEND ls_provider_urls TO lt_test_urls.

    LOOP AT lt_test_urls ASSIGNING <ls_provider_urls>.
      lv_show_url = lo_cut->get_default_commit_display_url(
        iv_repo_url = <ls_provider_urls>-repo_url
        iv_hash     = lv_testhash ).

      cl_abap_unit_assert=>assert_equals(
        exp = <ls_provider_urls>-show_url
        act = lv_show_url ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
