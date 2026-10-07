CLASS ltd_persist_repo DEFINITION FOR TESTING.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_persist_repo.

    DATA mt_repos TYPE zif_abapgit_persistence=>ty_repos.

ENDCLASS.

CLASS ltd_persist_repo IMPLEMENTATION.

  METHOD zif_abapgit_persist_repo~add.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_persist_repo~delete.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_persist_repo~exists.
    READ TABLE mt_repos TRANSPORTING NO FIELDS WITH KEY key = iv_key.
    rv_yes = boolc( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD zif_abapgit_persist_repo~list.
    rt_repos = mt_repos.
  ENDMETHOD.

  METHOD zif_abapgit_persist_repo~list_by_keys.
    rt_repos = mt_repos.
  ENDMETHOD.

  METHOD zif_abapgit_persist_repo~lock.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_persist_repo~read.
    READ TABLE mt_repos INTO rs_repo WITH KEY key = iv_key.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abapgit_not_found.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_persist_repo~update_metadata.
    RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_reload DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    CONSTANTS c_key TYPE zif_abapgit_persistence=>ty_value VALUE '000000000001'.

    DATA mo_persist TYPE REF TO ltd_persist_repo.
    DATA mi_srv TYPE REF TO zif_abapgit_repo_srv.

    METHODS setup.
    METHODS teardown.
    METHODS change_persisted_repo
      IMPORTING
        iv_branch_name TYPE string OPTIONAL
        iv_offline     TYPE abap_bool DEFAULT abap_false.

    METHODS branch_changed_elsewhere FOR TESTING RAISING zcx_abapgit_exception.
    METHODS unchanged_keeps_instance FOR TESTING RAISING zcx_abapgit_exception.
    METHODS switched_to_offline FOR TESTING RAISING zcx_abapgit_exception.
    METHODS offline_keeps_imported_files FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_reload IMPLEMENTATION.

  METHOD setup.

    DATA ls_repo TYPE zif_abapgit_persistence=>ty_repo.

    ls_repo-key         = c_key.
    ls_repo-url         = 'https://github.com/abapGit/abapGit.git'.
    ls_repo-branch_name = 'refs/heads/main'.
    ls_repo-package     = '$ABAPGIT_TEST'.

    CREATE OBJECT mo_persist.
    APPEND ls_repo TO mo_persist->mt_repos.
    zcl_abapgit_persist_injector=>set_repo( mo_persist ).

    " Fresh service instance, so that no repos are cached from other tests
    zcl_abapgit_repo_srv=>inject_instance( ).
    mi_srv = zcl_abapgit_repo_srv=>get_instance( ).

  ENDMETHOD.

  METHOD teardown.

    DATA li_initial TYPE REF TO zif_abapgit_persist_repo.

    zcl_abapgit_persist_injector=>set_repo( li_initial ).
    zcl_abapgit_repo_srv=>inject_instance( ).

  ENDMETHOD.

  METHOD change_persisted_repo.

    FIELD-SYMBOLS <ls_repo> LIKE LINE OF mo_persist->mt_repos.

    READ TABLE mo_persist->mt_repos ASSIGNING <ls_repo> INDEX 1.
    IF iv_branch_name IS NOT INITIAL.
      <ls_repo>-branch_name = iv_branch_name.
    ENDIF.
    <ls_repo>-offline = iv_offline.

  ENDMETHOD.

  METHOD branch_changed_elsewhere.

    DATA li_before TYPE REF TO zif_abapgit_repo.
    DATA li_cached TYPE REF TO zif_abapgit_repo.
    DATA li_after TYPE REF TO zif_abapgit_repo.
    DATA lv_same TYPE abap_bool.

    li_before = mi_srv->get( c_key ).

    change_persisted_repo( iv_branch_name = 'refs/heads/feature' ).

    " Without a reload the cached instance still has the old branch
    li_cached = mi_srv->get( c_key ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cached->ms_data-branch_name
      exp = 'refs/heads/main' ).

    li_after = mi_srv->reload( c_key ).

    cl_abap_unit_assert=>assert_equals(
      act = li_after->ms_data-branch_name
      exp = 'refs/heads/feature' ).
    lv_same = boolc( li_after = li_before ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_same
      exp = abap_false ).

    li_cached = mi_srv->get( c_key ).
    lv_same = boolc( li_cached = li_after ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_same
      exp = abap_true ).

  ENDMETHOD.

  METHOD unchanged_keeps_instance.

    DATA li_before TYPE REF TO zif_abapgit_repo.
    DATA li_after TYPE REF TO zif_abapgit_repo.
    DATA lv_same TYPE abap_bool.

    li_before = mi_srv->get( c_key ).
    li_after = mi_srv->reload( c_key ).

    lv_same = boolc( li_after = li_before ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_same
      exp = abap_true ).

  ENDMETHOD.

  METHOD switched_to_offline.

    DATA li_repo TYPE REF TO zif_abapgit_repo.

    li_repo = mi_srv->get( c_key ).
    cl_abap_unit_assert=>assert_equals(
      act = li_repo->is_offline( )
      exp = abap_false ).

    change_persisted_repo( iv_offline = abap_true ).

    li_repo = mi_srv->reload( c_key ).
    cl_abap_unit_assert=>assert_equals(
      act = li_repo->is_offline( )
      exp = abap_true ).

  ENDMETHOD.

  METHOD offline_keeps_imported_files.

    DATA li_before TYPE REF TO zif_abapgit_repo.
    DATA li_after TYPE REF TO zif_abapgit_repo.
    DATA lt_files TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA ls_file LIKE LINE OF lt_files.
    DATA lv_same TYPE abap_bool.

    change_persisted_repo( iv_offline = abap_true ).

    ls_file-path     = '/src/'.
    ls_file-filename = 'zcl_test.clas.abap'.
    APPEND ls_file TO lt_files.

    " Files imported from a ZIP exist only in memory
    li_before = mi_srv->get( c_key ).
    li_before->set_files_remote( lt_files ).

    change_persisted_repo(
      iv_branch_name = 'refs/heads/feature'
      iv_offline     = abap_true ).

    li_after = mi_srv->reload( c_key ).

    lv_same = boolc( li_after = li_before ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_same
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = li_after->get_files_remote( )
      exp = lt_files ).

  ENDMETHOD.

ENDCLASS.
