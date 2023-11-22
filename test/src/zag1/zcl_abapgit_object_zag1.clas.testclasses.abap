CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL CRITICAL FINAL.

  PRIVATE SECTION.
    METHODS upsert FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD upsert.

    DATA li_repo_srv TYPE REF TO zif_abapgit_repo_srv.
    DATA lo_online   TYPE REF TO zcl_abapgit_repo_online.
    DATA lv_url      TYPE string.


    ASSERT sy-sysid = 'ABC'.

    lv_url = zcl_abapgit_gitea=>create_repo( 'repo-' && cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ) ).

    zcl_abapgit_object_zag1=>upsert(
      iv_name    = 'ZFOOBAR'
      iv_value   = 'hello'
      iv_package = 'ZFOOBAR' ).

    li_repo_srv = zcl_abapgit_repo_srv=>get_instance( ).

    lo_online ?= li_repo_srv->new_online(
      iv_url     = lv_url
      iv_package = 'ZFOOBAR' ).

    cl_abap_unit_assert=>assert_not_initial( lo_online ).

    lo_online->get_files_local( ).

  ENDMETHOD.

ENDCLASS.
