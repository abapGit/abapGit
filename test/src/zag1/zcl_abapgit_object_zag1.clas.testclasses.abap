CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL CRITICAL FINAL.

  PUBLIC SECTION.
    INTERFACES if_ftd_invocation_answer.

  PRIVATE SECTION.
    METHODS setup.
    METHODS teardown.
    METHODS upsert FOR TESTING RAISING cx_static_check.

    DATA mi_env TYPE REF TO if_function_test_environment.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    DATA lt_deps    TYPE if_function_test_environment=>tt_function_dependencies.
    DATA lo_initial TYPE REF TO zif_abapgit_repo_srv.

    zcl_abapgit_repo_srv=>inject_instance( lo_initial ).

    INSERT 'ENQUEUE_EZABAPGIT' INTO TABLE lt_deps.
    INSERT 'DEQUEUE_EZABAPGIT' INTO TABLE lt_deps.
    mi_env = cl_function_test_environment=>create( lt_deps ).
    mi_env->get_double( 'ENQUEUE_EZABAPGIT' )->configure_call( )->ignore_all_parameters( )->then_answer( me ).
    mi_env->get_double( 'DEQUEUE_EZABAPGIT' )->configure_call( )->ignore_all_parameters( )->then_answer( me ).
  ENDMETHOD.

  METHOD teardown.
    mi_env->clear_doubles( ).
  ENDMETHOD.

  METHOD if_ftd_invocation_answer~answer.
    RETURN.
  ENDMETHOD.

  METHOD upsert.

    DATA lo_online TYPE REF TO zcl_abapgit_repo_online.
    DATA lv_url    TYPE string.


    ASSERT sy-sysid = 'ABC'.

    lv_url = zcl_abapgit_gitea=>create_repo( 'repo-' && cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ) ).

    zcl_abapgit_object_zag1=>upsert(
      iv_name    = 'ZFOOBAR'
      iv_value   = 'hello'
      iv_package = 'ZFOOBAR' ).

    lo_online ?= zcl_abapgit_repo_srv=>get_instance( )->new_online(
      iv_url     = lv_url
      iv_package = 'ZFOOBAR' ).

    cl_abap_unit_assert=>assert_not_initial( lo_online ).

    " todo, lo_online->get_files_local( ).

  ENDMETHOD.

ENDCLASS.
