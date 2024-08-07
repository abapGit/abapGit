CLASS lcl_tr_object_table DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_ftd_invocation_answer.
ENDCLASS.

CLASS lcl_tr_object_table IMPLEMENTATION.
  METHOD if_ftd_invocation_answer~answer.

    DATA lt_objects TYPE STANDARD TABLE OF ko100 WITH DEFAULT KEY.
    DATA ls_object  LIKE LINE OF lt_objects.

    ls_object-pgmid = 'R3TR'.
    ls_object-object = 'ZAG1'.
    INSERT ls_object INTO TABLE lt_objects.

    result->get_output_configuration( )->set_table_parameter(
      name  = 'WT_OBJECT_TEXT'
      value = lt_objects ).

  ENDMETHOD.
ENDCLASS.

************************************************************************

CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL CRITICAL FINAL.

  PUBLIC SECTION.
    INTERFACES if_ftd_invocation_answer.

  PRIVATE SECTION.
    METHODS setup.
    METHODS teardown.
    METHODS upsert FOR TESTING RAISING cx_static_check.

    CONSTANTS c_package TYPE devclass VALUE 'ZFOOBAR'.
    DATA mi_env TYPE REF TO if_function_test_environment.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    DATA lt_deps    TYPE if_function_test_environment=>tt_function_dependencies.
    DATA lo_initial TYPE REF TO zif_abapgit_repo_srv.
    DATA lo_tr_object_table TYPE REF TO lcl_tr_object_table.

    zcl_abapgit_repo_srv=>inject_instance( lo_initial ).

    CREATE OBJECT lo_tr_object_table.

    INSERT 'ENQUEUE_EZABAPGIT' INTO TABLE lt_deps.
    INSERT 'DEQUEUE_EZABAPGIT' INTO TABLE lt_deps.
    INSERT 'TR_OBJECT_TABLE' INTO TABLE lt_deps.

    mi_env = cl_function_test_environment=>create( lt_deps ).
    mi_env->get_double( 'ENQUEUE_EZABAPGIT' )->configure_call( )->ignore_all_parameters(
      )->then_answer( me ).
    mi_env->get_double( 'DEQUEUE_EZABAPGIT' )->configure_call( )->ignore_all_parameters(
      )->then_answer( me ).
    mi_env->get_double( 'TR_OBJECT_TABLE' )->configure_call( )->ignore_all_parameters(
      )->then_answer( lo_tr_object_table ).

  ENDMETHOD.

  METHOD teardown.
    mi_env->clear_doubles( ).
  ENDMETHOD.

  METHOD if_ftd_invocation_answer~answer.
    RETURN.
  ENDMETHOD.

  METHOD upsert.

    DATA lo_online TYPE REF TO zcl_abapgit_repo_online.
    DATA lt_result TYPE zif_abapgit_definitions=>ty_results_tt.
    DATA lv_url    TYPE string.


    ASSERT sy-sysid = 'ABC'.

    lv_url = zcl_abapgit_gitea=>create_repo( 'repo-' && cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ) ).

    zcl_abapgit_object_zag1=>upsert(
      iv_name    = 'ZAG1_NAME'
      iv_value   = 'hello'
      iv_package = c_package ).

    lo_online ?= zcl_abapgit_repo_srv=>get_instance( )->new_online(
      iv_url     = lv_url
      iv_package = c_package ).

    cl_abap_unit_assert=>assert_not_initial( lo_online ).

    lt_result = zcl_abapgit_repo_status=>calculate( lo_online ).

    cl_abap_unit_assert=>assert_equals(
      exp = lines( lt_result )
      act = 2 ).

  ENDMETHOD.

ENDCLASS.
