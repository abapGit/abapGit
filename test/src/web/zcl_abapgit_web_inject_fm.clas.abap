CLASS zcl_abapgit_web_inject_fm DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_ftd_invocation_answer.
    CLASS-METHODS inject.
ENDCLASS.

CLASS zcl_abapgit_web_inject_fm IMPLEMENTATION.
  METHOD if_ftd_invocation_answer~answer.
    RETURN.
  ENDMETHOD.

  METHOD inject.

    DATA lt_deps TYPE if_function_test_environment=>tt_function_dependencies.
    DATA li_env  TYPE REF TO if_function_test_environment.
    DATA lo_handler TYPE REF TO zcl_abapgit_web_inject_fm.


    INSERT 'ENQUEUE_EZABAPGIT' INTO TABLE lt_deps.
    INSERT 'DEQUEUE_EZABAPGIT' INTO TABLE lt_deps.
    li_env = cl_function_test_environment=>create( lt_deps ).

    CREATE OBJECT lo_handler.
    li_env->get_double( 'ENQUEUE_EZABAPGIT' )->configure_call( )->ignore_all_parameters( )->then_answer( lo_handler ).
    li_env->get_double( 'DEQUEUE_EZABAPGIT' )->configure_call( )->ignore_all_parameters( )->then_answer( lo_handler ).

  ENDMETHOD.

ENDCLASS.
