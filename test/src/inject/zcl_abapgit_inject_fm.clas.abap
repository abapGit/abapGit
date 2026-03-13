CLASS zcl_abapgit_inject_fm DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_ftd_invocation_answer.
    CLASS-METHODS inject.
    CLASS-METHODS clear.
  PRIVATE SECTION.
    CLASS-DATA gi_env TYPE REF TO if_function_test_environment.
ENDCLASS.

CLASS zcl_abapgit_inject_fm IMPLEMENTATION.

  METHOD if_ftd_invocation_answer~answer.
    RETURN.
  ENDMETHOD.

  METHOD inject.

    DATA lt_deps    TYPE if_function_test_environment=>tt_function_dependencies.
    DATA lv_dep     LIKE LINE OF lt_deps.
    DATA lo_handler TYPE REF TO zcl_abapgit_inject_fm.


    INSERT 'ENQUEUE_EZABAPGIT' INTO TABLE lt_deps.
    INSERT 'DEQUEUE_EZABAPGIT' INTO TABLE lt_deps.
    INSERT 'SAPGUI_PROGRESS_INDICATOR' INTO TABLE lt_deps.
    INSERT 'TR_OBJECT_TABLE' INTO TABLE lt_deps.
    INSERT 'SEO_INTERFACE_IMPLEM_GET_ALL' INTO TABLE lt_deps.
    gi_env = cl_function_test_environment=>create( lt_deps ).

    CREATE OBJECT lo_handler.
    LOOP AT lt_deps INTO lv_dep.
      gi_env->get_double( lv_dep )->configure_call( )->ignore_all_parameters( )->then_answer( lo_handler ).
    ENDLOOP.

  ENDMETHOD.

  METHOD clear.
    gi_env->clear_doubles( ).
    CLEAR gi_env.
  ENDMETHOD.

ENDCLASS.
