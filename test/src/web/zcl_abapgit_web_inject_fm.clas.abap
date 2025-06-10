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

    DATA lt_deps    TYPE if_function_test_environment=>tt_function_dependencies.
    DATA lv_dep     LIKE LINE OF lt_deps.
    DATA li_env     TYPE REF TO if_function_test_environment.
    DATA lo_handler TYPE REF TO zcl_abapgit_web_inject_fm.


    INSERT 'ENQUEUE_EZABAPGIT' INTO TABLE lt_deps.
    INSERT 'DEQUEUE_EZABAPGIT' INTO TABLE lt_deps.
    INSERT 'SAPGUI_PROGRESS_INDICATOR' INTO TABLE lt_deps.
    INSERT 'TR_OBJECT_TABLE' INTO TABLE lt_deps.
    INSERT 'SEO_INTERFACE_IMPLEM_GET_ALL' INTO TABLE lt_deps.
    li_env = cl_function_test_environment=>create( lt_deps ).

    CREATE OBJECT lo_handler.
    LOOP AT lt_deps INTO lv_dep.
      li_env->get_double( lv_dep )->configure_call( )->ignore_all_parameters( )->then_answer( lo_handler ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
