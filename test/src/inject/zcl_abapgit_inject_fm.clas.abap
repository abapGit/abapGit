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
    DATA lo_doma_put_handler TYPE REF TO zcl_abapgit_fm_ddif_doma_put.
    DATA lo_doma_get_handler TYPE REF TO zcl_abapgit_fm_ddif_doma_get.
    DATA lo_dtel_put_handler TYPE REF TO zcl_abapgit_fm_ddif_dtel_put.


    APPEND 'ENQUEUE_EZABAPGIT' TO lt_deps.
    APPEND 'DEQUEUE_EZABAPGIT' TO lt_deps.
    APPEND 'SAPGUI_PROGRESS_INDICATOR' TO lt_deps.
    APPEND 'TR_OBJECT_TABLE' TO lt_deps.
    APPEND 'SEO_INTERFACE_IMPLEM_GET_ALL' TO lt_deps.
    APPEND 'DDIF_DOMA_PUT' TO lt_deps.
    APPEND 'DDIF_DOMA_GET' TO lt_deps.
    APPEND 'DDIF_DTEL_PUT' TO lt_deps.
    gi_env = cl_function_test_environment=>create( lt_deps ).

    CREATE OBJECT lo_handler.
    CREATE OBJECT lo_doma_put_handler.
    CREATE OBJECT lo_doma_get_handler.
    CREATE OBJECT lo_dtel_put_handler.
    LOOP AT lt_deps INTO lv_dep.
      IF lv_dep = 'DDIF_DOMA_PUT'.
        gi_env->get_double( lv_dep )->configure_call( )->ignore_all_parameters( )->then_answer( lo_doma_put_handler ).
      ELSEIF lv_dep = 'DDIF_DOMA_GET'.
        gi_env->get_double( lv_dep )->configure_call( )->ignore_all_parameters( )->then_answer( lo_doma_get_handler ).
      ELSEIF lv_dep = 'DDIF_DTEL_PUT'.
        gi_env->get_double( lv_dep )->configure_call( )->ignore_all_parameters( )->then_answer( lo_dtel_put_handler ).
      ELSE.
        gi_env->get_double( lv_dep )->configure_call( )->ignore_all_parameters( )->then_answer( lo_handler ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD clear.
    gi_env->clear_doubles( ).
    CLEAR gi_env.
  ENDMETHOD.

ENDCLASS.
