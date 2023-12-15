CLASS zcl_abapgit_flow_exit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_flow_exit .

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ri_exit) TYPE REF TO zif_abapgit_flow_exit.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gi_global_exit TYPE REF TO zif_abapgit_flow_exit.
    CLASS-DATA gi_exit TYPE REF TO zif_abapgit_flow_exit.
ENDCLASS.



CLASS ZCL_ABAPGIT_FLOW_EXIT IMPLEMENTATION.


  METHOD get_instance.
* this exit only works with global classes

    IF gi_global_exit IS NOT INITIAL.
      ri_exit = gi_global_exit.
      RETURN.
    ENDIF.

    TRY.
        CREATE OBJECT gi_exit TYPE ('ZCL_ABAPGIT_FLOW_USER_EXIT').
      CATCH cx_sy_create_object_error ##NO_HANDLER.
    ENDTRY.

    CREATE OBJECT gi_global_exit TYPE zcl_abapgit_flow_exit. " this class

    ri_exit = gi_global_exit.

  ENDMETHOD.


  METHOD zif_abapgit_flow_exit~on_event.

    IF gi_exit IS NOT INITIAL.
      TRY.
          rs_result = gi_exit->on_event(
           ii_event    = ii_event
           it_features = it_features ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_flow_exit~toolbar_extras.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->toolbar_extras(
            io_toolbar = io_toolbar
            iv_index   = iv_index
            is_feature = is_feature ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
