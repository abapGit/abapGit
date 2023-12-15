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
ENDCLASS.
