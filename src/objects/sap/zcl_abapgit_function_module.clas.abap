CLASS zcl_abapgit_function_module DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_function_module.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_function_module IMPLEMENTATION.


  METHOD zif_abapgit_function_module~function_exists.

    DATA lv_function_module_name TYPE c LENGTH 30.
    DATA lv_exists TYPE string.

    lv_function_module_name = iv_function_module_name.
    lv_exists = 'FUNCTION_EXISTS'.

    TRY.
        CALL FUNCTION lv_exists
          EXPORTING
            funcname           = lv_function_module_name
          EXCEPTIONS
            function_not_exist = 1
            OTHERS             = 2.
        rv_exists = boolc( sy-subrc = 0 ).
      CATCH cx_sy_dyn_call_illegal_func.
* then its running in ABAP Cloud Programming Model, assume nothing is released
* I could not find any way to check for this -Hvam
        rv_exists = abap_false.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
