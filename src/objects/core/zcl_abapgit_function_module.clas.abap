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

    DATA: lv_function_module_name TYPE c LENGTH 30.

    lv_function_module_name = iv_function_module_name.

    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = lv_function_module_name
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    rv_exists = boolc( sy-subrc = 0 ).

  ENDMETHOD.
ENDCLASS.
