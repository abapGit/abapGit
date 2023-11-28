CLASS zcl_abapgit_function_module DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_function_module.

  protected section.
*"* protected components of class ZCL_ABAPGIT_FUNCTION_MODULE
*"* do not include other source files here!!!
  private section.
*"* private components of class ZCL_ABAPGIT_FUNCTION_MODULE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_ABAPGIT_FUNCTION_MODULE IMPLEMENTATION.


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
