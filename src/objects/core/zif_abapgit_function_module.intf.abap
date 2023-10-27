INTERFACE zif_abapgit_function_module
  PUBLIC.

  METHODS:
    function_exists
      IMPORTING
        iv_function_module_name TYPE clike
      RETURNING
        VALUE(rv_exists)        TYPE abap_bool.

ENDINTERFACE.
