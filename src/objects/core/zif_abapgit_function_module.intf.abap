INTERFACE zif_abapgit_function_module
  PUBLIC.

  METHODS:
    function_exists
      IMPORTING
        iv_function_module_name TYPE rs38l-name
      RETURNING
        VALUE(rv_exists)        TYPE abap_bool.

ENDINTERFACE.
