INTERFACE zif_abapgit_object_checks
  PUBLIC.

  METHODS skip_serialization
    RETURNING
      VALUE(rv_skip) TYPE abap_bool.

ENDINTERFACE.
