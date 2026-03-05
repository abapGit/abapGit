INTERFACE zif_abapgit_ajson_iterator
  PUBLIC.

  METHODS has_next
    RETURNING
      VALUE(rv_yes) TYPE abap_bool.

  METHODS next
    RETURNING
      VALUE(ri_item) TYPE REF TO zif_abapgit_ajson.

ENDINTERFACE.
