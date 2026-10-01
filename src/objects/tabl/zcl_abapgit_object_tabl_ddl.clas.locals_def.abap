" Maps between DD02V-VIEWREF (the database view name) and the CDS entity
" name that DDL uses in @AbapCatalog.replacementObject. The lookup depends
" on the objects present in the system, so unit tests replace it.
INTERFACE lif_replacement_mapping.

  METHODS to_entity
    IMPORTING
      !iv_view_name        TYPE ddobjname
    RETURNING
      VALUE(rv_entityname) TYPE string.
  METHODS to_view
    IMPORTING
      !iv_entityname      TYPE ddobjname
    RETURNING
      VALUE(rv_view_name) TYPE string.

ENDINTERFACE.
