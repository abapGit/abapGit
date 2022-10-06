INTERFACE zif_abapgit_aff_registry
  PUBLIC .

  METHODS:
    "! Returns TRUE if the object type is supported by ABAP file formats (AFF) in abapGit.<br/>
    "! Either there is a (standalone AFF capable) object handler,
    "! or object handler calls the AFF framework in newer ABAP systems.
    is_supported_object_type
      IMPORTING
        iv_obj_type      TYPE tadir-object
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
ENDINTERFACE.
