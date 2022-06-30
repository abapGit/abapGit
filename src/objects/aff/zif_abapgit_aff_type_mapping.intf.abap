INTERFACE zif_abapgit_aff_type_mapping
  PUBLIC .
  METHODS:
    "! Converts to AFF specific meta data
    "!
    "! @parameter iv_data | (meta-)data of the object
    "! @parameter es_data | aff data of the object, e.g. zif_abapgit_aff_intf_v1=>ty_main
    to_aff
      IMPORTING iv_data TYPE data
      EXPORTING es_data TYPE data.
ENDINTERFACE.
