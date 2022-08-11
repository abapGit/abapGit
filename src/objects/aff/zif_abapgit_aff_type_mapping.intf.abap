INTERFACE zif_abapgit_aff_type_mapping
  PUBLIC .
  METHODS:
    "! Convert from AFF to abapGit data
    "!
    "! @parameter iv_data | ABAP data as AFF type
    "! @parameter iv_object_name | Name of object
    "! @parameter es_data | ABAP data as abapGit type
    to_abapgit
      IMPORTING iv_data TYPE data
                iv_object_name TYPE sobj_name
      EXPORTING es_data TYPE data,

    "! Converts to AFF specific meta data
    "!
    "! @parameter iv_data | (meta-)data of the object
    "! @parameter es_data | aff data of the object, e.g. zif_abapgit_aff_intf_v1=>ty_main
    to_aff
      IMPORTING iv_data TYPE data
      EXPORTING es_data TYPE data.
ENDINTERFACE.
