INTERFACE zif_abapgit_data_serializer
  PUBLIC .

  METHODS serialize
    IMPORTING
      !ii_config      TYPE REF TO zif_abapgit_data_config
    RETURNING
      VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_tt .

ENDINTERFACE.
