INTERFACE zif_abapgit_data_deserializer
  PUBLIC .

  METHODS deserialize
    IMPORTING
      ii_config TYPE REF TO zif_abapgit_data_config
      it_files  TYPE zif_abapgit_definitions=>ty_files_tt.

ENDINTERFACE.
