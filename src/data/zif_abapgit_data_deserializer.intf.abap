INTERFACE zif_abapgit_data_deserializer
  PUBLIC .

  METHODS deserialize
    IMPORTING
      it_files TYPE zif_abapgit_definitions=>ty_files_tt.

ENDINTERFACE.
