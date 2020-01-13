INTERFACE zif_abapgit_file_filter
  PUBLIC .
  METHODS:
    filter
      CHANGING
        ct_files TYPE zif_abapgit_definitions=>ty_files_tt.

ENDINTERFACE.
