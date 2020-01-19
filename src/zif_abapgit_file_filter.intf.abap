INTERFACE zif_abapgit_file_filter
  PUBLIC .
  METHODS:
    filter_remote_files
      CHANGING
        ct_files TYPE zif_abapgit_definitions=>ty_files_tt,

    filter_local_files
      CHANGING
        ct_files TYPE zif_abapgit_definitions=>ty_files_item_tt.

ENDINTERFACE.
