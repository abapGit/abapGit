INTERFACE zif_abapgit_data_serializer
  PUBLIC .


  METHODS serialize
    IMPORTING
      !ii_config      TYPE REF TO zif_abapgit_data_config
    RETURNING
      VALUE(rt_files) TYPE zif_abapgit_git_definitions=>ty_files_tt
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
