INTERFACE zif_abapgit_stage_logic
  PUBLIC .

  METHODS get
    IMPORTING
      !io_repo        TYPE REF TO zcl_abapgit_repo_online
      !ii_obj_filter  TYPE REF TO zif_abapgit_object_filter OPTIONAL
    RETURNING
      VALUE(rs_files) TYPE zif_abapgit_definitions=>ty_stage_files
    RAISING
      zcx_abapgit_exception .

ENDINTERFACE.
