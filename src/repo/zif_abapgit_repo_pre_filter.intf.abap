INTERFACE zif_abapgit_repo_pre_filter
  PUBLIC .

  TYPES ty_trobj_name TYPE c LENGTH 120.    "Standard TROBJ_NAME
  TYPES ty_trrngtrkor_tt TYPE RANGE OF trkorr.
  TYPES ty_trrngtrkor TYPE LINE OF ty_trrngtrkor_tt.
  TYPES: BEGIN OF ty_e071_filter,
           pgmid    TYPE tadir-pgmid,
           object   TYPE tadir-object,
           obj_name TYPE ty_trobj_name,
         END OF ty_e071_filter,
         ty_e071_filter_tt TYPE STANDARD TABLE OF ty_e071_filter,
         ty_file_filter_tt TYPE RANGE OF string,
         ty_file_filter    TYPE LINE OF ty_file_filter_tt.

  METHODS filter_files
    CHANGING
      ct_files TYPE zif_abapgit_definitions=>ty_files_tt .

  METHODS get_local_filter
    RETURNING
      VALUE(rt_filter) TYPE zif_abapgit_definitions=>ty_tadir_tt .

  METHODS set_filter_values_via_dialog
    RAISING
      zcx_abapgit_exception .

  METHODS set_filter_values
    IMPORTING
      it_r_trkorr TYPE ty_trrngtrkor_tt
    RAISING
      zcx_abapgit_exception .

  METHODS get_filter_values
    RETURNING
      VALUE(rt_r_trkorr) TYPE ty_trrngtrkor_tt .

ENDINTERFACE.
