INTERFACE zif_abapgit_repo_pre_filter
  PUBLIC .
  "! <p class="shorttext synchronized" lang="en"></p>
  "!
  "! @parameter CT_FILES | <p class="shorttext synchronized" lang="en">Files</p>
  METHODS filter_files
    CHANGING
      ct_files TYPE zif_abapgit_definitions=>ty_files_tt .
  "! <p class="shorttext synchronized" lang="en"></p>
  "!
  "! @parameter RT_FILTER | <p class="shorttext synchronized" lang="en">Repository Filter</p>
  METHODS get_local_filter
    RETURNING
      VALUE(rt_filter) TYPE zif_abapgit_definitions=>ty_tadir_tt .
  "! <p class="shorttext synchronized" lang="en"></p>
  METHODS set_filter_values_via_dialog
    RAISING
      zcx_abapgit_exception .
  "! <p class="shorttext synchronized" lang="en"></p>
  "!
  "! @parameter IT_R_TRKORR | <p class="shorttext synchronized" lang="en">Tab of Range Struct for E070/E071-TRKORR</p>
  METHODS set_filter_values
    IMPORTING
      it_r_trkorr TYPE trrngtrkor_tab
    RAISING
      zcx_abapgit_exception .
  "! <p class="shorttext synchronized" lang="en"></p>
  "!
  "! @parameter RT_R_TRKORR | <p class="shorttext synchronized" lang="en">Tab of Range Struct for E070/E071-TRKORR</p>
  METHODS get_filter_values
    RETURNING
      VALUE(rt_r_trkorr) TYPE trrngtrkor_tab .

ENDINTERFACE.
