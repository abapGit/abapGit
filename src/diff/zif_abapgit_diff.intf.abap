INTERFACE zif_abapgit_diff PUBLIC.

  METHODS get
    RETURNING
      VALUE(rt_diff) TYPE zif_abapgit_definitions=>ty_diffs_tt.

  METHODS stats
    RETURNING
      VALUE(rs_count) TYPE zif_abapgit_definitions=>ty_count.

  METHODS set_patch_new
    IMPORTING
      !iv_line_new   TYPE i
      !iv_patch_flag TYPE abap_bool
    RAISING
      zcx_abapgit_exception.

  METHODS set_patch_old
    IMPORTING
      !iv_line_old   TYPE i
      !iv_patch_flag TYPE abap_bool
    RAISING
      zcx_abapgit_exception.

  METHODS get_beacons
    RETURNING
      VALUE(rt_beacons) TYPE zif_abapgit_definitions=>ty_string_tt.

  METHODS is_line_patched
    IMPORTING
      iv_index          TYPE i
    RETURNING
      VALUE(rv_patched) TYPE abap_bool
    RAISING
      zcx_abapgit_exception.

  METHODS set_patch_by_old_diff
    IMPORTING
      is_diff_old   TYPE zif_abapgit_definitions=>ty_diff
      iv_patch_flag TYPE abap_bool.

ENDINTERFACE.
