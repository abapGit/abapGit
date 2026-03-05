INTERFACE zif_abapgit_status_calc PUBLIC.

  METHODS calculate_status
    IMPORTING
      !it_local         TYPE zif_abapgit_definitions=>ty_files_item_tt
      !it_remote        TYPE zif_abapgit_git_definitions=>ty_files_tt
      !it_cur_state     TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
    RETURNING
      VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
