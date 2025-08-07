CLASS zcl_abapgit_repo_status DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS calculate
      IMPORTING
        !ii_repo          TYPE REF TO zif_abapgit_repo
        !ii_log           TYPE REF TO zif_abapgit_log OPTIONAL
        !ii_obj_filter    TYPE REF TO zif_abapgit_object_filter OPTIONAL
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_repo_status IMPLEMENTATION.


  METHOD calculate.

    DATA lt_local TYPE zif_abapgit_definitions=>ty_files_item_tt.
    DATA lt_remote TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA li_exit TYPE REF TO zif_abapgit_exit.
    DATA li_instance TYPE REF TO zif_abapgit_status_calc.
    DATA lo_consistency_checks TYPE REF TO lcl_status_consistency_checks.

    IF ii_obj_filter IS INITIAL.
      lt_local = ii_repo->get_files_local( ii_log ).
    ELSE.
      lt_local = ii_repo->get_files_local_filtered(
        ii_log        = ii_log
        ii_obj_filter = ii_obj_filter ).
    ENDIF.

    IF lines( lt_local ) <= 2 AND ii_obj_filter IS INITIAL.
      " Less equal two means that we have only the .abapgit.xml and the package in
      " our local repository. In this case we have to update our local .abapgit.xml
      " from the remote one. Otherwise we get errors when e.g. the folder starting
      " folder is different.
      ii_repo->find_remote_dot_abapgit( ).
    ENDIF.

    lt_remote = ii_repo->get_files_remote( ii_obj_filter = ii_obj_filter
                                           iv_ignore_files = abap_true ).

    li_exit = zcl_abapgit_exit=>get_instance( ).
    li_exit->pre_calculate_repo_status(
      EXPORTING
        is_repo_meta = ii_repo->ms_data
      CHANGING
        ct_local  = lt_local
        ct_remote = lt_remote ).

    li_instance = zcl_abapgit_status_calc=>get_instance(
      iv_root_package = ii_repo->get_package( )
      io_dot          = ii_repo->get_dot_abapgit( ) ).

    rt_results = li_instance->calculate_status(
      it_local     = lt_local
      it_remote    = lt_remote
      it_cur_state = ii_repo->checksums( )->get_checksums_per_file( ) ).

    IF ii_log IS BOUND.
      " This method just adds messages to the log. No log, nothing to do here
      CREATE OBJECT lo_consistency_checks
        EXPORTING
          iv_root_package = ii_repo->get_package( )
          io_dot          = ii_repo->get_dot_abapgit( ).
      ii_log->merge_with( lo_consistency_checks->run_checks( rt_results ) ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
