CLASS zcl_abapgit_file_status DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS status
      IMPORTING
        !io_repo          TYPE REF TO zcl_abapgit_repo
        !ii_log           TYPE REF TO zif_abapgit_log OPTIONAL
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .

    METHODS constructor
      IMPORTING
        !iv_root_package TYPE devclass
        !io_dot          TYPE REF TO zcl_abapgit_dot_abapgit.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_root_package TYPE devclass.
    DATA mo_dot          TYPE REF TO zcl_abapgit_dot_abapgit.

    METHODS calculate_status
      IMPORTING
        !it_local         TYPE zif_abapgit_definitions=>ty_files_item_tt
        !it_remote        TYPE zif_abapgit_git_definitions=>ty_files_tt
        !it_cur_state     TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
    METHODS process_local
      IMPORTING
        !it_local     TYPE zif_abapgit_definitions=>ty_files_item_tt
        !it_state_idx TYPE zif_abapgit_git_definitions=>ty_file_signatures_ts
      CHANGING
        !ct_remote    TYPE zif_abapgit_git_definitions=>ty_files_tt
        !ct_items     TYPE zif_abapgit_definitions=>ty_items_tt
        !ct_results   TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
    METHODS process_items
      IMPORTING
        !it_unprocessed_remote TYPE zif_abapgit_git_definitions=>ty_files_tt
      CHANGING
        !ct_items    TYPE zif_abapgit_definitions=>ty_items_tt
      RAISING
        zcx_abapgit_exception .
    METHODS process_remote
      IMPORTING
        !it_local     TYPE zif_abapgit_definitions=>ty_files_item_tt
        !it_unprocessed_remote TYPE zif_abapgit_git_definitions=>ty_files_tt
        !it_state_idx TYPE zif_abapgit_git_definitions=>ty_file_signatures_ts
        !it_items_idx TYPE zif_abapgit_definitions=>ty_items_ts
      CHANGING
        !ct_results   TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS build_existing
      IMPORTING
        !is_local        TYPE zif_abapgit_definitions=>ty_file_item
        !is_remote       TYPE zif_abapgit_git_definitions=>ty_file
        !it_state        TYPE zif_abapgit_git_definitions=>ty_file_signatures_ts
      RETURNING
        VALUE(rs_result) TYPE zif_abapgit_definitions=>ty_result .
    CLASS-METHODS build_new_local
      IMPORTING
        !is_local        TYPE zif_abapgit_definitions=>ty_file_item
      RETURNING
        VALUE(rs_result) TYPE zif_abapgit_definitions=>ty_result .
    METHODS build_new_remote
      IMPORTING
        !is_remote       TYPE zif_abapgit_git_definitions=>ty_file
        !it_items_idx    TYPE zif_abapgit_definitions=>ty_items_ts
        !it_state_idx    TYPE zif_abapgit_git_definitions=>ty_file_signatures_ts
      RETURNING
        VALUE(rs_result) TYPE zif_abapgit_definitions=>ty_result
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_object_package
      IMPORTING
        !iv_object         TYPE tadir-object
        !iv_obj_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rv_devclass) TYPE devclass
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS check_local_remote_consistency
      IMPORTING
        !is_local        TYPE zif_abapgit_definitions=>ty_file_item
        !is_remote       TYPE zif_abapgit_git_definitions=>ty_file
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS ensure_state
      IMPORTING
        !it_local         TYPE zif_abapgit_definitions=>ty_files_item_tt
        !it_cur_state     TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
      RETURNING
        VALUE(rt_state) TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt.

ENDCLASS.



CLASS zcl_abapgit_file_status IMPLEMENTATION.


  METHOD build_existing.

    DATA ls_file_sig LIKE LINE OF it_state.

    " Item
    rs_result-obj_type  = is_local-item-obj_type.
    rs_result-obj_name  = is_local-item-obj_name.
    rs_result-package   = is_local-item-devclass.
    rs_result-srcsystem = is_local-item-srcsystem.
    rs_result-origlang  = is_local-item-origlang.
    rs_result-inactive  = is_local-item-inactive.

    " File
    rs_result-path     = is_local-file-path.
    rs_result-filename = is_local-file-filename.

    rs_result-match    = boolc( is_local-file-sha1 = is_remote-sha1 ).
    IF rs_result-match = abap_true.
      RETURN.
    ENDIF.

    " Match against current state
    READ TABLE it_state INTO ls_file_sig
      WITH KEY
        path     = is_local-file-path
        filename = is_local-file-filename
      BINARY SEARCH.

    IF sy-subrc = 0.
      IF ls_file_sig-sha1 <> is_local-file-sha1.
        rs_result-lstate = zif_abapgit_definitions=>c_state-modified.
      ENDIF.
      IF ls_file_sig-sha1 <> is_remote-sha1.
        rs_result-rstate = zif_abapgit_definitions=>c_state-modified.
      ENDIF.
    ELSE.
      " This is a strange situation. As both local and remote exist
      " the state should also be present. Maybe this is a first run of the code.
      " In this case just compare hashes directly and mark both changed
      " the user will presumably decide what to do after checking the actual diff
      rs_result-lstate = zif_abapgit_definitions=>c_state-modified.
      rs_result-rstate = zif_abapgit_definitions=>c_state-modified.
    ENDIF.

  ENDMETHOD.


  METHOD build_new_local.

    " Item
    rs_result-obj_type  = is_local-item-obj_type.
    rs_result-obj_name  = is_local-item-obj_name.
    rs_result-package   = is_local-item-devclass.
    rs_result-srcsystem = is_local-item-srcsystem.
    rs_result-origlang  = is_local-item-origlang.
    rs_result-inactive  = is_local-item-inactive.

    " File
    rs_result-path     = is_local-file-path.
    rs_result-filename = is_local-file-filename.

    " Match
    rs_result-match    = abap_false.
    rs_result-lstate   = zif_abapgit_definitions=>c_state-added.

  ENDMETHOD.


  METHOD build_new_remote.

    DATA ls_item     LIKE LINE OF it_items_idx.
    DATA ls_file_sig LIKE LINE OF it_state_idx.

    " Common and default part
    rs_result-path     = is_remote-path.
    rs_result-filename = is_remote-filename.
    rs_result-match    = abap_false.
    rs_result-rstate   = zif_abapgit_definitions=>c_state-added.

    zcl_abapgit_filename_logic=>file_to_object(
      EXPORTING
        iv_filename = is_remote-filename
        iv_path     = is_remote-path
        iv_devclass = mv_root_package
        io_dot      = mo_dot
      IMPORTING
        es_item     = ls_item ).

    " Check if in item index + get package
    READ TABLE it_items_idx INTO ls_item
      WITH KEY
        obj_type = ls_item-obj_type
        obj_name = ls_item-obj_name.

    IF sy-subrc = 0.

      " Completely new (xml, abap) and new file in an existing object
      rs_result-obj_type  = ls_item-obj_type.
      rs_result-obj_name  = ls_item-obj_name.
      rs_result-package   = ls_item-devclass.
      rs_result-srcsystem = sy-sysid.
      rs_result-origlang  = sy-langu.

      READ TABLE it_state_idx INTO ls_file_sig
        WITH KEY
          path     = is_remote-path
          filename = is_remote-filename.

      " Existing file but from another package
      " was not added during local file proc as was not in tadir for repo package
      IF sy-subrc = 0.
        IF ls_file_sig-sha1 = is_remote-sha1.
          rs_result-match = abap_true.
          CLEAR rs_result-rstate.
        ELSE.
          rs_result-rstate = zif_abapgit_definitions=>c_state-modified.
        ENDIF.

        " Item is in state and in cache but with no package - it was deleted
        " OR devclass is the same as repo package (see #532)
        IF ls_item-devclass IS INITIAL OR ls_item-devclass = mv_root_package.
          rs_result-match  = abap_false.
          rs_result-lstate = zif_abapgit_definitions=>c_state-deleted.
        ENDIF.
      ENDIF.

    ELSE. " Completely unknown file, probably non-abapgit
      ASSERT 1 = 1. " No action, just follow defaults
    ENDIF.

  ENDMETHOD.


  METHOD calculate_status.

    DATA:
      lt_remote        LIKE it_remote,
      lt_items         TYPE zif_abapgit_definitions=>ty_items_tt,
      lt_items_by_obj  TYPE zif_abapgit_definitions=>ty_items_ts, " Sorted by obj_type+obj_name
      lt_state_by_file TYPE zif_abapgit_git_definitions=>ty_file_signatures_ts. " Sorted by path+filename

    lt_state_by_file = ensure_state( " Index by file
      it_cur_state = it_cur_state
      it_local     = it_local ).
    lt_remote        = it_remote.

    " Process local files and new local files
    process_local(
      EXPORTING
        it_local     = it_local
        it_state_idx = lt_state_by_file
      CHANGING
        ct_remote    = lt_remote
        ct_items     = lt_items
        ct_results   = rt_results ).

    " Remove processed remotes (with cleared SHA1)
    DELETE lt_remote WHERE sha1 IS INITIAL.

    " Complete item index for unmarked remote files
    process_items( " TODO: rename ?
      EXPORTING
        it_unprocessed_remote = lt_remote
      CHANGING
        ct_items              = lt_items ).

    " The item list was not unique by now, just collected as "mention" list
    SORT lt_items DESCENDING. " Default key - type, name, pkg, ...
    DELETE ADJACENT DUPLICATES FROM lt_items COMPARING obj_type obj_name.
    lt_items_by_obj = lt_items.

    " Process new remote files (marked above with empty SHA1)
    process_remote(
      EXPORTING
        it_local              = it_local
        it_unprocessed_remote = lt_remote
        it_state_idx          = lt_state_by_file
        it_items_idx          = lt_items_by_obj
      CHANGING
        ct_results            = rt_results ).

    SORT rt_results BY
      obj_type ASCENDING
      obj_name ASCENDING
      filename ASCENDING
      path ASCENDING.

  ENDMETHOD.


  METHOD check_local_remote_consistency.
    IF is_remote-sha1 IS INITIAL.
      IF is_local-file-filename = zcl_abapgit_filename_logic=>c_package_file.
        zcx_abapgit_exception=>raise(
          |Package name conflict { is_local-item-obj_type } { is_local-item-obj_name }. | &&
          |Rename package or use FULL folder logic| ).
      ELSE.
        zcx_abapgit_exception=>raise(
          |Checksum conflict { is_local-item-obj_type } { is_local-item-obj_name }. | &&
          |Please create an issue on Github| ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    mv_root_package = iv_root_package.
    mo_dot          = io_dot.
  ENDMETHOD.


  METHOD ensure_state.

    FIELD-SYMBOLS <ls_state> LIKE LINE OF rt_state.
    FIELD-SYMBOLS <ls_local> LIKE LINE OF it_local.

    IF lines( it_cur_state ) = 0.
      " Empty state is usually not expected. Maybe for new repos.
      " In this case suppose the local state is unchanged
      LOOP AT it_local ASSIGNING <ls_local>.
        APPEND INITIAL LINE TO rt_state ASSIGNING <ls_state>.
        MOVE-CORRESPONDING <ls_local>-file TO <ls_state>.
      ENDLOOP.
    ELSE.
      rt_state = it_cur_state.
    ENDIF.

  ENDMETHOD.


  METHOD get_object_package.
    DATA: lv_name    TYPE devclass,
          li_package TYPE REF TO zif_abapgit_sap_package.

    rv_devclass = zcl_abapgit_factory=>get_tadir( )->get_object_package(
      iv_object   = iv_object
      iv_obj_name = iv_obj_name ).
    IF rv_devclass IS INITIAL AND iv_object = 'DEVC' AND iv_obj_name(1) = '$'.
      " local packages usually have no tadir entry
      lv_name = iv_obj_name.
      li_package = zcl_abapgit_factory=>get_sap_package( lv_name ).
      IF li_package->exists( ) = abap_true.
        rv_devclass = lv_name.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD process_items.

    DATA:
      ls_item         LIKE LINE OF ct_items,
      lv_is_xml       TYPE abap_bool,
      lv_is_json      TYPE abap_bool,
      lv_sub_fetched  TYPE abap_bool,
      lt_sub_packages TYPE SORTED TABLE OF devclass WITH UNIQUE KEY table_line.

    FIELD-SYMBOLS <ls_remote> LIKE LINE OF it_unprocessed_remote.

    LOOP AT it_unprocessed_remote ASSIGNING <ls_remote>.

      zcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = <ls_remote>-filename
          iv_path     = <ls_remote>-path
          io_dot      = mo_dot
          iv_devclass = mv_root_package
        IMPORTING
          es_item     = ls_item
          ev_is_xml   = lv_is_xml
          ev_is_json  = lv_is_json ).

      CHECK lv_is_xml = abap_true OR lv_is_json = abap_true. " only object definitions

      ls_item-devclass = get_object_package(
        iv_object   = ls_item-obj_type
        iv_obj_name = ls_item-obj_name ).

      IF ls_item-devclass IS NOT INITIAL AND mv_root_package <> ls_item-devclass.
        IF lv_sub_fetched = abap_false.
          lt_sub_packages = zcl_abapgit_factory=>get_sap_package( mv_root_package )->list_subpackages( ).
          lv_sub_fetched  = abap_true.
        ENDIF.

        " Make sure the package is under the repo main package
        READ TABLE lt_sub_packages TRANSPORTING NO FIELDS
          WITH KEY table_line = ls_item-devclass.
        IF sy-subrc <> 0 AND ls_item-obj_type = 'DEVC'.
          CLEAR ls_item-devclass.
        ENDIF.
      ENDIF.

      APPEND ls_item TO ct_items.
    ENDLOOP.

  ENDMETHOD.


  METHOD process_local.

    FIELD-SYMBOLS:
      <ls_remote> LIKE LINE OF ct_remote,
      <ls_result> LIKE LINE OF ct_results,
      <ls_state>  LIKE LINE OF it_state_idx,
      <ls_local>  LIKE LINE OF it_local.

    LOOP AT it_local ASSIGNING <ls_local>.
      " Skip ignored files
      CHECK mo_dot->is_ignored(
        iv_path     = <ls_local>-file-path
        iv_filename = <ls_local>-file-filename ) = abap_false.

      IF <ls_local>-item IS NOT INITIAL
        AND zcl_abapgit_filename_logic=>is_obj_definition_file( <ls_local>-file-filename ) = abap_true.
        " Collect for item index
        APPEND <ls_local>-item TO ct_items.
      ENDIF.

      APPEND INITIAL LINE TO ct_results ASSIGNING <ls_result>.

      " Find a match in remote
      READ TABLE ct_remote ASSIGNING <ls_remote>
        WITH KEY file_path
        COMPONENTS
          path     = <ls_local>-file-path
          filename = <ls_local>-file-filename.
      IF sy-subrc = 0.  " Both local and remote exist
        check_local_remote_consistency(
          is_local  = <ls_local>
          is_remote = <ls_remote> ).
        <ls_result> = build_existing(
          is_local  = <ls_local>
          is_remote = <ls_remote>
          it_state  = it_state_idx ).
        CLEAR <ls_remote>-sha1. " Mark as processed
      ELSE. " Only local exists
        <ls_result> = build_new_local( <ls_local> ).
        " Check if same file exists in different location
        READ TABLE ct_remote ASSIGNING <ls_remote>
          WITH KEY file
          COMPONENTS filename = <ls_local>-file-filename.
        IF sy-subrc = 0 AND <ls_local>-file-sha1 = <ls_remote>-sha1.
          " If yes, then it was probably moved
          <ls_result>-packmove = abap_true.
        ELSEIF sy-subrc = 4.
          " Check if file existed before and was deleted remotely
          READ TABLE it_state_idx ASSIGNING <ls_state>
            WITH KEY
              path     = <ls_local>-file-path
              filename = <ls_local>-file-filename.
          IF sy-subrc = 0.
            IF <ls_local>-file-sha1 = <ls_state>-sha1.
              <ls_result>-lstate = zif_abapgit_definitions=>c_state-unchanged.
            ELSE.
              <ls_result>-lstate = zif_abapgit_definitions=>c_state-modified.
            ENDIF.
            <ls_result>-rstate = zif_abapgit_definitions=>c_state-deleted. " ??
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD process_remote.

    FIELD-SYMBOLS:
      <ls_remote> LIKE LINE OF it_unprocessed_remote,
      <ls_result> LIKE LINE OF ct_results,
      <ls_local>  LIKE LINE OF it_local.

    LOOP AT it_unprocessed_remote ASSIGNING <ls_remote>.

      APPEND INITIAL LINE TO ct_results ASSIGNING <ls_result>.

      <ls_result> = build_new_remote(
        is_remote   = <ls_remote>
        it_items_idx = it_items_idx
        it_state_idx = it_state_idx ).

      " Check if same file exists in different location (not for generic package files)
      READ TABLE it_local ASSIGNING <ls_local>
        WITH KEY file-filename = <ls_remote>-filename.
      IF sy-subrc = 0 AND <ls_remote>-filename <> zcl_abapgit_filename_logic=>c_package_file.
        <ls_result>-match = abap_false.
        <ls_result>-lstate = zif_abapgit_definitions=>c_state-deleted.
        <ls_result>-rstate = zif_abapgit_definitions=>c_state-unchanged.
        IF <ls_local>-file-sha1 = <ls_remote>-sha1.
          <ls_result>-packmove = abap_true.
        ENDIF.
      ELSE.
        " Check if file existed before and was deleted locally
        READ TABLE it_state_idx TRANSPORTING NO FIELDS
          WITH KEY
            path     = <ls_remote>-path
            filename = <ls_remote>-filename.
        IF sy-subrc = 0.
          <ls_result>-match  = abap_false.
          <ls_result>-lstate = zif_abapgit_definitions=>c_state-deleted.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD status.

    DATA lt_local TYPE zif_abapgit_definitions=>ty_files_item_tt.
    DATA lt_remote TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA li_exit TYPE REF TO zif_abapgit_exit.
    DATA lo_instance TYPE REF TO zcl_abapgit_file_status.
    DATA lo_consistency_checks TYPE REF TO lcl_status_consistency_checks.

    lt_local = io_repo->get_files_local( ii_log = ii_log ).

    IF lines( lt_local ) <= 2.
      " Less equal two means that we have only the .abapgit.xml and the package in
      " our local repository. In this case we have to update our local .abapgit.xml
      " from the remote one. Otherwise we get errors when e.g. the folder starting
      " folder is different.
      io_repo->find_remote_dot_abapgit( ).
    ENDIF.

    lt_remote = io_repo->get_files_remote( iv_ignore_files = abap_true ).

    li_exit = zcl_abapgit_exit=>get_instance( ).
    li_exit->pre_calculate_repo_status(
      EXPORTING
        is_repo_meta = io_repo->ms_data
      CHANGING
        ct_local  = lt_local
        ct_remote = lt_remote ).

    CREATE OBJECT lo_instance
      EXPORTING
        iv_root_package = io_repo->get_package( )
        io_dot          = io_repo->get_dot_abapgit( ).

    rt_results = lo_instance->calculate_status(
      it_local     = lt_local
      it_remote    = lt_remote
      it_cur_state = io_repo->zif_abapgit_repo~checksums( )->get_checksums_per_file( ) ).

    IF ii_log IS BOUND.
      " This method just adds messages to the log. No log, nothing to do here
      CREATE OBJECT lo_consistency_checks
        EXPORTING
          iv_root_package = io_repo->get_package( )
          io_dot          = io_repo->get_dot_abapgit( ).
      ii_log->merge_with( lo_consistency_checks->run_checks( rt_results ) ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
