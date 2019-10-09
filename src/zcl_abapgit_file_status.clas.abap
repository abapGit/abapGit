CLASS zcl_abapgit_file_status DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS status
      IMPORTING io_repo           TYPE REF TO zcl_abapgit_repo
                ii_log            TYPE REF TO zif_abapgit_log OPTIONAL
      RETURNING VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING   zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS:
      calculate_status
        IMPORTING iv_devclass       TYPE devclass
                  io_dot            TYPE REF TO zcl_abapgit_dot_abapgit
                  it_local          TYPE zif_abapgit_definitions=>ty_files_item_tt
                  it_remote         TYPE zif_abapgit_definitions=>ty_files_tt
                  it_cur_state      TYPE zif_abapgit_definitions=>ty_file_signatures_tt
        RETURNING VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt
        RAISING   zcx_abapgit_exception,
      run_checks
        IMPORTING ii_log     TYPE REF TO zif_abapgit_log
                  it_results TYPE zif_abapgit_definitions=>ty_results_tt
                  io_dot     TYPE REF TO zcl_abapgit_dot_abapgit
                  iv_top     TYPE devclass
        RAISING   zcx_abapgit_exception,
      build_existing
        IMPORTING is_local         TYPE zif_abapgit_definitions=>ty_file_item
                  is_remote        TYPE zif_abapgit_definitions=>ty_file
                  it_state         TYPE zif_abapgit_definitions=>ty_file_signatures_ts
        RETURNING VALUE(rs_result) TYPE zif_abapgit_definitions=>ty_result,
      build_new_local
        IMPORTING is_local         TYPE zif_abapgit_definitions=>ty_file_item
        RETURNING VALUE(rs_result) TYPE zif_abapgit_definitions=>ty_result,
      build_new_remote
        IMPORTING iv_devclass      TYPE devclass
                  io_dot           TYPE REF TO zcl_abapgit_dot_abapgit
                  is_remote        TYPE zif_abapgit_definitions=>ty_file
                  it_items         TYPE zif_abapgit_definitions=>ty_items_ts
                  it_state         TYPE zif_abapgit_definitions=>ty_file_signatures_ts
        RETURNING VALUE(rs_result) TYPE zif_abapgit_definitions=>ty_result
        RAISING   zcx_abapgit_exception,
      identify_object
        IMPORTING iv_filename TYPE string
                  iv_path     TYPE string
                  iv_devclass TYPE devclass
                  io_dot      TYPE REF TO zcl_abapgit_dot_abapgit
        EXPORTING es_item     TYPE zif_abapgit_definitions=>ty_item
                  ev_is_xml   TYPE abap_bool
        RAISING   zcx_abapgit_exception,
      get_object_package
        IMPORTING
          iv_object       TYPE tadir-object
          iv_obj_name     TYPE tadir-obj_name
        RETURNING
          VALUE(rv_devclass) TYPE devclass
        RAISING
          zcx_abapgit_exception .

ENDCLASS.



CLASS ZCL_ABAPGIT_FILE_STATUS IMPLEMENTATION.


  METHOD build_existing.

    DATA: ls_file_sig LIKE LINE OF it_state.

    " Item
    rs_result-obj_type = is_local-item-obj_type.
    rs_result-obj_name = is_local-item-obj_name.
    rs_result-package  = is_local-item-devclass.

    " File
    rs_result-path     = is_local-file-path.
    rs_result-filename = is_local-file-filename.

    IF is_local-file-sha1 = is_remote-sha1.
      rs_result-match = abap_true.
      RETURN.
    ENDIF.

    " Match against current state
    READ TABLE it_state INTO ls_file_sig
      WITH KEY path = is_local-file-path
      filename = is_local-file-filename
      BINARY SEARCH.

    IF sy-subrc = 0.
      IF ls_file_sig-sha1 <> is_local-file-sha1.
        rs_result-lstate = zif_abapgit_definitions=>c_state-modified.
      ENDIF.
      IF ls_file_sig-sha1 <> is_remote-sha1.
        rs_result-rstate = zif_abapgit_definitions=>c_state-modified.
      ENDIF.
      rs_result-match = boolc( rs_result-lstate IS INITIAL
        AND rs_result-rstate IS INITIAL ).
    ELSE.
      " This is a strange situation. As both local and remote exist
      " the state should also be present. Maybe this is a first run of the code.
      " In this case just compare hashes directly and mark both changed
      " the user will presumably decide what to do after checking the actual diff
      rs_result-match = boolc( is_local-file-sha1 = is_remote-sha1 ).
      IF rs_result-match = abap_false.
        rs_result-lstate = zif_abapgit_definitions=>c_state-modified.
        rs_result-rstate = zif_abapgit_definitions=>c_state-modified.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD build_new_local.

    " Item
    rs_result-obj_type = is_local-item-obj_type.
    rs_result-obj_name = is_local-item-obj_name.
    rs_result-package  = is_local-item-devclass.

    " File
    rs_result-path     = is_local-file-path.
    rs_result-filename = is_local-file-filename.

    " Match
    rs_result-match    = abap_false.
    rs_result-lstate   = zif_abapgit_definitions=>c_state-added.

  ENDMETHOD.


  METHOD build_new_remote.

    DATA: ls_item     LIKE LINE OF it_items,
          ls_file_sig LIKE LINE OF it_state.

    " Common and default part
    rs_result-path     = is_remote-path.
    rs_result-filename = is_remote-filename.
    rs_result-match    = abap_false.
    rs_result-rstate   = zif_abapgit_definitions=>c_state-added.

    identify_object( EXPORTING iv_filename = is_remote-filename
                               iv_path     = is_remote-path
                               iv_devclass = iv_devclass
                               io_dot      = io_dot
                     IMPORTING es_item     = ls_item ).

    " Check if in item index + get package
    READ TABLE it_items INTO ls_item
      WITH KEY obj_type = ls_item-obj_type obj_name = ls_item-obj_name
      BINARY SEARCH.

    IF sy-subrc = 0.

      " Completely new (xml, abap) and new file in an existing object
      rs_result-obj_type = ls_item-obj_type.
      rs_result-obj_name = ls_item-obj_name.
      rs_result-package  = ls_item-devclass.

      READ TABLE it_state INTO ls_file_sig
        WITH KEY path = is_remote-path filename = is_remote-filename
        BINARY SEARCH.

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
        IF ls_item-devclass IS INITIAL OR ls_item-devclass = iv_devclass.
          rs_result-match  = abap_false.
          rs_result-lstate = zif_abapgit_definitions=>c_state-deleted.
        ENDIF.
      ENDIF.

    ELSE. " Completely unknown file, probably non-abapgit
      ASSERT 1 = 1. " No action, just follow defaults
    ENDIF.

  ENDMETHOD.


  METHOD calculate_status.

    DATA: lt_remote       LIKE it_remote,
          lt_items        TYPE zif_abapgit_definitions=>ty_items_tt,
          ls_item         LIKE LINE OF lt_items,
          lv_is_xml       TYPE abap_bool,
          lv_sub_fetched  TYPE abap_bool,
          lt_sub_packages TYPE zif_abapgit_sap_package=>ty_devclass_tt,
          lt_items_idx    TYPE zif_abapgit_definitions=>ty_items_ts,
          lt_state_idx    TYPE zif_abapgit_definitions=>ty_file_signatures_ts. " Sorted by path+filename

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF it_remote,
                   <ls_result> LIKE LINE OF rt_results,
                   <ls_local>  LIKE LINE OF it_local.


    lt_state_idx = it_cur_state. " Force sort it
    lt_remote    = it_remote.
    SORT lt_remote BY path filename.

    " Process local files and new local files
    LOOP AT it_local ASSIGNING <ls_local>.
      APPEND INITIAL LINE TO rt_results ASSIGNING <ls_result>.
      IF <ls_local>-item IS NOT INITIAL.
        APPEND <ls_local>-item TO lt_items. " Collect for item index
      ENDIF.

      READ TABLE lt_remote ASSIGNING <ls_remote>
        WITH KEY path = <ls_local>-file-path filename = <ls_local>-file-filename
        BINARY SEARCH.
      IF sy-subrc = 0.  " Exist local and remote
        <ls_result> = build_existing(
          is_local  = <ls_local>
          is_remote = <ls_remote>
          it_state  = lt_state_idx ).
        ASSERT <ls_remote>-sha1 IS NOT INITIAL.
        CLEAR <ls_remote>-sha1. " Mark as processed
      ELSE.             " Only L exists
        <ls_result> = build_new_local( <ls_local> ).
      ENDIF.
      <ls_result>-inactive = <ls_local>-item-inactive.
    ENDLOOP.

    " Complete item index for unmarked remote files
    LOOP AT lt_remote ASSIGNING <ls_remote> WHERE sha1 IS NOT INITIAL.
      identify_object( EXPORTING iv_filename = <ls_remote>-filename
                                 iv_path     = <ls_remote>-path
                                 io_dot      = io_dot
                                 iv_devclass = iv_devclass
                       IMPORTING es_item     = ls_item
                                 ev_is_xml   = lv_is_xml ).

      CHECK lv_is_xml = abap_true. " Skip all but obj definitions

      ls_item-devclass = get_object_package(
        iv_object   = ls_item-obj_type
        iv_obj_name = ls_item-obj_name ).

      IF NOT ls_item-devclass IS INITIAL AND iv_devclass <> ls_item-devclass.
        IF lv_sub_fetched = abap_false.
          lt_sub_packages = zcl_abapgit_factory=>get_sap_package( iv_devclass )->list_subpackages( ).
          lv_sub_fetched = abap_true.
          SORT lt_sub_packages BY table_line. "Optimize Read Access
        ENDIF.
* make sure the package is under the repo main package
        READ TABLE lt_sub_packages TRANSPORTING NO FIELDS
          WITH KEY table_line = ls_item-devclass
          BINARY SEARCH.
        IF sy-subrc <> 0.
          CLEAR ls_item-devclass.
        ENDIF.
      ENDIF.

      APPEND ls_item TO lt_items.
    ENDLOOP.

    SORT lt_items DESCENDING. " Default key - type, name, pkg, inactive
    DELETE ADJACENT DUPLICATES FROM lt_items COMPARING obj_type obj_name devclass.
    lt_items_idx = lt_items. " Self protection + UNIQUE records assertion

    " Process new remote files (marked above with empty SHA1)
    LOOP AT lt_remote ASSIGNING <ls_remote> WHERE sha1 IS NOT INITIAL.
      APPEND INITIAL LINE TO rt_results ASSIGNING <ls_result>.
      <ls_result> = build_new_remote( iv_devclass = iv_devclass
                                      io_dot      = io_dot
                                      is_remote   = <ls_remote>
                                      it_items    = lt_items_idx
                                      it_state    = lt_state_idx ).
    ENDLOOP.

    SORT rt_results BY
      obj_type ASCENDING
      obj_name ASCENDING
      filename ASCENDING.

  ENDMETHOD.


  METHOD identify_object.

    DATA: lv_name TYPE tadir-obj_name,
          lv_type TYPE string,
          lv_ext  TYPE string.

    " Guess object type and name
    SPLIT to_upper( iv_filename ) AT '.' INTO lv_name lv_type lv_ext.

    " Handle namespaces
    REPLACE ALL OCCURRENCES OF '#' IN lv_name WITH '/'.
    REPLACE ALL OCCURRENCES OF '#' IN lv_type WITH '/'.
    REPLACE ALL OCCURRENCES OF '#' IN lv_ext WITH '/'.

    " Try to get a unique package name for DEVC by using the path
    IF lv_type = 'DEVC'.
      ASSERT lv_name = 'PACKAGE'.
      lv_name = zcl_abapgit_folder_logic=>get_instance( )->path_to_package(
        iv_top                  = iv_devclass
        io_dot                  = io_dot
        iv_create_if_not_exists = abap_false
        iv_path                 = iv_path ).
    ENDIF.

    CLEAR es_item.
    es_item-obj_type = lv_type.
    es_item-obj_name = lv_name.
    ev_is_xml        = boolc( lv_ext = 'XML' AND strlen( lv_type ) = 4 ).

  ENDMETHOD.


  METHOD run_checks.

    DATA: lv_path         TYPE string,
          ls_item         TYPE zif_abapgit_definitions=>ty_item,
          ls_file         TYPE zif_abapgit_definitions=>ty_file_signature,
          lt_res_sort     LIKE it_results,
          lt_item_idx     LIKE it_results,
          lo_folder_logic TYPE REF TO zcl_abapgit_folder_logic.

    FIELD-SYMBOLS: <ls_res1> LIKE LINE OF it_results,
                   <ls_res2> LIKE LINE OF it_results.


    IF ii_log IS INITIAL.
* huh?
      RETURN.
    ENDIF.

    " Collect object indexe
    lt_res_sort = it_results.
    SORT lt_res_sort BY obj_type ASCENDING obj_name ASCENDING.

    LOOP AT it_results ASSIGNING <ls_res1> WHERE NOT obj_type IS INITIAL.
      IF NOT ( <ls_res1>-obj_type = ls_item-obj_type
          AND <ls_res1>-obj_name = ls_item-obj_name ).
        APPEND INITIAL LINE TO lt_item_idx ASSIGNING <ls_res2>.
        <ls_res2>-obj_type = <ls_res1>-obj_type.
        <ls_res2>-obj_name = <ls_res1>-obj_name.
        <ls_res2>-path     = <ls_res1>-path.
        MOVE-CORRESPONDING <ls_res1> TO ls_item.
      ENDIF.
    ENDLOOP.

    " Check files for one object is in the same folder

    LOOP AT it_results ASSIGNING <ls_res1> WHERE NOT obj_type IS INITIAL AND obj_type <> 'DEVC'.
      READ TABLE lt_item_idx ASSIGNING <ls_res2>
        WITH KEY obj_type = <ls_res1>-obj_type obj_name = <ls_res1>-obj_name
        BINARY SEARCH. " Sorted above

      IF sy-subrc <> 0 OR <ls_res1>-path <> <ls_res2>-path. " All paths are same
        ii_log->add( iv_msg = |Files for object { <ls_res1>-obj_type } {
                       <ls_res1>-obj_name } are not placed in the same folder|
                     iv_type = 'W'
                     iv_rc    = '1' ) ##no_text.
      ENDIF.
    ENDLOOP.

    " Check that objects are created in package corresponding to folder
    lo_folder_logic = zcl_abapgit_folder_logic=>get_instance( ).
    LOOP AT it_results ASSIGNING <ls_res1>
        WHERE NOT package IS INITIAL AND NOT path IS INITIAL.
      lv_path = lo_folder_logic->package_to_path(
        iv_top     = iv_top
        io_dot     = io_dot
        iv_package = <ls_res1>-package ).
      IF lv_path <> <ls_res1>-path.
        ii_log->add( iv_msg = |Package and path does not match for object, {
                       <ls_res1>-obj_type } { <ls_res1>-obj_name }|
                     iv_type = 'W'
                     iv_rc    = '2' ) ##no_text.
      ENDIF.
    ENDLOOP.

    " Check for multiple files with same filename
    SORT lt_res_sort BY filename ASCENDING.

    LOOP AT lt_res_sort ASSIGNING <ls_res1> WHERE obj_type <> 'DEVC'.
      IF <ls_res1>-filename IS NOT INITIAL AND <ls_res1>-filename = ls_file-filename.
        ii_log->add( iv_msg  = |Multiple files with same filename, { <ls_res1>-filename }|
                     iv_type = 'W'
                     iv_rc   = '3' ) ##no_text.
      ENDIF.

      IF <ls_res1>-filename IS INITIAL.
        ii_log->add( iv_msg  = |Filename is empty for object { <ls_res1>-obj_type } { <ls_res1>-obj_name }|
                     iv_type = 'W'
                     iv_rc   = '4' ) ##no_text.
      ENDIF.

      MOVE-CORRESPONDING <ls_res1> TO ls_file.
    ENDLOOP.

  ENDMETHOD.


  METHOD status.

    DATA: lv_index       LIKE sy-tabix,
          lo_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit,
          lt_local       TYPE zif_abapgit_definitions=>ty_files_item_tt.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF rt_results.

    lt_local = io_repo->get_files_local( ii_log = ii_log ).

    IF lines( lt_local ) <= 2.
      " Less equal two means that we have only the .abapgit.xml and the package in
      " our local repository. In this case we have to update our local .abapgit.xml
      " from the remote one. Otherwise we get errors when e.g. the folder starting
      " folder is different.
      io_repo->find_remote_dot_abapgit( ).
    ENDIF.

    rt_results = calculate_status(
      iv_devclass  = io_repo->get_package( )
      io_dot       = io_repo->get_dot_abapgit( )
      it_local     = io_repo->get_files_local( ii_log = ii_log )
      it_remote    = io_repo->get_files_remote( )
      it_cur_state = io_repo->get_local_checksums_per_file( ) ).

    lo_dot_abapgit = io_repo->get_dot_abapgit( ).

    " Remove ignored files, fix .abapgit
    LOOP AT rt_results ASSIGNING <ls_result>.
      lv_index = sy-tabix.

      IF lo_dot_abapgit->is_ignored(
          iv_path     = <ls_result>-path
          iv_filename = <ls_result>-filename ) = abap_true.
        DELETE rt_results INDEX lv_index.
      ENDIF.
    ENDLOOP.

    run_checks(
      ii_log     = ii_log
      it_results = rt_results
      io_dot     = lo_dot_abapgit
      iv_top     = io_repo->get_package( ) ).

  ENDMETHOD.

  METHOD get_object_package.
    DATA: lv_name TYPE devclass,
          lo_package     TYPE REF TO zif_abapgit_sap_package.

    rv_devclass = zcl_abapgit_factory=>get_tadir( )->get_object_package(
      iv_object   = iv_object
      iv_obj_name = iv_obj_name ).
    IF rv_devclass IS INITIAL AND iv_object = 'DEVC' AND iv_obj_name(1) = '$'.
      " local packages usually have no tadir entry
      lv_name = iv_obj_name.
      lo_package = zcl_abapgit_factory=>get_sap_package( lv_name ).
      IF lo_package->exists(  ) = abap_true.
        rv_devclass = lv_name.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
