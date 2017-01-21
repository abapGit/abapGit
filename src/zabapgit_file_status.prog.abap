*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_FILE_STATUS
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_file_status DEFINITION
*----------------------------------------------------------------------*
CLASS ltcl_file_status DEFINITION DEFERRED.

CLASS lcl_file_status DEFINITION FINAL
  FRIENDS ltcl_file_status.

  PUBLIC SECTION.

    CLASS-METHODS status
      IMPORTING io_repo           TYPE REF TO lcl_repo
                io_log            TYPE REF TO lcl_log OPTIONAL
      RETURNING VALUE(rt_results) TYPE ty_results_tt
      RAISING   lcx_exception.

  PRIVATE SECTION.

    CLASS-METHODS calculate_status
      IMPORTING iv_devclass        TYPE devclass
                it_local           TYPE ty_files_item_tt
                it_remote          TYPE ty_files_tt
                it_cur_state       TYPE ty_file_signatures_tt
      RETURNING VALUE(rt_results)  TYPE ty_results_tt.

    CLASS-METHODS:
      build_existing
        IMPORTING is_local         TYPE ty_file_item
                  is_remote        TYPE ty_file
                  it_state         TYPE ty_file_signatures_ts
        RETURNING VALUE(rs_result) TYPE ty_result,
      build_new_local
        IMPORTING is_local         TYPE ty_file_item
        RETURNING VALUE(rs_result) TYPE ty_result,
      build_new_remote
        IMPORTING iv_devclass      TYPE devclass
                  is_remote        TYPE ty_file
                  it_items         TYPE ty_items_ts
                  it_state         TYPE ty_file_signatures_ts
        RETURNING VALUE(rs_result) TYPE ty_result,
      identify_object
        IMPORTING iv_filename      TYPE string
        EXPORTING es_item          TYPE ty_item
                  ev_is_xml        TYPE abap_bool.

ENDCLASS.                    "lcl_file_status DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_file_status IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_file_status IMPLEMENTATION.

  METHOD status.

    DATA: lv_index       LIKE sy-tabix,
          lo_dot_abapgit TYPE REF TO lcl_dot_abapgit.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF rt_results.


    rt_results = calculate_status(

      iv_devclass  = io_repo->get_package( )
      it_local     = io_repo->get_files_local( io_log = io_log )
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

    lcl_sap_package=>check(
      io_log     = io_log
      it_results = rt_results
      iv_start   = lo_dot_abapgit->get_starting_folder( )
      iv_top     = io_repo->get_package( ) ).

  ENDMETHOD.  "status

  METHOD calculate_status.

    DATA: lt_remote    LIKE it_remote,
          lt_items     TYPE ty_items_tt,
          ls_item      LIKE LINE OF lt_items,
          lv_is_xml    TYPE abap_bool,
          lt_items_idx TYPE ty_items_ts,
          lt_state_idx TYPE ty_file_signatures_ts. " Sorted by path+filename

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
        <ls_result> = build_new_local( is_local = <ls_local> ).
      ENDIF.
    ENDLOOP.

    " Complete item index for unmarked remote files
    LOOP AT lt_remote ASSIGNING <ls_remote> WHERE sha1 IS NOT INITIAL.
      identify_object( EXPORTING iv_filename = <ls_remote>-filename
                       IMPORTING es_item     = ls_item
                                 ev_is_xml   = lv_is_xml ).

      CHECK lv_is_xml = abap_true. " Skip all but obj definitions

      ls_item-devclass = lcl_tadir=>get_object_package(
                           iv_object   = ls_item-obj_type
                           iv_obj_name = ls_item-obj_name ).
      APPEND ls_item TO lt_items.
    ENDLOOP.

    SORT lt_items. " Default key - type, name, pkg
    DELETE ADJACENT DUPLICATES FROM lt_items.
    lt_items_idx = lt_items. " Self protection + UNIQUE records assertion

    " Process new remote files (marked above with empty SHA1)
    LOOP AT lt_remote ASSIGNING <ls_remote> WHERE sha1 IS NOT INITIAL.
      APPEND INITIAL LINE TO rt_results ASSIGNING <ls_result>.
      <ls_result> = build_new_remote( iv_devclass = iv_devclass
                                      is_remote   = <ls_remote>
                                      it_items    = lt_items_idx
                                      it_state    = lt_state_idx ).
    ENDLOOP.

    SORT rt_results BY
      obj_type ASCENDING
      obj_name ASCENDING
      filename ASCENDING.

  ENDMETHOD.  "calculate_status.

  METHOD identify_object.

    DATA: lv_name   TYPE tadir-obj_name,
          lv_type   TYPE string,
          lv_ext    TYPE string.

    " Guess object type and name
    SPLIT to_upper( iv_filename ) AT '.' INTO lv_name lv_type lv_ext.

    " Handle namespaces
    REPLACE ALL OCCURRENCES OF '#' IN lv_name WITH '/'.

    CLEAR es_item.
    es_item-obj_type = lv_type.
    es_item-obj_name = lv_name.
    ev_is_xml        = boolc( lv_ext = 'XML' AND strlen( lv_type ) = 4 ).

  ENDMETHOD.  "identify_object.

  METHOD build_existing.

    DATA: ls_file_sig LIKE LINE OF it_state.

    " Item
    rs_result-obj_type = is_local-item-obj_type.
    rs_result-obj_name = is_local-item-obj_name.
    rs_result-package  = is_local-item-devclass.

    " File
    rs_result-path     = is_local-file-path.
    rs_result-filename = is_local-file-filename.

    " Match against current state
    READ TABLE it_state INTO ls_file_sig
      WITH KEY path = is_local-file-path
      filename = is_local-file-filename
      BINARY SEARCH.

    IF sy-subrc = 0.
      IF ls_file_sig-sha1 <> is_local-file-sha1.
        rs_result-lstate = gc_state-modified.
      ENDIF.
      IF ls_file_sig-sha1 <> is_remote-sha1.
        rs_result-rstate = gc_state-modified.
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
        rs_result-lstate = gc_state-modified.
        rs_result-rstate = gc_state-modified.
      ENDIF.
    ENDIF.

  ENDMETHOD.  "build_existing

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
    rs_result-lstate   = gc_state-added.

  ENDMETHOD.  "build_new_local

  METHOD build_new_remote.

    DATA: ls_item     LIKE LINE OF it_items,
          ls_file_sig LIKE LINE OF it_state.

    " Common and default part
    rs_result-path     = is_remote-path.
    rs_result-filename = is_remote-filename.
    rs_result-match    = abap_false.
    rs_result-rstate   = gc_state-added.

    identify_object( EXPORTING iv_filename = is_remote-filename
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
          rs_result-rstate = gc_state-modified.
        ENDIF.

        " Item is in state and in cache but with no package - it was deleted
        " OR devclass is the same as repo package (see #532)
        IF ls_item-devclass IS INITIAL OR ls_item-devclass = iv_devclass.
          rs_result-match  = abap_false.
          rs_result-lstate = gc_state-deleted.
        ENDIF.
      ENDIF.

    ELSE. " Completely unknown file, probably non-abapgit
      ASSERT 1 = 1. " No action, just follow defaults
    ENDIF.

  ENDMETHOD.  "build_new_remote

ENDCLASS.                    "lcl_file_status IMPLEMENTATION