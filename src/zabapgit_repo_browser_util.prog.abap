*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_REPO_BROWSER_UTIL
*&---------------------------------------------------------------------*

CLASS lcl_repo_content_browser DEFINITION FINAL.

  PUBLIC SECTION.

    CONSTANTS: BEGIN OF c_sortkey,
                 default    TYPE i VALUE 9999,
                 parent_dir TYPE i VALUE 0,
                 dir        TYPE i VALUE 1,
                 orphan     TYPE i VALUE 2,
                 changed    TYPE i VALUE 3,
               END OF c_sortkey.

    TYPES: BEGIN OF ty_repo_item,
             obj_type TYPE tadir-object,
             obj_name TYPE tadir-obj_name,
             sortkey  TYPE i,
             path     TYPE string,
             is_dir   TYPE abap_bool,
             changes  TYPE i,
             lstate   TYPE char1,
             rstate   TYPE char1,
             files    TYPE tt_repo_files,
           END OF ty_repo_item.
    TYPES tt_repo_items TYPE STANDARD TABLE OF ty_repo_item WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING io_repo TYPE REF TO lcl_repo.

    METHODS list
      IMPORTING iv_path              TYPE string
                iv_by_folders        TYPE abap_bool
                iv_changes_only      TYPE abap_bool
      RETURNING VALUE(rt_repo_items) TYPE tt_repo_items
      RAISING   lcx_exception.

    METHODS get_log
      RETURNING VALUE(ro_log) TYPE REF TO lcl_log.

  PRIVATE SECTION.
    DATA: mo_repo TYPE REF TO lcl_repo,
          mo_log  TYPE REF TO lcl_log.

    METHODS build_repo_items_offline
      RETURNING VALUE(rt_repo_items) TYPE tt_repo_items
      RAISING   lcx_exception.

    METHODS build_repo_items_online
      RETURNING VALUE(rt_repo_items) TYPE tt_repo_items
      RAISING   lcx_exception.

    METHODS build_folders
      IMPORTING iv_cur_dir    TYPE string
      CHANGING  ct_repo_items TYPE tt_repo_items
      RAISING   lcx_exception.

    METHODS filter_changes
      CHANGING ct_repo_items TYPE tt_repo_items.

ENDCLASS. "lcl_repo_content_browser

DEFINE _reduce_state.
  " &1 - prev, &2 - cur
  IF &1 = &2 OR &2 IS INITIAL.
    ASSERT 1 = 1. " No change
  ELSEIF &1 IS INITIAL.
    &1 = &2.
  ELSE.
    &1 = gc_state-mixed.
  ENDIF.
END-OF-DEFINITION.

CLASS lcl_repo_content_browser IMPLEMENTATION.

  METHOD constructor.
    mo_repo = io_repo.
    CREATE OBJECT mo_log.
  ENDMETHOD.  "constructor

  METHOD get_log.
    ro_log = mo_log.
  ENDMETHOD. "get_log

  METHOD list.

    mo_log->clear( ).

    IF mo_repo->is_offline( ) = abap_true.
      rt_repo_items = build_repo_items_offline( ).
    ELSE.
      rt_repo_items = build_repo_items_online( ).
    ENDIF.

    IF iv_by_folders = abap_true.
      build_folders(
        EXPORTING iv_cur_dir    = iv_path
        CHANGING  ct_repo_items = rt_repo_items ).
    ENDIF.

    IF iv_changes_only = abap_true.
      filter_changes( CHANGING ct_repo_items = rt_repo_items ).
    ENDIF.

    SORT rt_repo_items BY
      sortkey ASCENDING
      obj_type ASCENDING
      obj_name ASCENDING.

  ENDMETHOD.  "list

  METHOD build_folders.

    DATA: lv_index    TYPE i,
          lt_subitems LIKE ct_repo_items,
          ls_subitem  LIKE LINE OF ct_repo_items,
          ls_folder   LIKE LINE OF ct_repo_items.

    FIELD-SYMBOLS <item> LIKE LINE OF ct_repo_items.

    LOOP AT ct_repo_items ASSIGNING <item>.
      lv_index = sy-tabix.
      CHECK <item>-path <> iv_cur_dir. " files in target dir - just leave them be

      IF lcl_path=>is_subdir( iv_path = <item>-path  iv_parent = iv_cur_dir ) = abap_true.
        ls_subitem-changes = <item>-changes.
        ls_subitem-path    = <item>-path.
        ls_subitem-lstate  = <item>-lstate.
        ls_subitem-rstate  = <item>-rstate.
        APPEND ls_subitem TO lt_subitems.
      ENDIF.

      DELETE ct_repo_items INDEX lv_index.
    ENDLOOP.

    SORT lt_subitems BY path ASCENDING.

    LOOP AT lt_subitems ASSIGNING <item>.
      AT NEW path.
        CLEAR ls_folder.
        ls_folder-path    = <item>-path.
        ls_folder-sortkey = c_sortkey-dir. " Directory
        ls_folder-is_dir  = abap_true.
      ENDAT.

      ls_folder-changes = ls_folder-changes + <item>-changes.
      _reduce_state ls_folder-lstate <item>-lstate.
      _reduce_state ls_folder-rstate <item>-rstate.

      AT END OF path.
        APPEND ls_folder TO ct_repo_items.
      ENDAT.
    ENDLOOP.

  ENDMETHOD. "build_folders

  METHOD filter_changes.

    DATA lt_repo_temp LIKE ct_repo_items.

    FIELD-SYMBOLS <item> LIKE LINE OF ct_repo_items.

    LOOP AT ct_repo_items ASSIGNING <item>.
      CHECK <item>-changes > 0.
      APPEND <item> TO lt_repo_temp.
    ENDLOOP.

    IF lines( lt_repo_temp ) > 0. " Prevent showing empty package if no changes, show all
      ct_repo_items = lt_repo_temp.
    ENDIF.

  ENDMETHOD. "filter_changes

  METHOD build_repo_items_offline.

    DATA: lt_tadir TYPE ty_tadir_tt.

    FIELD-SYMBOLS: <ls_repo_item> LIKE LINE OF rt_repo_items,
                   <ls_tadir>     LIKE LINE OF lt_tadir.


* todo, offline projects should have an dot abapgit too
    lt_tadir = lcl_tadir=>read(
      iv_package = mo_repo->get_package( )
      io_dot     = lcl_dot_abapgit=>build_default( sy-langu ) ).

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      APPEND INITIAL LINE TO rt_repo_items ASSIGNING <ls_repo_item>.
      <ls_repo_item>-obj_type = <ls_tadir>-object.
      <ls_repo_item>-obj_name = <ls_tadir>-obj_name.
      <ls_repo_item>-path     = <ls_tadir>-path.
      <ls_repo_item>-sortkey  = c_sortkey-default.      " Default sort key
    ENDLOOP.

  ENDMETHOD.  "build_repo_items_offline

  METHOD build_repo_items_online.

    DATA: lo_repo_online TYPE REF TO lcl_repo_online,
          ls_file        TYPE ty_repo_file,
          lt_status      TYPE ty_results_tt.

    FIELD-SYMBOLS: <status>       LIKE LINE OF lt_status,
                   <ls_repo_item> LIKE LINE OF rt_repo_items.


    lo_repo_online ?= mo_repo.
    lt_status       = lo_repo_online->status( mo_log ).

    LOOP AT lt_status ASSIGNING <status>.
      AT NEW obj_name. "obj_type + obj_name
        APPEND INITIAL LINE TO rt_repo_items ASSIGNING <ls_repo_item>.
        <ls_repo_item>-obj_type = <status>-obj_type.
        <ls_repo_item>-obj_name = <status>-obj_name.
        <ls_repo_item>-sortkey  = c_sortkey-default. " Default sort key
        <ls_repo_item>-changes  = 0.
        <ls_repo_item>-path     = <status>-path.
      ENDAT.

      IF <status>-filename IS NOT INITIAL.
        ls_file-path        = <status>-path.
        ls_file-filename    = <status>-filename.
        ls_file-is_changed  = boolc( <status>-match = abap_false ). " TODO refactor
        ls_file-rstate      = <status>-rstate.
        ls_file-lstate      = <status>-lstate.
        APPEND ls_file TO <ls_repo_item>-files.

        IF ls_file-is_changed = abap_true.
          <ls_repo_item>-sortkey = c_sortkey-changed. " Changed files
          <ls_repo_item>-changes = <ls_repo_item>-changes + 1.
          _reduce_state <ls_repo_item>-lstate ls_file-lstate.
          _reduce_state <ls_repo_item>-rstate ls_file-rstate.
        ENDIF.
      ENDIF.

      AT END OF obj_name. "obj_type + obj_name
        IF <ls_repo_item>-obj_type IS INITIAL.
          <ls_repo_item>-sortkey = c_sortkey-orphan. "Virtual objects
        ENDIF.
      ENDAT.
    ENDLOOP.

  ENDMETHOD. "build_repo_items_online

ENDCLASS. "lcl_repo_content_browser
