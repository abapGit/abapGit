CLASS zcl_abapgit_repo_content_list DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_repo TYPE REF TO zcl_abapgit_repo.

    METHODS list
      IMPORTING iv_path              TYPE string
                iv_by_folders        TYPE abap_bool OPTIONAL
                iv_changes_only      TYPE abap_bool OPTIONAL
                iv_transports        TYPE abap_bool OPTIONAL
      RETURNING VALUE(rt_repo_items) TYPE zif_abapgit_definitions=>ty_repo_item_tt
      RAISING   zcx_abapgit_exception.

    METHODS get_log
      RETURNING VALUE(ri_log) TYPE REF TO zif_abapgit_log.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_sortkey,
                 default    TYPE i VALUE 9999,
                 parent_dir TYPE i VALUE 0,
                 dir        TYPE i VALUE 1,
                 orphan     TYPE i VALUE 2,
                 changed    TYPE i VALUE 3,
                 inactive   TYPE i VALUE 4,
               END OF c_sortkey.

    DATA: mo_repo TYPE REF TO zcl_abapgit_repo,
          mi_log  TYPE REF TO zif_abapgit_log.

    METHODS build_repo_items_local_only
      RETURNING VALUE(rt_repo_items) TYPE zif_abapgit_definitions=>ty_repo_item_tt
      RAISING   zcx_abapgit_exception.

    METHODS build_repo_items_with_remote
      RETURNING VALUE(rt_repo_items) TYPE zif_abapgit_definitions=>ty_repo_item_tt
      RAISING   zcx_abapgit_exception.

    METHODS build_folders
      IMPORTING iv_cur_dir    TYPE string
      CHANGING  ct_repo_items TYPE zif_abapgit_definitions=>ty_repo_item_tt
      RAISING   zcx_abapgit_exception.

    METHODS determine_transports
      CHANGING ct_repo_items TYPE zif_abapgit_definitions=>ty_repo_item_tt.

    METHODS filter_changes
      CHANGING ct_repo_items TYPE zif_abapgit_definitions=>ty_repo_item_tt.

    METHODS check_repo_size
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_repo_content_list IMPLEMENTATION.


  METHOD build_folders.

    DATA: lv_index    TYPE i,
          lt_subitems LIKE ct_repo_items,
          ls_subitem  LIKE LINE OF ct_repo_items,
          ls_folder   LIKE LINE OF ct_repo_items.

    DATA lo_state TYPE REF TO zcl_abapgit_item_state.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF ct_repo_items.


    LOOP AT ct_repo_items ASSIGNING <ls_item>.
      lv_index = sy-tabix.
      CHECK <ls_item>-path <> iv_cur_dir. " files in target dir - just leave them be

      IF zcl_abapgit_path=>is_subdir( iv_path = <ls_item>-path
                                      iv_parent = iv_cur_dir ) = abap_true.
        ls_subitem-changes = <ls_item>-changes.
        ls_subitem-path    = <ls_item>-path.
        ls_subitem-lstate  = <ls_item>-lstate.
        ls_subitem-rstate  = <ls_item>-rstate.
        APPEND ls_subitem TO lt_subitems.
      ENDIF.

      DELETE ct_repo_items INDEX lv_index.
    ENDLOOP.

    SORT lt_subitems BY path ASCENDING.

    LOOP AT lt_subitems ASSIGNING <ls_item>.
      AT NEW path.
        CLEAR ls_folder.
        ls_folder-path    = <ls_item>-path.
        ls_folder-sortkey = c_sortkey-dir. " Directory
        ls_folder-is_dir  = abap_true.
        CREATE OBJECT lo_state.
      ENDAT.

      ls_folder-changes = ls_folder-changes + <ls_item>-changes.
      lo_state->sum_with_repo_item( <ls_item> ).

      AT END OF path.
        ls_folder-lstate = lo_state->local( ).
        ls_folder-rstate = lo_state->remote( ).
        APPEND ls_folder TO ct_repo_items.
      ENDAT.
    ENDLOOP.

  ENDMETHOD.


  METHOD build_repo_items_local_only.

    DATA: lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt,
          ls_item  TYPE zif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS: <ls_repo_item> LIKE LINE OF rt_repo_items,
                   <ls_tadir>     LIKE LINE OF lt_tadir.


    lt_tadir = zcl_abapgit_factory=>get_tadir( )->read(
      iv_package            = mo_repo->get_package( )
      iv_ignore_subpackages = mo_repo->get_local_settings( )-ignore_subpackages
      iv_only_local_objects = mo_repo->get_local_settings( )-only_local_objects
      io_dot                = mo_repo->get_dot_abapgit( ) ).

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      APPEND INITIAL LINE TO rt_repo_items ASSIGNING <ls_repo_item>.
      <ls_repo_item>-obj_type  = <ls_tadir>-object.
      <ls_repo_item>-obj_name  = <ls_tadir>-obj_name.
      <ls_repo_item>-path      = <ls_tadir>-path.
      <ls_repo_item>-srcsystem = <ls_tadir>-srcsystem.
      MOVE-CORRESPONDING <ls_repo_item> TO ls_item.
      <ls_repo_item>-inactive = boolc( zcl_abapgit_objects=>is_active( ls_item ) = abap_false ).
      IF <ls_repo_item>-inactive = abap_true.
        <ls_repo_item>-sortkey = c_sortkey-inactive.
      ELSE.
        <ls_repo_item>-sortkey = c_sortkey-default.      " Default sort key
      ENDIF.

      IF <ls_repo_item>-obj_type IS NOT INITIAL.
        MOVE-CORRESPONDING <ls_repo_item> TO ls_item.
        IF zcl_abapgit_objects=>exists( ls_item ) = abap_true.
          <ls_repo_item>-changed_by = zcl_abapgit_objects=>changed_by( ls_item ).
        ENDIF.
        CLEAR ls_item.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD build_repo_items_with_remote.

    DATA:
      lo_state      TYPE REF TO zcl_abapgit_item_state,
      ls_file       TYPE zif_abapgit_definitions=>ty_repo_file,
      lt_status     TYPE zif_abapgit_definitions=>ty_results_tt,
      ls_item       TYPE zif_abapgit_definitions=>ty_item,
      ls_previous   LIKE ls_item,
      lv_changed_by TYPE string.

    FIELD-SYMBOLS: <ls_status>    LIKE LINE OF lt_status,
                   <ls_repo_item> LIKE LINE OF rt_repo_items.


    lt_status = mo_repo->status( mi_log ).

    LOOP AT lt_status ASSIGNING <ls_status>.
      AT NEW obj_name. "obj_type + obj_name
        APPEND INITIAL LINE TO rt_repo_items ASSIGNING <ls_repo_item>.
        <ls_repo_item>-obj_type  = <ls_status>-obj_type.
        <ls_repo_item>-obj_name  = <ls_status>-obj_name.
        <ls_repo_item>-inactive  = <ls_status>-inactive.
        <ls_repo_item>-sortkey   = c_sortkey-default. " Default sort key
        <ls_repo_item>-changes   = 0.
        <ls_repo_item>-path      = <ls_status>-path.
        <ls_repo_item>-srcsystem = <ls_status>-srcsystem.
        CREATE OBJECT lo_state.
      ENDAT.

      IF <ls_status>-filename IS NOT INITIAL.
        MOVE-CORRESPONDING <ls_status> TO ls_file.
        ls_file-is_changed = boolc( <ls_status>-match = abap_false ). " TODO refactor
        APPEND ls_file TO <ls_repo_item>-files.

        IF <ls_status>-inactive = abap_true AND <ls_repo_item>-sortkey > c_sortkey-changed.
          <ls_repo_item>-sortkey = c_sortkey-inactive.
        ENDIF.

        IF ls_file-is_changed = abap_true.
          <ls_repo_item>-sortkey = c_sortkey-changed. " Changed files
          <ls_repo_item>-changes = <ls_repo_item>-changes + 1.
          lo_state->sum_with_status_item( <ls_status> ).
        ENDIF.
      ENDIF.

      IF <ls_repo_item>-changes > 0 AND <ls_repo_item>-obj_type IS NOT INITIAL.
        MOVE-CORRESPONDING <ls_repo_item> TO ls_item.
        IF ls_previous = ls_item.
          <ls_repo_item>-changed_by = lv_changed_by.
        ELSEIF zcl_abapgit_objects=>exists( ls_item ) = abap_true.
          <ls_repo_item>-changed_by = zcl_abapgit_objects=>changed_by( ls_item ).
          ls_previous = ls_item.
          lv_changed_by = <ls_repo_item>-changed_by.
        ENDIF.
        CLEAR ls_item.
      ENDIF.

      AT END OF obj_name. "obj_type + obj_name
        IF <ls_repo_item>-obj_type IS INITIAL.
          <ls_repo_item>-sortkey = c_sortkey-orphan. "Virtual objects
        ENDIF.
        <ls_repo_item>-lstate = lo_state->local( ).
        <ls_repo_item>-rstate = lo_state->remote( ).
        <ls_repo_item>-packmove = lo_state->is_reassigned( ).
      ENDAT.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_repo_size.

    CONSTANTS lc_new_repo_size TYPE i VALUE 10.

    DATA lt_remote TYPE zif_abapgit_git_definitions=>ty_files_tt.

    lt_remote = mo_repo->get_files_remote( ).

    IF lines( lt_remote ) > lc_new_repo_size.
      " Less files means it's a new repo (with just readme and license, for example) which is ok
      READ TABLE lt_remote TRANSPORTING NO FIELDS
        WITH KEY file_path
        COMPONENTS path     = zif_abapgit_definitions=>c_root_dir
                   filename = zif_abapgit_definitions=>c_dot_abapgit.
      IF sy-subrc <> 0.
        mi_log->add_warning( |Cannot find .abapgit.xml - Is this an abapGit repository?| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.
    mo_repo = io_repo.
    CREATE OBJECT mi_log TYPE zcl_abapgit_log.
  ENDMETHOD.


  METHOD determine_transports.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF ct_repo_items.

    LOOP AT ct_repo_items ASSIGNING <ls_item>.
      ls_item-obj_type = <ls_item>-obj_type.
      ls_item-obj_name = <ls_item>-obj_name.
      TRY.
          <ls_item>-transport = zcl_abapgit_factory=>get_cts_api( )->get_transport_for_object( ls_item ).
        CATCH zcx_abapgit_exception ##NO_HANDLER.
          " Ignore errors related to object check when trying to get transport
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD filter_changes.

    FIELD-SYMBOLS: <ls_item> TYPE zif_abapgit_definitions=>ty_repo_item.

    DELETE ct_repo_items WHERE changes = 0 AND inactive = abap_false.
    LOOP AT ct_repo_items ASSIGNING <ls_item> WHERE inactive = abap_false.
      DELETE <ls_item>-files WHERE is_changed = abap_false.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_log.
    DATA li_repo_log TYPE REF TO zif_abapgit_log.
    DATA lt_repo_msg TYPE zif_abapgit_log=>ty_log_outs.
    DATA lr_repo_msg TYPE REF TO zif_abapgit_log=>ty_log_out.

    ri_log = mi_log.

    "add warning and error messages from repo log
    li_repo_log = mo_repo->get_log( ).
    IF li_repo_log IS BOUND.
      lt_repo_msg = li_repo_log->get_messages( ).
      LOOP AT lt_repo_msg REFERENCE INTO lr_repo_msg WHERE type CA 'EW'.
        CASE lr_repo_msg->type.
          WHEN 'E'.
            ri_log->add_error( lr_repo_msg->text ).
          WHEN 'W'.
            ri_log->add_warning( lr_repo_msg->text ).
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD list.

    mi_log->clear( ).

    IF mo_repo->has_remote_source( ) = abap_true.
      rt_repo_items = build_repo_items_with_remote( ).
      check_repo_size( ).
    ELSE.
      rt_repo_items = build_repo_items_local_only( ).
    ENDIF.

    IF iv_by_folders = abap_true.
      build_folders(
        EXPORTING iv_cur_dir    = iv_path
        CHANGING  ct_repo_items = rt_repo_items ).
    ENDIF.

    IF iv_changes_only = abap_true.
      " There are never changes for offline repositories
      filter_changes( CHANGING ct_repo_items = rt_repo_items ).
    ENDIF.

    IF iv_transports = abap_true.
      determine_transports( CHANGING ct_repo_items = rt_repo_items ).
    ENDIF.

    SORT rt_repo_items BY
      sortkey ASCENDING
      path ASCENDING
      obj_name ASCENDING.

  ENDMETHOD.
ENDCLASS.
