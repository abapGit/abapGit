CLASS zcl_abapgit_repo_online DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_repo
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_git_operations .

    ALIASES create_branch
      FOR zif_abapgit_git_operations~create_branch .
    ALIASES push
      FOR zif_abapgit_git_operations~push .

    METHODS constructor
      IMPORTING
        !is_data TYPE zif_abapgit_persistence=>ty_repo
      RAISING
        zcx_abapgit_exception .
    METHODS get_url
      RETURNING
        VALUE(rv_url) TYPE zif_abapgit_persistence=>ty_repo-url .
    METHODS get_branch_name
      RETURNING
        VALUE(rv_name) TYPE zif_abapgit_persistence=>ty_repo-branch_name .
    METHODS set_url
      IMPORTING
        !iv_url TYPE zif_abapgit_persistence=>ty_repo-url
      RAISING
        zcx_abapgit_exception .
    METHODS set_branch_name
      IMPORTING
        !iv_branch_name TYPE zif_abapgit_persistence=>ty_repo-branch_name
      RAISING
        zcx_abapgit_exception .
    METHODS get_sha1_remote
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .
    METHODS get_objects
      RETURNING
        VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception .
    METHODS status
      IMPORTING
        !io_log           TYPE REF TO zcl_abapgit_log OPTIONAL
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
    METHODS get_unnecessary_local_objs
      RETURNING
        VALUE(rt_unnecessary_local_objects) TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .

    METHODS deserialize
        REDEFINITION .
    METHODS get_files_remote
        REDEFINITION .
    METHODS rebuild_local_checksums
        REDEFINITION .
    METHODS refresh
        REDEFINITION .
  PRIVATE SECTION.

    DATA mt_objects TYPE zif_abapgit_definitions=>ty_objects_tt .
    DATA mv_branch TYPE zif_abapgit_definitions=>ty_sha1 .
    DATA mv_initialized TYPE abap_bool .
    DATA mt_status TYPE zif_abapgit_definitions=>ty_results_tt .

    METHODS reset_status .
    METHODS initialize
      RAISING
        zcx_abapgit_exception .
    METHODS handle_stage_ignore
      IMPORTING
        !io_stage TYPE REF TO zcl_abapgit_stage
      RAISING
        zcx_abapgit_exception .
    METHODS set_objects
      IMPORTING
        !it_objects TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_REPO_ONLINE IMPLEMENTATION.


  METHOD constructor.

    super->constructor( is_data ).

    mv_initialized = abap_false.

  ENDMETHOD.                    "constructor


  METHOD deserialize.

    initialize( ).

    super->deserialize( is_checks ).

    reset_status( ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.                    "deserialize


  METHOD get_branch_name.
    rv_name = ms_data-branch_name.
  ENDMETHOD.                    "get_branch_name


  METHOD get_files_remote.
    initialize( ).

    rt_files = mt_remote.
  ENDMETHOD.                    "get_files


  METHOD get_objects.
    initialize( ).

    rt_objects = mt_objects.
  ENDMETHOD.                    "get_objects


  METHOD get_sha1_remote.
    initialize( ).

    rv_sha1 = mv_branch.
  ENDMETHOD.                    "get_sha1_remote


  METHOD get_unnecessary_local_objs.

    DATA: lt_tadir        TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lt_tadir_unique TYPE HASHED TABLE OF zif_abapgit_definitions=>ty_tadir
                               WITH UNIQUE KEY pgmid object obj_name,
          lt_local        TYPE zif_abapgit_definitions=>ty_files_item_tt,
          lt_remote       TYPE zif_abapgit_definitions=>ty_files_tt,
          lt_status       TYPE zif_abapgit_definitions=>ty_results_tt,
          lv_package      TYPE zif_abapgit_persistence=>ty_repo-package.

    FIELD-SYMBOLS: <ls_status> TYPE zif_abapgit_definitions=>ty_result,
                   <ls_tadir>  TYPE zif_abapgit_definitions=>ty_tadir.


    " delete objects which are added locally but are not in remote repo
    lt_local  = get_files_local( ).
    lt_remote = get_files_remote( ).
    lt_status = status( ).

    lv_package = get_package( ).
    lt_tadir = zcl_abapgit_factory=>get_tadir( )->read( lv_package ).
    SORT lt_tadir BY pgmid ASCENDING object ASCENDING obj_name ASCENDING devclass ASCENDING.

    LOOP AT lt_status ASSIGNING <ls_status>
                      WHERE lstate = zif_abapgit_definitions=>gc_state-added.

      READ TABLE lt_tadir ASSIGNING <ls_tadir>
                          WITH KEY pgmid    = 'R3TR'
                                   object   = <ls_status>-obj_type
                                   obj_name = <ls_status>-obj_name
                                   devclass = <ls_status>-package
                          BINARY SEARCH.
      IF sy-subrc <> 0.
* skip objects that does not exist locally
        CONTINUE.
      ENDIF.

      INSERT <ls_tadir> INTO TABLE lt_tadir_unique.

    ENDLOOP.

    rt_unnecessary_local_objects = lt_tadir_unique.

  ENDMETHOD.


  METHOD get_url.
    rv_url = ms_data-url.
  ENDMETHOD.                    "get_url


  METHOD handle_stage_ignore.

    DATA: lv_add         TYPE abap_bool,
          lo_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit,
          lt_stage       TYPE zcl_abapgit_stage=>ty_stage_tt.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF lt_stage.


    lo_dot_abapgit = get_dot_abapgit( ).
    lt_stage = io_stage->get_all( ).
    LOOP AT lt_stage ASSIGNING <ls_stage> WHERE method = zcl_abapgit_stage=>c_method-ignore.

      lo_dot_abapgit->add_ignore(
        iv_path     = <ls_stage>-file-path
        iv_filename = <ls_stage>-file-filename ).

      " remove it from the staging object, as the action is handled here
      io_stage->reset( iv_path     = <ls_stage>-file-path
                       iv_filename = <ls_stage>-file-filename ).

      lv_add = abap_true.

    ENDLOOP.

    IF lv_add = abap_true.
      io_stage->add(
        iv_path     = zif_abapgit_definitions=>gc_root_dir
        iv_filename = zif_abapgit_definitions=>gc_dot_abapgit
        iv_data     = lo_dot_abapgit->serialize( ) ).

      set_dot_abapgit( lo_dot_abapgit ).
    ENDIF.

  ENDMETHOD.


  METHOD initialize.
    IF mv_initialized = abap_false.
      refresh( ).
    ENDIF.
  ENDMETHOD.


  METHOD rebuild_local_checksums. "REMOTE

    DATA: lt_remote    TYPE zif_abapgit_definitions=>ty_files_tt,
          lt_local     TYPE zif_abapgit_definitions=>ty_files_item_tt,
          ls_last_item TYPE zif_abapgit_definitions=>ty_item,
          lt_checksums TYPE zif_abapgit_persistence=>ty_local_checksum_tt.

    FIELD-SYMBOLS: <ls_checksum> LIKE LINE OF lt_checksums,
                   <ls_file_sig> LIKE LINE OF <ls_checksum>-files,
                   <ls_remote>   LIKE LINE OF lt_remote,
                   <ls_local>    LIKE LINE OF lt_local.

    lt_remote = get_files_remote( ).
    lt_local  = get_files_local( ).

    DELETE lt_local " Remove non-code related files except .abapgit
      WHERE item IS INITIAL
      AND NOT ( file-path     = zif_abapgit_definitions=>gc_root_dir
      AND       file-filename = zif_abapgit_definitions=>gc_dot_abapgit ).

    SORT lt_local BY item.
    SORT lt_remote BY path filename.

    LOOP AT lt_local ASSIGNING <ls_local>.
      IF ls_last_item <> <ls_local>-item OR sy-tabix = 1. " First or New item reached ?
        APPEND INITIAL LINE TO lt_checksums ASSIGNING <ls_checksum>.
        <ls_checksum>-item = <ls_local>-item.
        ls_last_item       = <ls_local>-item.
      ENDIF.

      READ TABLE lt_remote ASSIGNING <ls_remote>
        WITH KEY path = <ls_local>-file-path filename = <ls_local>-file-filename
        BINARY SEARCH.
      CHECK sy-subrc = 0.  " Ignore new ones

      APPEND INITIAL LINE TO <ls_checksum>-files ASSIGNING <ls_file_sig>.
      MOVE-CORRESPONDING <ls_local>-file TO <ls_file_sig>.

      " If hashes are equal -> local sha1 is OK
      " Else if R-branch is ahead  -> assume changes were remote, state - local sha1
      "      Else (branches equal) -> assume changes were local, state - remote sha1
      IF <ls_local>-file-sha1 <> <ls_remote>-sha1.
        <ls_file_sig>-sha1 = <ls_remote>-sha1.
      ENDIF.
    ENDLOOP.

    set( it_checksums = lt_checksums ).
    reset_status( ).

  ENDMETHOD.  " rebuild_local_checksums.


  METHOD refresh.

    DATA: lo_progress  TYPE REF TO zcl_abapgit_progress,
          lx_exception TYPE REF TO zcx_abapgit_exception.

    super->refresh( iv_drop_cache ).
    reset_status( ).

    CREATE OBJECT lo_progress
      EXPORTING
        iv_total = 1.

    lo_progress->show( iv_current = 1
                       iv_text    = 'Fetch remote files' ) ##NO_TEXT.

    zcl_abapgit_git_porcelain=>pull(
      EXPORTING
        iv_url         = get_url( )
        iv_branch_name = get_branch_name( )
      IMPORTING
        et_files       = mt_remote
        et_objects     = mt_objects
        ev_branch      = mv_branch ).

    mv_initialized = abap_true.

  ENDMETHOD.                    "refresh


  METHOD reset_status.
    CLEAR mt_status.
  ENDMETHOD.  " reset_status.


  METHOD set_branch_name.

    IF ms_data-local_settings-write_protected = abap_true.
      zcx_abapgit_exception=>raise( 'Cannot switch branch. Local code is write-protected by repo config' ).
    ENDIF.

    mv_initialized = abap_false.
    set( iv_branch_name = iv_branch_name ).

  ENDMETHOD.


  METHOD set_objects.
    mt_objects = it_objects.
  ENDMETHOD.


  METHOD set_url.

    IF ms_data-local_settings-write_protected = abap_true.
      zcx_abapgit_exception=>raise( 'Cannot change URL. Local code is write-protected by repo config' ).
    ENDIF.

    mv_initialized = abap_false.
    set( iv_url = iv_url ).

  ENDMETHOD.


  METHOD status.

    initialize( ).

    IF lines( mt_status ) = 0.
      mt_status = zcl_abapgit_file_status=>status( io_repo = me
                                                   io_log  = io_log ).
    ENDIF.
    rt_results = mt_status.

  ENDMETHOD.                    "status


  METHOD zif_abapgit_git_operations~create_branch.

    DATA: lv_sha1 TYPE zif_abapgit_definitions=>ty_sha1.

    ASSERT iv_name CP 'refs/heads/+*'.

    IF iv_from IS INITIAL.
      lv_sha1 = get_sha1_remote( ).
    ELSE.
      lv_sha1 = iv_from.
    ENDIF.

    zcl_abapgit_git_porcelain=>create_branch(
      iv_url  = get_url( )
      iv_name = iv_name
      iv_from = lv_sha1 ).

    " automatically switch to new branch
    set_branch_name( iv_name ).

  ENDMETHOD.


  METHOD zif_abapgit_git_operations~push.

* assumption: PUSH is done on top of the currently selected branch

    DATA: lv_branch        TYPE zif_abapgit_definitions=>ty_sha1,
          lt_updated_files TYPE zif_abapgit_definitions=>ty_file_signatures_tt,
          lt_new_files     TYPE zif_abapgit_definitions=>ty_files_tt,
          lt_new_objects   TYPE zif_abapgit_definitions=>ty_objects_tt,
          lv_text          TYPE string.


    IF ms_data-branch_name CP 'refs/tags*'.
      lv_text = |You're working on a tag. Currently it's not |
             && |possible to push on tags. Consider creating a branch instead|.
      zcx_abapgit_exception=>raise( lv_text ).
    ENDIF.

    IF ms_data-local_settings-block_commit = abap_true
        AND mv_code_inspector_successful = abap_false.
      zcx_abapgit_exception=>raise( |A successful code inspection is required| ).
    ENDIF.

    handle_stage_ignore( io_stage ).

    zcl_abapgit_git_porcelain=>push(
      EXPORTING
        is_comment       = is_comment
        io_stage         = io_stage
        iv_branch_name   = get_branch_name( )
        iv_url           = get_url( )
        iv_parent        = get_sha1_remote( )
        it_old_objects   = get_objects( )
      IMPORTING
        ev_branch        = lv_branch
        et_new_files     = lt_new_files
        et_new_objects   = lt_new_objects
        et_updated_files = lt_updated_files ).

    set_objects( lt_new_objects ).
    set_files_remote( lt_new_files ).

    mv_branch = lv_branch.

    update_local_checksums( lt_updated_files ).

    CLEAR: mv_code_inspector_successful.

  ENDMETHOD.
ENDCLASS.
