*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_REPO_IMPL
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_repo_offline IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_repo_offline IMPLEMENTATION.

  METHOD set_files_remote.

    mt_remote = it_files.

    find_dot_abapgit( ).

  ENDMETHOD.

ENDCLASS.                    "lcl_repo_offline IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_repo_online IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_repo_online IMPLEMENTATION.

  METHOD constructor.

    super->constructor( is_data ).

    mv_initialized = abap_false.

  ENDMETHOD.                    "constructor

  METHOD initialize.
    IF mv_initialized = abap_false.
      refresh( ).
    ENDIF.
  ENDMETHOD.

  METHOD status.

    initialize( ).

    IF lines( mt_status ) = 0.
      mt_status = lcl_file_status=>status( io_repo = me
                                           io_log  = io_log ).
    ENDIF.
    rt_results = mt_status.

  ENDMETHOD.                    "status

  METHOD deserialize.

    IF ms_data-write_protect = abap_true.
      lcx_exception=>raise( 'Cannot deserialize. Local code is write-protected by repo config' ).
    ENDIF.

    initialize( ).

    super->deserialize( ).

    set( iv_sha1 = mv_branch ).

    reset_status( ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.                    "deserialize

  METHOD reset_status.
    CLEAR mt_status.
  ENDMETHOD.  " reset_status.

  METHOD refresh.

    super->refresh( iv_drop_cache ).
    reset_status( ).

    lcl_progress=>show( iv_key     = 'Fetch'
                        iv_current = 1
                        iv_total   = 1
                        iv_text    = 'Remote files' ) ##NO_TEXT.

    lcl_git_porcelain=>pull( EXPORTING io_repo    = me
                             IMPORTING et_files   = mt_remote
                                       et_objects = mt_objects
                                       ev_branch  = mv_branch ).

    mo_branches = lcl_git_transport=>branches( get_url( ) ).
    actualize_head_branch( ).

    find_dot_abapgit( ).

    mv_initialized = abap_true.

  ENDMETHOD.                    "refresh

  METHOD actualize_head_branch.
    DATA lv_branch_name TYPE string.
    lv_branch_name = mo_branches->get_head( )-name.

    IF lv_branch_name <> ms_data-head_branch.
      set( iv_head_branch = lv_branch_name ).
    ENDIF.

  ENDMETHOD.                    "actualize_head_branch

  METHOD get_sha1_remote.
    initialize( ).

    rv_sha1 = mv_branch.
  ENDMETHOD.                    "get_sha1_remote

  METHOD get_files_remote.
    initialize( ).

    rt_files = mt_remote.
  ENDMETHOD.                    "get_files

  METHOD get_objects.
    initialize( ).

    rt_objects = mt_objects.
  ENDMETHOD.                    "get_objects

  METHOD get_url.
    rv_url = ms_data-url.
  ENDMETHOD.                    "get_url

  METHOD get_branch_name.
    rv_name = ms_data-branch_name.
  ENDMETHOD.                    "get_branch_name

  METHOD get_head_branch_name.
    rv_name = ms_data-head_branch.
  ENDMETHOD.                    "get_head_branch_name

  METHOD get_branches.
    IF mo_branches IS NOT BOUND.
      mo_branches = lcl_git_transport=>branches( get_url( ) ).
    ENDIF.
    ro_branches = mo_branches.
  ENDMETHOD.                    "get_branches

  METHOD set_url.

    IF ms_data-write_protect = abap_true.
      lcx_exception=>raise( 'Cannot change URL. Local code is write-protected by repo config' ).
    ENDIF.

    mv_initialized = abap_false.
    set( iv_url = iv_url ).

  ENDMETHOD.

  METHOD set_branch_name.

    IF ms_data-write_protect = abap_true.
      lcx_exception=>raise( 'Cannot switch branch. Local code is write-protected by repo config' ).
    ENDIF.

    mv_initialized = abap_false.
    set( iv_branch_name = iv_branch_name ).

  ENDMETHOD.

  METHOD set_new_remote.

    IF ms_data-write_protect = abap_true.
      lcx_exception=>raise( 'Cannot change remote. Local code is write-protected by repo config' ).
    ENDIF.

    mv_initialized = abap_false.
    set( iv_url         = iv_url
         iv_branch_name = iv_branch_name
         iv_head_branch = ''
         iv_sha1        = '' ).

  ENDMETHOD.  "set_new_remote

  METHOD get_sha1_local.
    rv_sha1 = ms_data-sha1.
  ENDMETHOD.                    "get_sha1_local

  METHOD push.

    DATA: lv_branch        TYPE ty_sha1,
          lt_updated_files TYPE ty_file_signatures_tt.


    handle_stage_ignore( io_stage ).

    lcl_git_porcelain=>push( EXPORTING is_comment       = is_comment
                                       io_repo          = me
                                       io_stage         = io_stage
                             IMPORTING ev_branch        = lv_branch
                                       et_updated_files = lt_updated_files ).

    IF io_stage->get_branch_sha1( ) = get_sha1_local( ).
* pushing to the branch currently represented by this repository object
      set( iv_sha1 = lv_branch ).
    ENDIF.

    refresh( ).
    update_local_checksums( lt_updated_files ).

    IF lcl_stage_logic=>count( me ) = 0.
      set( iv_sha1 = lv_branch ).
    ENDIF.

  ENDMETHOD.                    "push

  METHOD handle_stage_ignore.

    DATA: lv_add   TYPE abap_bool,
          lt_stage TYPE lcl_stage=>ty_stage_tt.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF lt_stage.


    lt_stage = io_stage->get_all( ).
    LOOP AT lt_stage ASSIGNING <ls_stage> WHERE method = lcl_stage=>c_method-ignore.

      mo_dot_abapgit->add_ignore(
        iv_path     = <ls_stage>-file-path
        iv_filename = <ls_stage>-file-filename ).

      " remove it from the staging object, as the action is handled here
      io_stage->reset( iv_path     = <ls_stage>-file-path
                       iv_filename = <ls_stage>-file-filename ).

      lv_add = abap_true.

    ENDLOOP.

    IF lv_add = abap_true.
      io_stage->add(
        iv_path     = gc_root_dir
        iv_filename = gc_dot_abapgit
        iv_data     = mo_dot_abapgit->serialize( ) ).
    ENDIF.

  ENDMETHOD.

  METHOD rebuild_local_checksums. "REMOTE

    DATA: lt_remote       TYPE ty_files_tt,
          lt_local        TYPE ty_files_item_tt,
          ls_last_item    TYPE ty_item,
          lv_branch_equal TYPE abap_bool,
          lt_checksums    TYPE lcl_persistence_repo=>ty_local_checksum_tt.

    FIELD-SYMBOLS: <ls_checksum> LIKE LINE OF lt_checksums,
                   <ls_file_sig> LIKE LINE OF <ls_checksum>-files,
                   <ls_remote>   LIKE LINE OF lt_remote,
                   <ls_local>    LIKE LINE OF lt_local.

    lt_remote       = get_files_remote( ).
    lt_local        = get_files_local( ).
    lv_branch_equal = boolc( get_sha1_remote( ) = get_sha1_local( ) ).

    DELETE lt_local " Remove non-code related files except .abapgit
      WHERE item IS INITIAL
      AND NOT ( file-path = gc_root_dir AND file-filename = gc_dot_abapgit ).

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
      IF <ls_local>-file-sha1 <> <ls_remote>-sha1 AND lv_branch_equal = abap_true.
        <ls_file_sig>-sha1 = <ls_remote>-sha1.
      ENDIF.
    ENDLOOP.

    set( it_checksums = lt_checksums ).
    reset_status( ).

  ENDMETHOD.  " rebuild_local_checksums.

ENDCLASS.                    "lcl_repo_online IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_repo IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_repo IMPLEMENTATION.

  METHOD constructor.

    ASSERT NOT is_data-key IS INITIAL.

    ms_data = is_data.

  ENDMETHOD.                    "constructor

  METHOD find_dot_abapgit.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF mt_remote.


    READ TABLE mt_remote ASSIGNING <ls_remote>
      WITH KEY path = gc_root_dir
      filename = gc_dot_abapgit.
    IF sy-subrc = 0.
      mo_dot_abapgit = lcl_dot_abapgit=>deserialize( <ls_remote>-data ).
    ENDIF.

  ENDMETHOD.

  METHOD get_files_remote.
    rt_files = mt_remote.
  ENDMETHOD.

  METHOD set.

    DATA: lo_persistence TYPE REF TO lcl_persistence_repo.


    ASSERT iv_sha1 IS SUPPLIED
      OR it_checksums IS SUPPLIED
      OR iv_url IS SUPPLIED
      OR iv_branch_name IS SUPPLIED
      OR iv_head_branch IS SUPPLIED
      OR iv_offline IS SUPPLIED.

    CREATE OBJECT lo_persistence.

    IF iv_sha1 IS SUPPLIED.
      lo_persistence->update_sha1(
        iv_key         = ms_data-key
        iv_branch_sha1 = iv_sha1 ).
      ms_data-sha1 = iv_sha1.
    ENDIF.

    IF it_checksums IS SUPPLIED.
      lo_persistence->update_local_checksums(
        iv_key       = ms_data-key
        it_checksums = it_checksums ).
      ms_data-local_checksums = it_checksums.
    ENDIF.

    IF iv_url IS SUPPLIED.
      lo_persistence->update_url(
        iv_key = ms_data-key
        iv_url = iv_url ).
      ms_data-url = iv_url.
    ENDIF.

    IF iv_branch_name IS SUPPLIED.
      lo_persistence->update_branch_name(
        iv_key         = ms_data-key
        iv_branch_name = iv_branch_name ).
      ms_data-branch_name = iv_branch_name.
    ENDIF.

    IF iv_head_branch IS SUPPLIED.
      lo_persistence->update_head_branch(
        iv_key         = ms_data-key
        iv_head_branch = iv_head_branch ).
      ms_data-head_branch = iv_head_branch.
    ENDIF.

    IF iv_offline IS SUPPLIED.
      lo_persistence->update_offline(
        iv_key     = ms_data-key
        iv_offline = iv_offline ).
      ms_data-offline = iv_offline.
    ENDIF.

  ENDMETHOD.                    "set_sha1

  METHOD update_local_checksums.

    " ASSUMTION: SHA1 in param is actual and correct.
    " Push fills it from local files before pushing, deserialize from remote
    " If this is not true that there is an error somewhere but not here

    DATA: lt_checksums TYPE lcl_persistence_repo=>ty_local_checksum_tt,
          lt_files_idx TYPE ty_file_signatures_tt,
          lt_local     TYPE ty_files_item_tt,
          lv_chks_row  TYPE i,
          lv_file_row  TYPE i.

    FIELD-SYMBOLS: <ls_checksum>  LIKE LINE OF lt_checksums,
                   <ls_file>      LIKE LINE OF <ls_checksum>-files,
                   <ls_local>     LIKE LINE OF lt_local,
                   <ls_new_state> LIKE LINE OF it_files.

    lt_checksums = get_local_checksums( ).
    lt_files_idx = it_files.
    SORT lt_files_idx BY path filename. " Sort for binary search

    " Loop through current chacksum state, update sha1 for common files
    LOOP AT lt_checksums ASSIGNING <ls_checksum>.
      lv_chks_row = sy-tabix.

      LOOP AT <ls_checksum>-files ASSIGNING <ls_file>.
        lv_file_row = sy-tabix.

        READ TABLE lt_files_idx ASSIGNING <ls_new_state>
          WITH KEY path = <ls_file>-path filename = <ls_file>-filename
          BINARY SEARCH.
        CHECK sy-subrc = 0. " Missing in param table, skip

        IF <ls_new_state>-sha1 IS INITIAL. " Empty input sha1 is a deletion marker
          DELETE <ls_checksum>-files INDEX lv_file_row.
        ELSE.
          <ls_file>-sha1 = <ls_new_state>-sha1.  " Update sha1
          CLEAR <ls_new_state>-sha1.             " Mark as processed
        ENDIF.
      ENDLOOP.

      IF lines( <ls_checksum>-files ) = 0. " Remove empty objects
        DELETE lt_checksums INDEX lv_chks_row.
      ENDIF.
    ENDLOOP.

    DELETE lt_files_idx WHERE sha1 IS INITIAL. " Remove processed
    IF lines( lt_files_idx ) > 0.
      lt_local = get_files_local( ).
      SORT lt_local BY file-path file-filename. " Sort for binary search
    ENDIF.

    " Add new files - not deleted and not marked as processed above
    LOOP AT lt_files_idx ASSIGNING <ls_new_state>.

      READ TABLE lt_local ASSIGNING <ls_local>
        WITH KEY file-path = <ls_new_state>-path file-filename = <ls_new_state>-filename
        BINARY SEARCH.
      IF sy-subrc <> 0.
* if the deserialization fails, the local file might not be there
        CONTINUE.
      ENDIF.

      READ TABLE lt_checksums ASSIGNING <ls_checksum> " TODO Optimize
        WITH KEY item = <ls_local>-item.
      IF sy-subrc > 0.
        APPEND INITIAL LINE TO lt_checksums ASSIGNING <ls_checksum>.
        <ls_checksum>-item = <ls_local>-item.
      ENDIF.

      APPEND <ls_new_state> TO <ls_checksum>-files.
    ENDLOOP.

    SORT lt_checksums BY item.
    set( it_checksums = lt_checksums ).

  ENDMETHOD.  " update_local_checksums

  METHOD deserialize.

    DATA: lt_updated_files TYPE ty_file_signatures_tt.

    IF mo_dot_abapgit IS INITIAL.
      mo_dot_abapgit = lcl_dot_abapgit=>build_default( ms_data-master_language ).
    ENDIF.
    IF mo_dot_abapgit->get_master_language( ) <> sy-langu.
      lcx_exception=>raise( 'Current login language does not match master language' ).
    ENDIF.

    lt_updated_files = lcl_objects=>deserialize( me ).
    APPEND mo_dot_abapgit->get_signature( ) TO lt_updated_files.

    CLEAR: mt_local, mv_last_serialization.

    update_local_checksums( lt_updated_files ).

  ENDMETHOD.

  METHOD get_local_checksums.
    rt_checksums = ms_data-local_checksums.
  ENDMETHOD.

  METHOD get_local_checksums_per_file.

    FIELD-SYMBOLS <object> LIKE LINE OF ms_data-local_checksums.

    LOOP AT ms_data-local_checksums ASSIGNING <object>.
      APPEND LINES OF <object>-files TO rt_checksums.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_files_local.

    DATA: lt_tadir TYPE ty_tadir_tt,
          ls_item  TYPE ty_item,
          lt_files TYPE ty_files_tt,
          lt_cache TYPE SORTED TABLE OF ty_file_item
                   WITH NON-UNIQUE KEY item.

    DATA: lt_filter       TYPE SORTED TABLE OF tadir
                          WITH NON-UNIQUE KEY object obj_name,
          lv_filter_exist TYPE abap_bool.

    FIELD-SYMBOLS: <ls_file>   LIKE LINE OF lt_files,
                   <ls_return> LIKE LINE OF rt_files,
                   <ls_cache>  LIKE LINE OF lt_cache,
                   <ls_tadir>  LIKE LINE OF lt_tadir.


    " Serialization happened before and no refresh request
    IF mv_last_serialization IS NOT INITIAL AND mv_do_local_refresh = abap_false.
      rt_files = mt_local.
      RETURN.
    ENDIF.

    IF mo_dot_abapgit IS INITIAL.
      mo_dot_abapgit = lcl_dot_abapgit=>build_default( ms_data-master_language ).
    ENDIF.
    APPEND INITIAL LINE TO rt_files ASSIGNING <ls_return>.
    <ls_return>-file-path     = gc_root_dir.
    <ls_return>-file-filename = gc_dot_abapgit.
    <ls_return>-file-data     = mo_dot_abapgit->serialize( ).
    <ls_return>-file-sha1     = lcl_hash=>sha1( iv_type = gc_type-blob
                                                iv_data = <ls_return>-file-data ).

    lt_cache = mt_local.
    lt_tadir = lcl_tadir=>read(
      iv_package            = get_package( )
      iv_ignore_subpackages = ignore_subpackages( ) ).

    lt_filter = it_filter.
    lv_filter_exist = boolc( lines( lt_filter ) > 0 ) .

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      IF lv_filter_exist = abap_true.
        READ TABLE lt_filter TRANSPORTING NO FIELDS WITH KEY object = <ls_tadir>-object
                                                             obj_name = <ls_tadir>-obj_name
                                                    BINARY SEARCH.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
      ENDIF.

      lcl_progress=>show( iv_key     = 'Serialize'
                          iv_current = sy-tabix
                          iv_total   = lines( lt_tadir )
                          iv_text    = <ls_tadir>-obj_name ) ##NO_TEXT.

      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.
      ls_item-devclass = <ls_tadir>-devclass.

      IF mv_last_serialization IS NOT INITIAL. " Try to fetch from cache
        READ TABLE lt_cache TRANSPORTING NO FIELDS
          WITH KEY item = ls_item. " type+name+package key
        " There is something in cache and the object is unchanged
        IF sy-subrc = 0
            AND abap_false = lcl_objects=>has_changed_since(
            is_item      = ls_item
            iv_timestamp = mv_last_serialization ).
          LOOP AT lt_cache ASSIGNING <ls_cache> WHERE item = ls_item.
            APPEND <ls_cache> TO rt_files.
          ENDLOOP.

          CONTINUE.
        ENDIF.
      ENDIF.

      lt_files = lcl_objects=>serialize(
        is_item     = ls_item
        iv_language = get_master_language( )
        io_log      = io_log ).
      LOOP AT lt_files ASSIGNING <ls_file>.
        <ls_file>-path = mo_dot_abapgit->get_starting_folder( ) && <ls_tadir>-path.
        <ls_file>-sha1 = lcl_hash=>sha1( iv_type = gc_type-blob iv_data = <ls_file>-data ).

        APPEND INITIAL LINE TO rt_files ASSIGNING <ls_return>.
        <ls_return>-file = <ls_file>.
        <ls_return>-item = ls_item.
      ENDLOOP.
    ENDLOOP.

    GET TIME STAMP FIELD mv_last_serialization.
    mt_local            = rt_files.
    mv_do_local_refresh = abap_false. " Fulfill refresh

  ENDMETHOD.

  METHOD get_dot_abapgit.
    ro_dot_abapgit = mo_dot_abapgit.
  ENDMETHOD.

  METHOD delete.

    DATA: lo_persistence TYPE REF TO lcl_persistence_repo.


    CREATE OBJECT lo_persistence.

    lo_persistence->delete( ms_data-key ).

  ENDMETHOD.                    "delete

  METHOD is_offline.
    rv_offline = ms_data-offline.
  ENDMETHOD.

  METHOD refresh.

    mv_do_local_refresh = abap_true.

    IF iv_drop_cache = abap_true.
      CLEAR: mv_last_serialization, mt_local.
    ENDIF.

  ENDMETHOD.                    "refresh

  METHOD refresh_local. " For testing purposes, maybe removed later
    mv_do_local_refresh = abap_true.
  ENDMETHOD.  "refresh_local

  METHOD get_package.
    rv_package = ms_data-package.
  ENDMETHOD.                    "get_package

  METHOD get_master_language.
    rv_language = ms_data-master_language.
  ENDMETHOD.

  METHOD get_key.
    rv_key = ms_data-key.
  ENDMETHOD.                    "get_key

  METHOD get_name.

    IF ms_data-offline = abap_true.
      rv_name = ms_data-url.
    ELSE.
      rv_name = lcl_url=>name( ms_data-url ).
      rv_name = cl_http_utility=>if_http_utility~unescape_url( rv_name ).
    ENDIF.

  ENDMETHOD.                    "get_name

  METHOD is_write_protected.
    rv_yes = ms_data-write_protect.
  ENDMETHOD.                    "is_write_protected

  METHOD ignore_subpackages.
    rv_yes = ms_data-ignore_subpackages.
  ENDMETHOD.

  METHOD rebuild_local_checksums. "LOCAL (BASE)

    DATA: lt_local     TYPE ty_files_item_tt,
          ls_last_item TYPE ty_item,
          lt_checksums TYPE lcl_persistence_repo=>ty_local_checksum_tt.

    FIELD-SYMBOLS: <ls_checksum> LIKE LINE OF lt_checksums,
                   <ls_file_sig> LIKE LINE OF <ls_checksum>-files,
                   <ls_local>    LIKE LINE OF lt_local.


    lt_local = get_files_local( ).

    DELETE lt_local " Remove non-code related files except .abapgit
      WHERE item IS INITIAL
      AND NOT ( file-path = gc_root_dir AND file-filename = gc_dot_abapgit ).

    SORT lt_local BY item.

    LOOP AT lt_local ASSIGNING <ls_local>.
      IF ls_last_item <> <ls_local>-item OR sy-tabix = 1. " First or New item reached ?
        APPEND INITIAL LINE TO lt_checksums ASSIGNING <ls_checksum>.
        <ls_checksum>-item = <ls_local>-item.
        ls_last_item       = <ls_local>-item.
      ENDIF.

      APPEND INITIAL LINE TO <ls_checksum>-files ASSIGNING <ls_file_sig>.
      MOVE-CORRESPONDING <ls_local>-file TO <ls_file_sig>.

    ENDLOOP.

    set( it_checksums = lt_checksums ).

  ENDMETHOD.  " rebuild_local_checksums.

ENDCLASS.                    "lcl_repo IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_repo_srv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_repo_srv IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT mo_persistence.
  ENDMETHOD.                    "class_constructor

  METHOD list.

    IF mv_init = abap_false.
      refresh( ).
    ENDIF.

    rt_list = mt_list.

  ENDMETHOD.                    "list

  METHOD get.

    FIELD-SYMBOLS: <lo_list> LIKE LINE OF mt_list.


    IF mv_init = abap_false.
      refresh( ).
    ENDIF.

    LOOP AT mt_list ASSIGNING <lo_list>.
      IF <lo_list>->get_key( ) = iv_key.
        ro_repo = <lo_list>.
        RETURN.
      ENDIF.
    ENDLOOP.

    lcx_exception=>raise( 'repo not found, get' ).

  ENDMETHOD.                    "get

  METHOD refresh.

    DATA: lt_list    TYPE lcl_persistence_repo=>tt_repo,
          lo_online  TYPE REF TO lcl_repo_online,
          lo_offline TYPE REF TO lcl_repo_offline.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    CLEAR mt_list.

    lt_list = mo_persistence->list( ).
    LOOP AT lt_list ASSIGNING <ls_list>.
      IF <ls_list>-offline = abap_false.
        CREATE OBJECT lo_online
          EXPORTING
            is_data = <ls_list>.
        APPEND lo_online TO mt_list.
      ELSE.
        CREATE OBJECT lo_offline
          EXPORTING
            is_data = <ls_list>.
        APPEND lo_offline TO mt_list.
      ENDIF.
    ENDLOOP.

    mv_init = abap_true.

  ENDMETHOD.                    "refresh

  METHOD new_online.

    DATA: ls_repo TYPE lcl_persistence_repo=>ty_repo,
          lv_key  TYPE lcl_persistence_repo=>ty_repo-key.


    validate_package( iv_package ).

    lv_key = mo_persistence->add(
      iv_url         = iv_url
      iv_branch_name = iv_branch_name
      iv_package     = iv_package
      iv_offline     = abap_false ).

    TRY.
        ls_repo = mo_persistence->read( lv_key ).
      CATCH lcx_not_found.
        lcx_exception=>raise( 'new_online not found' ).
    ENDTRY.

    CREATE OBJECT ro_repo
      EXPORTING
        is_data = ls_repo.

    add( ro_repo ).

  ENDMETHOD.                    "new_online

  METHOD new_offline.

    DATA: ls_repo TYPE lcl_persistence_repo=>ty_repo,
          lv_key  TYPE lcl_persistence_repo=>ty_repo-key.


    validate_package( iv_package ).

    lv_key = mo_persistence->add(
      iv_url         = iv_url
      iv_branch_name = ''
      iv_package     = iv_package
      iv_offline     = abap_true ).

    TRY.
        ls_repo = mo_persistence->read( lv_key ).
      CATCH lcx_not_found.
        lcx_exception=>raise( 'new_offline not found' ).
    ENDTRY.

    CREATE OBJECT ro_repo
      EXPORTING
        is_data = ls_repo.

    add( ro_repo ).

  ENDMETHOD.                    "new_offline

  METHOD add.

    DATA: lo_repo LIKE LINE OF mt_list.


    LOOP AT mt_list INTO lo_repo.
      IF lo_repo->get_key( ) = io_repo->get_key( ).
        IF lo_repo = io_repo.
          RETURN.
        ENDIF.
        lcx_exception=>raise( 'identical keys' ).
      ENDIF.
    ENDLOOP.

    APPEND io_repo TO mt_list.

  ENDMETHOD.                    "add

  METHOD validate_package.

    DATA: lv_devclass TYPE tdevc-devclass,
          lt_repos    TYPE lcl_persistence_repo=>tt_repo.


    IF iv_package IS INITIAL.
      lcx_exception=>raise( 'add, package empty' ).
    ENDIF.

    IF iv_package = '$TMP'.
      lcx_exception=>raise( 'not possible to use $TMP, create new (local) package' ).
    ENDIF.

    SELECT SINGLE devclass FROM tdevc INTO lv_devclass
      WHERE devclass = iv_package
      AND as4user <> 'SAP'.                             "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'package not found or not allowed' ).
    ENDIF.

    " make sure its not already in use for a different repository
    lt_repos = mo_persistence->list( ).
    READ TABLE lt_repos WITH KEY package = iv_package TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lcx_exception=>raise( 'Package already in use' ).
    ENDIF.

  ENDMETHOD.                    "validate_package

  METHOD delete.

    io_repo->delete( ).

    DELETE TABLE mt_list FROM io_repo.
    ASSERT sy-subrc = 0.

  ENDMETHOD.                    "delete

  METHOD is_repo_installed.

    DATA: lt_repo        TYPE lcl_repo_srv=>ty_repo_tt,
          lo_repo        TYPE REF TO lcl_repo,
          lv_url         TYPE string,
          lv_package     TYPE devclass,
          lo_repo_online TYPE REF TO lcl_repo_online,
          lv_err         TYPE string.

    lt_repo = list( ).

    LOOP AT lt_repo INTO lo_repo.
      CHECK lo_repo->is_offline( ) = abap_false.
      lo_repo_online ?= lo_repo.

      lv_url     = lo_repo_online->get_url( ).
      lv_package = lo_repo_online->get_package( ).
      CHECK to_upper( lv_url ) = to_upper( iv_url ).

      " Validate bindings
      "TODO refactor: move this message out of this method
      IF iv_target_package IS NOT INITIAL AND iv_target_package <> lv_package.
        lv_err = |Installation to package { lv_package } detected. |
              && |Cancelling installation|.
        lcx_exception=>raise( lv_err ).
      ENDIF.

      rv_installed = abap_true.
      EXIT.
    ENDLOOP.

  ENDMETHOD. "is_repo_installed

  METHOD switch_repo_type.

    DATA lo_repo TYPE REF TO lcl_repo.

    FIELD-SYMBOLS <repo> LIKE LINE OF mt_list.

    lo_repo = get( iv_key ).
    READ TABLE mt_list ASSIGNING <repo> FROM lo_repo.
    ASSERT sy-subrc IS INITIAL.
    ASSERT iv_offline <> lo_repo->ms_data-offline.

    IF iv_offline = abap_true. " On-line -> OFFline
      lo_repo->set(
        iv_url         = lcl_url=>name( lo_repo->ms_data-url )
        iv_branch_name = ''
        iv_sha1        = ''
        iv_head_branch = ''
        iv_offline     = abap_true ).
      CREATE OBJECT <repo> TYPE lcl_repo_offline
        EXPORTING
          is_data = lo_repo->ms_data.
    ELSE. " OFFline -> On-line
      lo_repo->set( iv_offline     = abap_false ).
      CREATE OBJECT <repo> TYPE lcl_repo_online
        EXPORTING
          is_data = lo_repo->ms_data.
    ENDIF.

  ENDMETHOD.  "switch_repo_type

ENDCLASS.                    "lcl_repo_srv IMPLEMENTATION
