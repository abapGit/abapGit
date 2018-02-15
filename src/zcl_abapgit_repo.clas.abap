class ZCL_ABAPGIT_REPO definition
  public
  abstract
  create public

  global friends ZCL_ABAPGIT_REPO_SRV .

public section.

  methods CONSTRUCTOR
    importing
      !IS_DATA type ZCL_ABAPGIT_PERSISTENCE_REPO=>TY_REPO .
  methods GET_KEY
    returning
      value(RV_KEY) type ZIF_ABAPGIT_PERSISTENCE=>TY_VALUE .
  methods GET_NAME
    returning
      value(RV_NAME) type STRING
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods GET_FILES_LOCAL
    importing
      !IO_LOG type ref to ZCL_ABAPGIT_LOG optional
      !IT_FILTER type SCTS_TADIR optional
    returning
      value(RT_FILES) type ZIF_ABAPGIT_DEFINITIONS=>TY_FILES_ITEM_TT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods GET_LOCAL_CHECKSUMS
    returning
      value(RT_CHECKSUMS) type ZCL_ABAPGIT_PERSISTENCE_REPO=>TY_LOCAL_CHECKSUM_TT .
  methods GET_LOCAL_CHECKSUMS_PER_FILE
    returning
      value(RT_CHECKSUMS) type ZIF_ABAPGIT_DEFINITIONS=>TY_FILE_SIGNATURES_TT .
  methods GET_FILES_REMOTE
    returning
      value(RT_FILES) type ZIF_ABAPGIT_DEFINITIONS=>TY_FILES_TT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods GET_PACKAGE
    returning
      value(RV_PACKAGE) type ZCL_ABAPGIT_PERSISTENCE_REPO=>TY_REPO-PACKAGE .
  methods GET_MASTER_LANGUAGE
    returning
      value(RV_LANGUAGE) type SPRAS .
  methods IS_WRITE_PROTECTED
    returning
      value(RV_YES) type SAP_BOOL .
  methods IGNORE_SUBPACKAGES
    returning
      value(RV_YES) type SAP_BOOL .
  methods DELETE
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods GET_DOT_ABAPGIT
    returning
      value(RO_DOT_ABAPGIT) type ref to ZCL_ABAPGIT_DOT_ABAPGIT .
  methods SET_DOT_ABAPGIT
    importing
      !IO_DOT_ABAPGIT type ref to ZCL_ABAPGIT_DOT_ABAPGIT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods DESERIALIZE
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods REFRESH
    importing
      !IV_DROP_CACHE type ABAP_BOOL default ABAP_FALSE
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods REFRESH_LOCAL .
  methods UPDATE_LOCAL_CHECKSUMS
    importing
      !IT_FILES type ZIF_ABAPGIT_DEFINITIONS=>TY_FILE_SIGNATURES_TT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods REBUILD_LOCAL_CHECKSUMS
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods FIND_REMOTE_DOT_ABAPGIT
    returning
      value(RO_DOT) type ref to ZCL_ABAPGIT_DOT_ABAPGIT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods IS_OFFLINE
    returning
      value(RV_OFFLINE) type ABAP_BOOL
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods SET_FILES_REMOTE
    importing
      !IT_FILES type ZIF_ABAPGIT_DEFINITIONS=>TY_FILES_TT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  PROTECTED SECTION.
    DATA: mt_local              TYPE zif_abapgit_definitions=>ty_files_item_tt,
          mt_remote             TYPE zif_abapgit_definitions=>ty_files_tt,
          mv_do_local_refresh   TYPE abap_bool,
          mv_last_serialization TYPE timestamp,
          ms_data               TYPE zcl_abapgit_persistence_repo=>ty_repo.

    METHODS:
      set
        IMPORTING iv_sha1        TYPE zif_abapgit_definitions=>ty_sha1 OPTIONAL
                  it_checksums   TYPE zcl_abapgit_persistence_repo=>ty_local_checksum_tt OPTIONAL
                  iv_url         TYPE zcl_abapgit_persistence_repo=>ty_repo-url OPTIONAL
                  iv_branch_name TYPE zcl_abapgit_persistence_repo=>ty_repo-branch_name OPTIONAL
                  iv_head_branch TYPE zcl_abapgit_persistence_repo=>ty_repo-head_branch OPTIONAL
                  iv_offline     TYPE zcl_abapgit_persistence_repo=>ty_repo-offline OPTIONAL
                  is_dot_abapgit TYPE zcl_abapgit_persistence_repo=>ty_repo-dot_abapgit OPTIONAL
        RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_REPO IMPLEMENTATION.


  METHOD constructor.

    ASSERT NOT is_data-key IS INITIAL.

    ms_data = is_data.

  ENDMETHOD.                    "constructor


  METHOD delete.

    DATA: lo_persistence TYPE REF TO zcl_abapgit_persistence_repo.


    CREATE OBJECT lo_persistence.

    lo_persistence->delete( ms_data-key ).

  ENDMETHOD.                    "delete


  METHOD deserialize.

    DATA: lt_updated_files TYPE zif_abapgit_definitions=>ty_file_signatures_tt,
          lt_requirements  TYPE STANDARD TABLE OF zif_abapgit_dot_abapgit=>ty_requirement,
          lx_error         TYPE REF TO zcx_abapgit_exception.

    find_remote_dot_abapgit( ).

    IF get_dot_abapgit( )->get_master_language( ) <> sy-langu.
      zcx_abapgit_exception=>raise( 'Current login language does not match master language' ).
    ENDIF.

    lt_requirements = get_dot_abapgit( )->get_data( )-requirements.
    IF lt_requirements IS NOT INITIAL.
      zcl_abapgit_requirement_helper=>check_requirements( it_requirements = lt_requirements
                                                          iv_show_popup   = abap_true ).
    ENDIF.

    TRY.
        lt_updated_files = zcl_abapgit_objects=>deserialize( me ).

      CATCH zcx_abapgit_exception INTO lx_error.

        " ensure to reset default transport request task
        zcl_abapgit_default_task=>get_instance( )->reset( ).
        zcx_abapgit_exception=>raise( lx_error->text ).

    ENDTRY.

    APPEND get_dot_abapgit( )->get_signature( ) TO lt_updated_files.

    CLEAR: mt_local, mv_last_serialization.

    update_local_checksums( lt_updated_files ).

  ENDMETHOD.


  METHOD find_remote_dot_abapgit.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF mt_remote.


    READ TABLE mt_remote ASSIGNING <ls_remote>
      WITH KEY path = zif_abapgit_definitions=>gc_root_dir
      filename = zif_abapgit_definitions=>gc_dot_abapgit.
    IF sy-subrc = 0.
      ro_dot = zcl_abapgit_dot_abapgit=>deserialize( <ls_remote>-data ).
      set_dot_abapgit( ro_dot ).
    ENDIF.

  ENDMETHOD.


  METHOD get_dot_abapgit.
    CREATE OBJECT ro_dot_abapgit
      EXPORTING
        is_data = ms_data-dot_abapgit.
  ENDMETHOD.


  METHOD get_files_local.

    DATA: lt_tadir    TYPE zif_abapgit_definitions=>ty_tadir_tt,
          ls_item     TYPE zif_abapgit_definitions=>ty_item,
          lt_files    TYPE zif_abapgit_definitions=>ty_files_tt,
          lo_progress TYPE REF TO zcl_abapgit_progress,
          lt_cache    TYPE SORTED TABLE OF zif_abapgit_definitions=>ty_file_item
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

    APPEND INITIAL LINE TO rt_files ASSIGNING <ls_return>.
    <ls_return>-file-path     = zif_abapgit_definitions=>gc_root_dir.
    <ls_return>-file-filename = zif_abapgit_definitions=>gc_dot_abapgit.
    <ls_return>-file-data     = get_dot_abapgit( )->serialize( ).
    <ls_return>-file-sha1     = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>gc_type-blob
                                                        iv_data = <ls_return>-file-data ).

    lt_cache = mt_local.
    lt_tadir = zcl_abapgit_tadir=>read(
      iv_package            = get_package( )
      iv_ignore_subpackages = ignore_subpackages( )
      io_dot                = get_dot_abapgit( )
      io_log                = io_log ).

    lt_filter = it_filter.
    lv_filter_exist = boolc( lines( lt_filter ) > 0 ).

    CREATE OBJECT lo_progress
      EXPORTING
        iv_total = lines( lt_tadir ).

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      IF lv_filter_exist = abap_true.
        READ TABLE lt_filter TRANSPORTING NO FIELDS WITH KEY object = <ls_tadir>-object
                                                             obj_name = <ls_tadir>-obj_name
                                                    BINARY SEARCH.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
      ENDIF.

      lo_progress->show(
        iv_current = sy-tabix
        iv_text    = |Serialize { <ls_tadir>-obj_name }| ) ##NO_TEXT.

      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.
      ls_item-devclass = <ls_tadir>-devclass.

      IF mv_last_serialization IS NOT INITIAL. " Try to fetch from cache
        READ TABLE lt_cache TRANSPORTING NO FIELDS
          WITH KEY item = ls_item. " type+name+package key
        " There is something in cache and the object is unchanged
        IF sy-subrc = 0
            AND abap_false = zcl_abapgit_objects=>has_changed_since(
            is_item      = ls_item
            iv_timestamp = mv_last_serialization ).
          LOOP AT lt_cache ASSIGNING <ls_cache> WHERE item = ls_item.
            APPEND <ls_cache> TO rt_files.
          ENDLOOP.

          CONTINUE.
        ENDIF.
      ENDIF.

      lt_files = zcl_abapgit_objects=>serialize(
        is_item     = ls_item
        iv_language = get_master_language( )
        io_log      = io_log ).
      LOOP AT lt_files ASSIGNING <ls_file>.
        <ls_file>-path = <ls_tadir>-path.
        <ls_file>-sha1 = zcl_abapgit_hash=>sha1(
          iv_type = zif_abapgit_definitions=>gc_type-blob
          iv_data = <ls_file>-data ).

        APPEND INITIAL LINE TO rt_files ASSIGNING <ls_return>.
        <ls_return>-file = <ls_file>.
        <ls_return>-item = ls_item.
      ENDLOOP.
    ENDLOOP.

    GET TIME STAMP FIELD mv_last_serialization.
    mt_local            = rt_files.
    mv_do_local_refresh = abap_false. " Fulfill refresh

  ENDMETHOD.


  METHOD get_files_remote.
    rt_files = mt_remote.
  ENDMETHOD.


  METHOD get_key.
    rv_key = ms_data-key.
  ENDMETHOD.                    "get_key


  METHOD get_local_checksums.
    rt_checksums = ms_data-local_checksums.
  ENDMETHOD.


  METHOD get_local_checksums_per_file.

    FIELD-SYMBOLS <object> LIKE LINE OF ms_data-local_checksums.

    LOOP AT ms_data-local_checksums ASSIGNING <object>.
      APPEND LINES OF <object>-files TO rt_checksums.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_master_language.
    rv_language = ms_data-dot_abapgit-master_language.
  ENDMETHOD.


  METHOD get_name.

    IF ms_data-offline = abap_true.
      rv_name = ms_data-url.
    ELSE.
      rv_name = zcl_abapgit_url=>name( ms_data-url ).
      rv_name = cl_http_utility=>if_http_utility~unescape_url( rv_name ).
    ENDIF.

  ENDMETHOD.                    "get_name


  METHOD get_package.
    rv_package = ms_data-package.
  ENDMETHOD.                    "get_package


  METHOD ignore_subpackages.
    rv_yes = ms_data-ignore_subpackages.
  ENDMETHOD.


  METHOD is_offline.
    rv_offline = ms_data-offline.
  ENDMETHOD.


  METHOD is_write_protected.
    rv_yes = ms_data-write_protect.
  ENDMETHOD.                    "is_write_protected


  METHOD rebuild_local_checksums. "LOCAL (BASE)

    DATA: lt_local     TYPE zif_abapgit_definitions=>ty_files_item_tt,
          ls_last_item TYPE zif_abapgit_definitions=>ty_item,
          lt_checksums TYPE zcl_abapgit_persistence_repo=>ty_local_checksum_tt.

    FIELD-SYMBOLS: <ls_checksum> LIKE LINE OF lt_checksums,
                   <ls_file_sig> LIKE LINE OF <ls_checksum>-files,
                   <ls_local>    LIKE LINE OF lt_local.


    lt_local = get_files_local( ).

    DELETE lt_local " Remove non-code related files except .abapgit
      WHERE item IS INITIAL
      AND NOT ( file-path     = zif_abapgit_definitions=>gc_root_dir
      AND       file-filename = zif_abapgit_definitions=>gc_dot_abapgit ).

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


  METHOD refresh.

    mv_do_local_refresh = abap_true.

    IF iv_drop_cache = abap_true.
      CLEAR: mv_last_serialization, mt_local.
    ENDIF.

  ENDMETHOD.                    "refresh


  METHOD refresh_local. " For testing purposes, maybe removed later
    mv_do_local_refresh = abap_true.
  ENDMETHOD.  "refresh_local


  METHOD set.

    DATA: lo_persistence TYPE REF TO zcl_abapgit_persistence_repo.


    ASSERT iv_sha1 IS SUPPLIED
      OR it_checksums IS SUPPLIED
      OR iv_url IS SUPPLIED
      OR iv_branch_name IS SUPPLIED
      OR iv_head_branch IS SUPPLIED
      OR iv_offline IS SUPPLIED
      OR is_dot_abapgit IS SUPPLIED.

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

    IF is_dot_abapgit IS SUPPLIED.
      lo_persistence->update_dot_abapgit(
        iv_key         = ms_data-key
        is_dot_abapgit = is_dot_abapgit ).
      ms_data-dot_abapgit = is_dot_abapgit.
    ENDIF.

  ENDMETHOD.


  METHOD set_dot_abapgit.
    set( is_dot_abapgit = io_dot_abapgit->get_data( ) ).
  ENDMETHOD.


  METHOD set_files_remote.

    mt_remote = it_files.

  ENDMETHOD.


  METHOD update_local_checksums.

    " ASSUMTION: SHA1 in param is actual and correct.
    " Push fills it from local files before pushing, deserialize from remote
    " If this is not true that there is an error somewhere but not here

    DATA: lt_checksums TYPE zcl_abapgit_persistence_repo=>ty_local_checksum_tt,
          lt_files_idx TYPE zif_abapgit_definitions=>ty_file_signatures_tt,
          lt_local     TYPE zif_abapgit_definitions=>ty_files_item_tt,
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
ENDCLASS.
