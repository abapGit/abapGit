class ZCL_ABAPGIT_REPO definition
  public
  abstract
  create public .

public section.

  methods BIND_LISTENER
    importing
      !II_LISTENER type ref to ZIF_ABAPGIT_REPO_LISTENER .
  methods DESERIALIZE_CHECKS
    returning
      value(RS_CHECKS) type ZIF_ABAPGIT_DEFINITIONS=>TY_DESERIALIZE_CHECKS
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods DELETE_CHECKS
    returning
      value(RS_CHECKS) type ZIF_ABAPGIT_DEFINITIONS=>TY_DELETE_CHECKS
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods CONSTRUCTOR
    importing
      !IS_DATA type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO .
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
      !II_LOG type ref to ZIF_ABAPGIT_LOG optional
      !IT_FILTER type ZIF_ABAPGIT_DEFINITIONS=>TY_TADIR_TT optional
    returning
      value(RT_FILES) type ZIF_ABAPGIT_DEFINITIONS=>TY_FILES_ITEM_TT
    raising
      ZCX_ABAPGIT_EXCEPTION .
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
      value(RV_PACKAGE) type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-PACKAGE .
  methods GET_DOT_ABAPGIT
    returning
      value(RO_DOT_ABAPGIT) type ref to ZCL_ABAPGIT_DOT_ABAPGIT .
  methods SET_DOT_ABAPGIT
    importing
      !IO_DOT_ABAPGIT type ref to ZCL_ABAPGIT_DOT_ABAPGIT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods DESERIALIZE
    importing
      !IS_CHECKS type ZIF_ABAPGIT_DEFINITIONS=>TY_DESERIALIZE_CHECKS
      !II_LOG type ref to ZIF_ABAPGIT_LOG
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods REFRESH
    importing
      !IV_DROP_CACHE type ABAP_BOOL default ABAP_FALSE
    raising
      ZCX_ABAPGIT_EXCEPTION .
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
      !IT_FILES type ZIF_ABAPGIT_DEFINITIONS=>TY_FILES_TT .
  methods GET_LOCAL_SETTINGS
    returning
      value(RS_SETTINGS) type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-LOCAL_SETTINGS .
  methods SET_LOCAL_SETTINGS
    importing
      !IS_SETTINGS type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-LOCAL_SETTINGS
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods HAS_REMOTE_SOURCE
  abstract
    returning
      value(RV_YES) type ABAP_BOOL .
  methods STATUS
    importing
      !II_LOG type ref to ZIF_ABAPGIT_LOG optional
    returning
      value(RT_RESULTS) type ZIF_ABAPGIT_DEFINITIONS=>TY_RESULTS_TT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods SWITCH_REPO_TYPE
    importing
      !IV_OFFLINE type ABAP_BOOL
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods CREATE_NEW_LOG
    importing
      !IV_TITLE type STRING optional
    returning
      value(RI_LOG) type ref to ZIF_ABAPGIT_LOG .
  methods GET_LOG
    exporting
      !EI_LOG type ref to ZIF_ABAPGIT_LOG
      !EV_TITLE type STRING .
  methods RESET_LOG .
protected section.

  data MT_LOCAL type ZIF_ABAPGIT_DEFINITIONS=>TY_FILES_ITEM_TT .
  data MT_REMOTE type ZIF_ABAPGIT_DEFINITIONS=>TY_FILES_TT .
  data MV_REQUEST_LOCAL_REFRESH type ABAP_BOOL .
  data MS_DATA type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO .
  data MV_REQUEST_REMOTE_REFRESH type ABAP_BOOL .
  data MT_STATUS type ZIF_ABAPGIT_DEFINITIONS=>TY_RESULTS_TT .
  data MI_LOG type ref to ZIF_ABAPGIT_LOG .
  data MV_LOG_TITLE type STRING .

  methods SET
    importing
      !IT_CHECKSUMS type ZIF_ABAPGIT_PERSISTENCE=>TY_LOCAL_CHECKSUM_TT optional
      !IV_URL type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-URL optional
      !IV_BRANCH_NAME type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-BRANCH_NAME optional
      !IV_HEAD_BRANCH type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-HEAD_BRANCH optional
      !IV_OFFLINE type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-OFFLINE optional
      !IS_DOT_ABAPGIT type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-DOT_ABAPGIT optional
      !IS_LOCAL_SETTINGS type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-LOCAL_SETTINGS optional
      !IV_DESERIALIZED_AT type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-DESERIALIZED_AT optional
      !IV_DESERIALIZED_BY type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-DESERIALIZED_BY optional
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods RESET_STATUS .
  methods RESET_REMOTE .
  PRIVATE SECTION.

    DATA mi_listener TYPE REF TO zif_abapgit_repo_listener .

    METHODS get_local_checksums
      RETURNING
        VALUE(rt_checksums) TYPE zif_abapgit_persistence=>ty_local_checksum_tt .
    METHODS notify_listener
      IMPORTING
        !is_change_mask TYPE zif_abapgit_persistence=>ty_repo_meta_mask
      RAISING
        zcx_abapgit_exception .
    METHODS apply_filter
      IMPORTING
        !it_filter TYPE zif_abapgit_definitions=>ty_tadir_tt
      CHANGING
        !ct_tadir  TYPE zif_abapgit_definitions=>ty_tadir_tt .
    METHODS build_dotabapgit_file
      RETURNING
        VALUE(rs_file) TYPE zif_abapgit_definitions=>ty_file
      RAISING
        zcx_abapgit_exception .
    METHODS build_apack_manifest_file
      RETURNING
        VALUE(rs_file) TYPE zif_abapgit_definitions=>ty_file
      RAISING
        zcx_abapgit_exception .
    METHODS update_last_deserialize
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_REPO IMPLEMENTATION.


  METHOD apply_filter.

    DATA: lt_filter TYPE SORTED TABLE OF zif_abapgit_definitions=>ty_tadir
                      WITH NON-UNIQUE KEY object obj_name,
          lv_index  TYPE i.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF ct_tadir.


    IF lines( it_filter ) = 0.
      RETURN.
    ENDIF.

    lt_filter = it_filter.

* this is another loop at TADIR, but typically the filter is blank
    LOOP AT ct_tadir ASSIGNING <ls_tadir>.
      lv_index = sy-tabix.
      READ TABLE lt_filter TRANSPORTING NO FIELDS WITH KEY object = <ls_tadir>-object
                                                           obj_name = <ls_tadir>-obj_name
                                                  BINARY SEARCH.
      IF sy-subrc <> 0.
        DELETE ct_tadir INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD bind_listener.
    mi_listener = ii_listener.
  ENDMETHOD.


  METHOD build_apack_manifest_file.
    DATA: lo_manifest_reader TYPE REF TO zcl_abapgit_apack_reader,
          ls_descriptor      TYPE zif_abapgit_apack_definitions=>ty_descriptor,
          lo_manifest_writer TYPE REF TO zcl_abapgit_apack_writer.

    lo_manifest_reader = zcl_abapgit_apack_reader=>create_instance( ms_data-package ).
    IF lo_manifest_reader->has_manifest( ) = abap_true.
      ls_descriptor = lo_manifest_reader->get_manifest_descriptor( ).
      lo_manifest_writer = zcl_abapgit_apack_writer=>create_instance( ls_descriptor ).
      rs_file-path     = zif_abapgit_definitions=>c_root_dir.
      rs_file-filename = zif_abapgit_apack_definitions=>c_dot_apack_manifest.
      rs_file-data     = zcl_abapgit_convert=>string_to_xstring_utf8( lo_manifest_writer->serialize( ) ).
      rs_file-sha1     = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>c_type-blob
                                                 iv_data = rs_file-data ).
    ENDIF.
  ENDMETHOD.


  METHOD build_dotabapgit_file.

    rs_file-path     = zif_abapgit_definitions=>c_root_dir.
    rs_file-filename = zif_abapgit_definitions=>c_dot_abapgit.
    rs_file-data     = get_dot_abapgit( )->serialize( ).
    rs_file-sha1     = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>c_type-blob
                                               iv_data = rs_file-data ).

  ENDMETHOD.


  METHOD constructor.

    ASSERT NOT is_data-key IS INITIAL.

    ms_data = is_data.
    mv_request_remote_refresh = abap_true.

  ENDMETHOD.


  METHOD CREATE_NEW_LOG.

    CREATE OBJECT mi_log TYPE zcl_abapgit_log.
    mv_log_title = iv_title.

    ri_log = mi_log.

  ENDMETHOD.


  METHOD delete_checks.

    DATA: li_package TYPE REF TO zif_abapgit_sap_package.

    li_package = zcl_abapgit_factory=>get_sap_package( get_package( ) ).
    rs_checks-transport-required = li_package->are_changes_recorded_in_tr_req( ).

  ENDMETHOD.


  METHOD deserialize.

    DATA: lt_updated_files TYPE zif_abapgit_definitions=>ty_file_signatures_tt,
          lx_error         TYPE REF TO zcx_abapgit_exception.


    deserialize_checks( ).

    IF is_checks-requirements-met = 'N' AND is_checks-requirements-decision IS INITIAL.
      zcx_abapgit_exception=>raise( 'Requirements not met and undecided' ).
    ENDIF.

    IF is_checks-transport-required = abap_true AND is_checks-transport-transport IS INITIAL.
      zcx_abapgit_exception=>raise( |No transport request was supplied| ).
    ENDIF.

    TRY.
        lt_updated_files = zcl_abapgit_objects=>deserialize(
            io_repo   = me
            is_checks = is_checks
            ii_log    = ii_log ).
      CATCH zcx_abapgit_exception INTO lx_error.
* ensure to reset default transport request task
        zcl_abapgit_default_transport=>get_instance( )->reset( ).
        RAISE EXCEPTION lx_error.
    ENDTRY.

    APPEND get_dot_abapgit( )->get_signature( ) TO lt_updated_files.

    CLEAR: mt_local.

    update_local_checksums( lt_updated_files ).
    update_last_deserialize( ).
    reset_status( ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD deserialize_checks.

    DATA: lt_requirements    TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
          lv_master_language TYPE spras,
          lv_logon_language  TYPE spras.


    find_remote_dot_abapgit( ).

    lv_master_language = get_dot_abapgit( )->get_master_language( ).
    lv_logon_language  = sy-langu.

    IF get_local_settings( )-write_protected = abap_true.
      zcx_abapgit_exception=>raise( 'Cannot deserialize. Local code is write-protected by repo config' ).
    ELSEIF lv_master_language <> lv_logon_language.
      zcx_abapgit_exception=>raise( |Current login language |
                                 && |'{ zcl_abapgit_convert=>conversion_exit_isola_output( lv_logon_language ) }'|
                                 && | does not match master language |
                                 && |'{ zcl_abapgit_convert=>conversion_exit_isola_output( lv_master_language ) }'.|
                                 && | Run 'Advanced' > 'Open in master language'| ).
    ENDIF.

    rs_checks = zcl_abapgit_objects=>deserialize_checks( me ).

    lt_requirements = get_dot_abapgit( )->get_data( )-requirements.
    rs_checks-requirements-met = zcl_abapgit_requirement_helper=>is_requirements_met(
      lt_requirements ).

  ENDMETHOD.


  METHOD find_remote_dot_abapgit.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF mt_remote.

    get_files_remote( ).

    READ TABLE mt_remote ASSIGNING <ls_remote>
      WITH KEY path = zif_abapgit_definitions=>c_root_dir
      filename = zif_abapgit_definitions=>c_dot_abapgit.
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

    DATA: lt_tadir      TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lo_serialize  TYPE REF TO zcl_abapgit_serialize,
          lt_found      LIKE rt_files,
          ls_apack_file TYPE zif_abapgit_definitions=>ty_file.

    FIELD-SYMBOLS: <ls_return> LIKE LINE OF rt_files.


    " Serialization happened before and no refresh request
    IF lines( mt_local ) > 0 AND mv_request_local_refresh = abap_false.
      rt_files = mt_local.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO rt_files ASSIGNING <ls_return>.
    <ls_return>-file = build_dotabapgit_file( ).

    ls_apack_file = build_apack_manifest_file( ).
    IF ls_apack_file IS NOT INITIAL.
      APPEND INITIAL LINE TO rt_files ASSIGNING <ls_return>.
      <ls_return>-file = ls_apack_file.
    ENDIF.

    lt_tadir = zcl_abapgit_factory=>get_tadir( )->read(
      iv_package            = get_package( )
      iv_ignore_subpackages = get_local_settings( )-ignore_subpackages
      iv_only_local_objects = get_local_settings( )-only_local_objects
      io_dot                = get_dot_abapgit( )
      ii_log                = ii_log ).

    apply_filter( EXPORTING it_filter = it_filter
                  CHANGING ct_tadir  = lt_tadir ).

    CREATE OBJECT lo_serialize.

    lt_found = lo_serialize->serialize(
      it_tadir    = lt_tadir
      iv_language = get_dot_abapgit( )->get_master_language( )
      ii_log      = ii_log ).
    APPEND LINES OF lt_found TO rt_files.

    mt_local                 = rt_files.
    mv_request_local_refresh = abap_false. " Fulfill refresh

  ENDMETHOD.


  METHOD get_files_remote.
    rt_files = mt_remote.
  ENDMETHOD.


  METHOD get_key.
    rv_key = ms_data-key.
  ENDMETHOD.


  METHOD get_local_checksums.
    rt_checksums = ms_data-local_checksums.
  ENDMETHOD.


  METHOD get_local_checksums_per_file.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF ms_data-local_checksums.

    LOOP AT ms_data-local_checksums ASSIGNING <ls_object>.
      APPEND LINES OF <ls_object>-files TO rt_checksums.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_local_settings.

    rs_settings = ms_data-local_settings.

  ENDMETHOD.


  METHOD get_log.
    ei_log   = mi_log.
    ev_title = mv_log_title.
  ENDMETHOD.


  METHOD get_name.

    rv_name = ms_data-local_settings-display_name.

  ENDMETHOD.


  METHOD get_package.
    rv_package = ms_data-package.
  ENDMETHOD.


  METHOD is_offline.
    rv_offline = ms_data-offline.
  ENDMETHOD.


  METHOD notify_listener.

    DATA ls_meta_slug TYPE zif_abapgit_persistence=>ty_repo_xml.

    IF mi_listener IS BOUND.
      MOVE-CORRESPONDING ms_data TO ls_meta_slug.
      mi_listener->on_meta_change(
        iv_key         = ms_data-key
        is_meta        = ls_meta_slug
        is_change_mask = is_change_mask ).
    ENDIF.

  ENDMETHOD.


  METHOD rebuild_local_checksums.

    DATA:
      lt_local     TYPE zif_abapgit_definitions=>ty_files_item_tt,
      ls_last_item TYPE zif_abapgit_definitions=>ty_item,
      lt_checksums TYPE zif_abapgit_persistence=>ty_local_checksum_tt.

    FIELD-SYMBOLS:
      <ls_checksum> LIKE LINE OF lt_checksums,
      <ls_file_sig> LIKE LINE OF <ls_checksum>-files,
      <ls_local>    LIKE LINE OF lt_local.

    lt_local = get_files_local( ).

    DELETE lt_local " Remove non-code related files except .abapgit
      WHERE item IS INITIAL
      AND NOT ( file-path     = zif_abapgit_definitions=>c_root_dir
      AND       file-filename = zif_abapgit_definitions=>c_dot_abapgit ).
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
    reset_status( ).

  ENDMETHOD.


  METHOD refresh.

    mv_request_local_refresh = abap_true.
    reset_remote( ).

    CLEAR mi_log.

    IF iv_drop_cache = abap_true.
      CLEAR: mt_local.
    ENDIF.

  ENDMETHOD.


  METHOD reset_log.
    CLEAR mi_log.
    CLEAR mv_log_title.
  ENDMETHOD.


  METHOD reset_remote.
    CLEAR mt_remote.
    mv_request_remote_refresh = abap_true.
    reset_status( ).
  ENDMETHOD.


  METHOD reset_status.
    CLEAR mt_status.
  ENDMETHOD.


  METHOD set.

* TODO: refactor

    DATA:
          ls_mask        TYPE zif_abapgit_persistence=>ty_repo_meta_mask.


    ASSERT it_checksums IS SUPPLIED
      OR iv_url IS SUPPLIED
      OR iv_branch_name IS SUPPLIED
      OR iv_head_branch IS SUPPLIED
      OR iv_offline IS SUPPLIED
      OR is_dot_abapgit IS SUPPLIED
      OR is_local_settings IS SUPPLIED
      OR iv_deserialized_by IS SUPPLIED
      OR iv_deserialized_at IS SUPPLIED.


    IF it_checksums IS SUPPLIED.
      ms_data-local_checksums = it_checksums.
      ls_mask-local_checksums = abap_true.
    ENDIF.

    IF iv_url IS SUPPLIED.
      ms_data-url = iv_url.
      ls_mask-url = abap_true.
    ENDIF.

    IF iv_branch_name IS SUPPLIED.
      ms_data-branch_name = iv_branch_name.
      ls_mask-branch_name = abap_true.
    ENDIF.

    IF iv_head_branch IS SUPPLIED.
      ms_data-head_branch = iv_head_branch.
      ls_mask-head_branch = abap_true.
    ENDIF.

    IF iv_offline IS SUPPLIED.
      ms_data-offline = iv_offline.
      ls_mask-offline = abap_true.
    ENDIF.

    IF is_dot_abapgit IS SUPPLIED.
      ms_data-dot_abapgit = is_dot_abapgit.
      ls_mask-dot_abapgit = abap_true.
    ENDIF.

    IF is_local_settings IS SUPPLIED.
      ms_data-local_settings = is_local_settings.
      ls_mask-local_settings = abap_true.
    ENDIF.

    IF iv_deserialized_at IS SUPPLIED OR iv_deserialized_by IS SUPPLIED.
      ms_data-deserialized_at = iv_deserialized_at.
      ms_data-deserialized_by = iv_deserialized_by.
      ls_mask-deserialized_at = abap_true.
      ls_mask-deserialized_by = abap_true.
    ENDIF.

    notify_listener( ls_mask ).

  ENDMETHOD.


  METHOD set_dot_abapgit.
    set( is_dot_abapgit = io_dot_abapgit->get_data( ) ).
  ENDMETHOD.


  METHOD set_files_remote.

    mt_remote = it_files.
    mv_request_remote_refresh = abap_false.

  ENDMETHOD.


  METHOD set_local_settings.

    set( is_local_settings = is_settings ).

  ENDMETHOD.


  METHOD status.

    IF lines( mt_status ) = 0.
      mt_status = zcl_abapgit_file_status=>status(
        io_repo = me
        ii_log  = ii_log ).
    ENDIF.

    rt_results = mt_status.

  ENDMETHOD.


  METHOD switch_repo_type.

    IF iv_offline = ms_data-offline.
      zcx_abapgit_exception=>raise( |Cannot switch_repo_type, offline already = "{ ms_data-offline }"| ).
    ENDIF.

    IF iv_offline = abap_true. " On-line -> OFFline
      set(
        iv_url         = zcl_abapgit_url=>name( ms_data-url )
        iv_branch_name = ''
        iv_head_branch = ''
        iv_offline     = abap_true ).
    ELSE. " OFFline -> On-line
      set( iv_offline = abap_false ).
    ENDIF.

  ENDMETHOD.


  METHOD update_last_deserialize.

    DATA: lv_deserialized_at TYPE zif_abapgit_persistence=>ty_repo-deserialized_at,
          lv_deserialized_by TYPE zif_abapgit_persistence=>ty_repo-deserialized_by.

    GET TIME STAMP FIELD lv_deserialized_at.
    lv_deserialized_by = sy-uname.

    set( iv_deserialized_at = lv_deserialized_at
         iv_deserialized_by = lv_deserialized_by ).

  ENDMETHOD.


  METHOD update_local_checksums.

    " ASSUMTION: SHA1 in param is actual and correct.
    " Push fills it from local files before pushing, deserialize from remote
    " If this is not true that there is an error somewhere but not here

    DATA: lt_checksums TYPE zif_abapgit_persistence=>ty_local_checksum_tt,
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

  ENDMETHOD.
ENDCLASS.
