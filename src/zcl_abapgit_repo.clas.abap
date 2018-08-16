CLASS zcl_abapgit_repo DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_abapgit_repo_srv .

  PUBLIC SECTION.

    METHODS deserialize_checks
      RETURNING
        VALUE(rs_checks) TYPE zif_abapgit_definitions=>ty_deserialize_checks
      RAISING
        zcx_abapgit_exception .
    METHODS delete_checks
      RETURNING
        VALUE(rs_checks) TYPE zif_abapgit_definitions=>ty_delete_checks
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      IMPORTING
        !is_data TYPE zif_abapgit_persistence=>ty_repo .
    METHODS get_key
      RETURNING
        VALUE(rv_key) TYPE zif_abapgit_persistence=>ty_value .
    METHODS get_name
      RETURNING
        VALUE(rv_name) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS get_files_local
      IMPORTING
        !io_log         TYPE REF TO zcl_abapgit_log OPTIONAL
        !it_filter      TYPE zif_abapgit_definitions=>ty_tadir_tt OPTIONAL
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_item_tt
      RAISING
        zcx_abapgit_exception .
    METHODS get_local_checksums
      RETURNING
        VALUE(rt_checksums) TYPE zif_abapgit_persistence=>ty_local_checksum_tt .
    METHODS get_local_checksums_per_file
      RETURNING
        VALUE(rt_checksums) TYPE zif_abapgit_definitions=>ty_file_signatures_tt .
    METHODS get_files_remote
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception .
    METHODS get_package
      RETURNING
        VALUE(rv_package) TYPE zif_abapgit_persistence=>ty_repo-package .
    METHODS delete
      RAISING
        zcx_abapgit_exception .
    METHODS get_dot_abapgit
      RETURNING
        VALUE(ro_dot_abapgit) TYPE REF TO zcl_abapgit_dot_abapgit .
    METHODS set_dot_abapgit
      IMPORTING
        !io_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize
      IMPORTING
        !is_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks
      RAISING
        zcx_abapgit_exception .
    METHODS refresh
      IMPORTING
        !iv_drop_cache TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception .
    METHODS update_local_checksums
      IMPORTING
        !it_files TYPE zif_abapgit_definitions=>ty_file_signatures_tt
      RAISING
        zcx_abapgit_exception .
    METHODS rebuild_local_checksums
      RAISING
        zcx_abapgit_exception .
    METHODS find_remote_dot_abapgit
      RETURNING
        VALUE(ro_dot) TYPE REF TO zcl_abapgit_dot_abapgit
      RAISING
        zcx_abapgit_exception .
    METHODS is_offline
      RETURNING
        VALUE(rv_offline) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS set_files_remote
      IMPORTING
        !it_files TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception .
    METHODS get_local_settings
      RETURNING
        VALUE(rs_settings) TYPE zif_abapgit_persistence=>ty_repo-local_settings .
    METHODS set_local_settings
      IMPORTING
        !is_settings TYPE zif_abapgit_persistence=>ty_repo-local_settings
      RAISING
        zcx_abapgit_exception .
    METHODS run_code_inspector
      RETURNING
        VALUE(rt_list) TYPE scit_alvlist
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
    DATA mt_local TYPE zif_abapgit_definitions=>ty_files_item_tt .
    DATA mt_remote TYPE zif_abapgit_definitions=>ty_files_tt .
    DATA mv_do_local_refresh TYPE abap_bool .
    DATA mv_last_serialization TYPE timestamp .
    DATA ms_data TYPE zif_abapgit_persistence=>ty_repo .
    DATA mv_code_inspector_successful TYPE abap_bool .

    METHODS set
      IMPORTING
        !it_checksums       TYPE zif_abapgit_persistence=>ty_local_checksum_tt OPTIONAL
        !iv_url             TYPE zif_abapgit_persistence=>ty_repo-url OPTIONAL
        !iv_branch_name     TYPE zif_abapgit_persistence=>ty_repo-branch_name OPTIONAL
        !iv_head_branch     TYPE zif_abapgit_persistence=>ty_repo-head_branch OPTIONAL
        !iv_offline         TYPE zif_abapgit_persistence=>ty_repo-offline OPTIONAL
        !is_dot_abapgit     TYPE zif_abapgit_persistence=>ty_repo-dot_abapgit OPTIONAL
        !is_local_settings  TYPE zif_abapgit_persistence=>ty_repo-local_settings OPTIONAL
        !iv_deserialized_at TYPE zif_abapgit_persistence=>ty_repo-deserialized_at OPTIONAL
        !iv_deserialized_by TYPE zif_abapgit_persistence=>ty_repo-deserialized_by OPTIONAL
      RAISING
        zcx_abapgit_exception .

  PRIVATE SECTION.
    METHODS:
      update_last_deserialize
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_REPO IMPLEMENTATION.


  METHOD constructor.

    ASSERT NOT is_data-key IS INITIAL.

    ms_data = is_data.

  ENDMETHOD.                    "constructor


  METHOD delete.

    zcl_abapgit_persist_factory=>get_repo( )->delete( ms_data-key ).

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
      zcx_abapgit_exception=>raise( 'Requirements not met and undecided ').
    ENDIF.

    IF is_checks-transport-required = abap_true AND is_checks-transport-transport IS INITIAL.
      zcx_abapgit_exception=>raise( |No transport request was supplied| ).
    ENDIF.

    TRY.
        lt_updated_files = zcl_abapgit_objects=>deserialize(
            io_repo   = me
            is_checks = is_checks ).
      CATCH zcx_abapgit_exception INTO lx_error.
* ensure to reset default transport request task
        zcl_abapgit_default_transport=>get_instance( )->reset( ).
        RAISE EXCEPTION lx_error.
    ENDTRY.

    APPEND get_dot_abapgit( )->get_signature( ) TO lt_updated_files.

    CLEAR: mt_local, mv_last_serialization.

    update_local_checksums( lt_updated_files ).
    update_last_deserialize( ).

  ENDMETHOD.


  METHOD deserialize_checks.

    DATA: lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt.


    find_remote_dot_abapgit( ).

    IF get_local_settings( )-write_protected = abap_true.
      zcx_abapgit_exception=>raise( 'Cannot deserialize. Local code is write-protected by repo config' ).
    ELSEIF get_dot_abapgit( )->get_master_language( ) <> sy-langu.
      zcx_abapgit_exception=>raise( 'Current login language does not match master language' ).
    ENDIF.

    rs_checks = zcl_abapgit_objects=>deserialize_checks( me ).

    lt_requirements = get_dot_abapgit( )->get_data( )-requirements.
    rs_checks-requirements-met = zcl_abapgit_requirement_helper=>is_requirements_met(
      lt_requirements ).

  ENDMETHOD.


  METHOD find_remote_dot_abapgit.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF mt_remote.


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

    DATA: lt_tadir    TYPE zif_abapgit_definitions=>ty_tadir_tt,
          ls_item     TYPE zif_abapgit_definitions=>ty_item,
          lt_files    TYPE zif_abapgit_definitions=>ty_files_tt,
          lo_progress TYPE REF TO zcl_abapgit_progress,
          lt_cache    TYPE SORTED TABLE OF zif_abapgit_definitions=>ty_file_item
                   WITH NON-UNIQUE KEY item.

    DATA: lt_filter       TYPE SORTED TABLE OF zif_abapgit_definitions=>ty_tadir
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
    <ls_return>-file-path     = zif_abapgit_definitions=>c_root_dir.
    <ls_return>-file-filename = zif_abapgit_definitions=>c_dot_abapgit.
    <ls_return>-file-data     = get_dot_abapgit( )->serialize( ).
    <ls_return>-file-sha1     = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>c_type-blob
                                                        iv_data = <ls_return>-file-data ).

    lt_cache = mt_local.
    lt_tadir = zcl_abapgit_factory=>get_tadir( )->read(
      iv_package            = get_package( )
      iv_ignore_subpackages = get_local_settings( )-ignore_subpackages
      iv_only_local_objects = get_local_settings( )-only_local_objects
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
        iv_language = get_dot_abapgit( )->get_master_language( )
        io_log      = io_log ).
      LOOP AT lt_files ASSIGNING <ls_file>.
        <ls_file>-path = <ls_tadir>-path.

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

    FIELD-SYMBOLS <ls_object> LIKE LINE OF ms_data-local_checksums.

    LOOP AT ms_data-local_checksums ASSIGNING <ls_object>.
      APPEND LINES OF <ls_object>-files TO rt_checksums.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_local_settings.

    rs_settings = ms_data-local_settings.

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


  METHOD is_offline.
    rv_offline = ms_data-offline.
  ENDMETHOD.


  METHOD rebuild_local_checksums. "LOCAL (BASE)

    DATA: lt_local     TYPE zif_abapgit_definitions=>ty_files_item_tt,
          ls_last_item TYPE zif_abapgit_definitions=>ty_item,
          lt_checksums TYPE zif_abapgit_persistence=>ty_local_checksum_tt.

    FIELD-SYMBOLS: <ls_checksum> LIKE LINE OF lt_checksums,
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

  ENDMETHOD.  " rebuild_local_checksums.


  METHOD refresh.

    mv_do_local_refresh = abap_true.

    IF iv_drop_cache = abap_true.
      CLEAR: mv_last_serialization, mt_local.
    ENDIF.

  ENDMETHOD.                    "refresh


  METHOD run_code_inspector.

    DATA: li_code_inspector TYPE REF TO zif_abapgit_code_inspector,
          lv_check_variant  TYPE string.

    lv_check_variant = get_local_settings( )-code_inspector_check_variant.

    IF lv_check_variant IS INITIAL.
      zcx_abapgit_exception=>raise( |No check variant maintained in repo settings.| ).
    ENDIF.

    li_code_inspector = zcl_abapgit_factory=>get_code_inspector(
                                  iv_package            = get_package( )
                                  iv_check_variant_name = |{ lv_check_variant }| ).

    rt_list = li_code_inspector->run( ).

    DELETE rt_list WHERE kind = 'N'.

    READ TABLE rt_list TRANSPORTING NO FIELDS
                       WITH KEY kind = 'E'.

    mv_code_inspector_successful = boolc( sy-subrc <> 0 ).

  ENDMETHOD.


  METHOD set.

* TODO: refactor

    DATA: li_persistence TYPE REF TO zif_abapgit_persist_repo.


    ASSERT it_checksums IS SUPPLIED
      OR iv_url IS SUPPLIED
      OR iv_branch_name IS SUPPLIED
      OR iv_head_branch IS SUPPLIED
      OR iv_offline IS SUPPLIED
      OR is_dot_abapgit IS SUPPLIED
      OR is_local_settings IS SUPPLIED
      OR iv_deserialized_by IS SUPPLIED
      OR iv_deserialized_at IS SUPPLIED.

    li_persistence = zcl_abapgit_persist_factory=>get_repo( ).

    IF it_checksums IS SUPPLIED.
      li_persistence->update_local_checksums(
        iv_key       = ms_data-key
        it_checksums = it_checksums ).
      ms_data-local_checksums = it_checksums.
    ENDIF.

    IF iv_url IS SUPPLIED.
      li_persistence->update_url(
        iv_key = ms_data-key
        iv_url = iv_url ).
      ms_data-url = iv_url.
    ENDIF.

    IF iv_branch_name IS SUPPLIED.
      li_persistence->update_branch_name(
        iv_key         = ms_data-key
        iv_branch_name = iv_branch_name ).
      ms_data-branch_name = iv_branch_name.
    ENDIF.

    IF iv_head_branch IS SUPPLIED.
      li_persistence->update_head_branch(
        iv_key         = ms_data-key
        iv_head_branch = iv_head_branch ).
      ms_data-head_branch = iv_head_branch.
    ENDIF.

    IF iv_offline IS SUPPLIED.
      li_persistence->update_offline(
        iv_key     = ms_data-key
        iv_offline = iv_offline ).
      ms_data-offline = iv_offline.
    ENDIF.

    IF is_dot_abapgit IS SUPPLIED.
      li_persistence->update_dot_abapgit(
        iv_key         = ms_data-key
        is_dot_abapgit = is_dot_abapgit ).
      ms_data-dot_abapgit = is_dot_abapgit.
    ENDIF.

    IF is_local_settings IS SUPPLIED.
      li_persistence->update_local_settings(
        iv_key      = ms_data-key
        is_settings = is_local_settings ).
      ms_data-local_settings = is_local_settings.
    ENDIF.

    IF iv_deserialized_at IS SUPPLIED OR iv_deserialized_by IS SUPPLIED.
      li_persistence->update_deserialized(
        iv_key             = ms_data-key
        iv_deserialized_at = iv_deserialized_at
        iv_deserialized_by = iv_deserialized_by ).
      ms_data-deserialized_at = iv_deserialized_at.
    ENDIF.

  ENDMETHOD.


  METHOD set_dot_abapgit.
    set( is_dot_abapgit = io_dot_abapgit->get_data( ) ).
  ENDMETHOD.


  METHOD set_files_remote.

    mt_remote = it_files.

  ENDMETHOD.


  METHOD set_local_settings.

    set( is_local_settings = is_settings ).

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

  ENDMETHOD.  " update_local_checksums
ENDCLASS.
