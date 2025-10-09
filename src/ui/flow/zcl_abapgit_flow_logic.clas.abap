CLASS zcl_abapgit_flow_logic DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS get
      RETURNING
        VALUE(rs_information) TYPE zif_abapgit_flow_logic=>ty_information
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS consolidate
      IMPORTING
        ii_online             TYPE REF TO zif_abapgit_repo_online
      RETURNING
        VALUE(rs_consolidate) TYPE zif_abapgit_flow_logic=>ty_consolidate
      RAISING
        zcx_abapgit_exception.

    TYPES ty_repos_tt TYPE STANDARD TABLE OF REF TO zif_abapgit_repo_online WITH DEFAULT KEY.

    CLASS-METHODS list_repos
      IMPORTING
        iv_favorites_only TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rt_repos)   TYPE ty_repos_tt
      RAISING
        zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_transport,
             trkorr   TYPE trkorr,
             title    TYPE string,
             object   TYPE e071-object,
             obj_name TYPE e071-obj_name,
             devclass TYPE tadir-devclass,
           END OF ty_transport.

    TYPES ty_transports_tt TYPE STANDARD TABLE OF ty_transport WITH DEFAULT KEY.

    TYPES ty_trkorr_tt TYPE STANDARD TABLE OF trkorr WITH DEFAULT KEY.

    CLASS-METHODS consolidate_files
      IMPORTING
        ii_online      TYPE REF TO zif_abapgit_repo_online
      CHANGING
        cs_information TYPE zif_abapgit_flow_logic=>ty_consolidate
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS check_files
      IMPORTING
        it_local          TYPE zif_abapgit_definitions=>ty_files_item_tt
        it_features       TYPE zif_abapgit_flow_logic=>ty_features
      CHANGING
        ct_main_expanded  TYPE zif_abapgit_git_definitions=>ty_expanded_tt
        ct_missing_remote TYPE zif_abapgit_flow_logic=>ty_consolidate-missing_remote
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS build_repo_data
      IMPORTING
        ii_repo        TYPE REF TO zif_abapgit_repo
      RETURNING
        VALUE(rs_data) TYPE zif_abapgit_flow_logic=>ty_feature-repo.

    CLASS-METHODS try_matching_transports
      IMPORTING
        ii_repo          TYPE REF TO zif_abapgit_repo
        it_main_expanded TYPE zif_abapgit_git_definitions=>ty_expanded_tt
        it_local         TYPE zif_abapgit_definitions=>ty_files_item_tt
      CHANGING
        ct_features      TYPE zif_abapgit_flow_logic=>ty_features
        ct_transports    TYPE ty_transports_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS errors_from_transports
      IMPORTING
        it_transports  TYPE ty_transports_tt
      CHANGING
        cs_information TYPE zif_abapgit_flow_logic=>ty_information.

    CLASS-METHODS add_objects_and_files_from_tr
      IMPORTING
        iv_trkorr        TYPE trkorr
        it_transports    TYPE ty_transports_tt
        it_local         TYPE zif_abapgit_definitions=>ty_files_item_tt
        it_main_expanded TYPE zif_abapgit_git_definitions=>ty_expanded_tt
      CHANGING
        cs_feature       TYPE zif_abapgit_flow_logic=>ty_feature
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS find_up_to_date
      IMPORTING
        iv_url      TYPE string
        it_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
      CHANGING
        ct_features TYPE zif_abapgit_flow_logic=>ty_features
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS find_prs
      IMPORTING
        iv_url      TYPE string
      CHANGING
        ct_features TYPE zif_abapgit_flow_logic=>ty_features
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS add_local_status
      IMPORTING
        it_local    TYPE zif_abapgit_definitions=>ty_files_item_tt
      CHANGING
        ct_features TYPE zif_abapgit_flow_logic=>ty_features
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS relevant_transports_via_devc
      IMPORTING
        ii_repo              TYPE REF TO zif_abapgit_repo
        it_transports        TYPE ty_transports_tt
      RETURNING
        VALUE(rt_transports) TYPE ty_trkorr_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS find_open_transports
      RETURNING
        VALUE(rt_transports) TYPE ty_transports_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS serialize_filtered
      IMPORTING
        it_relevant_transports TYPE ty_trkorr_tt
        ii_repo                TYPE REF TO zif_abapgit_repo
        it_all_transports      TYPE ty_transports_tt
        it_features            TYPE zif_abapgit_flow_logic=>ty_features
      RETURNING
        VALUE(rt_local)        TYPE zif_abapgit_definitions=>ty_files_item_tt
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_FLOW_LOGIC IMPLEMENTATION.


  METHOD add_local_status.

    FIELD-SYMBOLS <ls_branch>       LIKE LINE OF ct_features.
    FIELD-SYMBOLS <ls_local>        LIKE LINE OF it_local.
    FIELD-SYMBOLS <ls_changed_file> TYPE zif_abapgit_flow_logic=>ty_path_name.


    LOOP AT ct_features ASSIGNING <ls_branch>.
      LOOP AT <ls_branch>-changed_files ASSIGNING <ls_changed_file>.
        READ TABLE it_local ASSIGNING <ls_local>
          WITH KEY file-filename = <ls_changed_file>-filename
          file-path = <ls_changed_file>-path.
        IF sy-subrc = 0.
          <ls_changed_file>-local_sha1 = <ls_local>-file-sha1.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD add_objects_and_files_from_tr.

    DATA ls_changed      LIKE LINE OF cs_feature-changed_objects.
    DATA ls_changed_file LIKE LINE OF cs_feature-changed_files.
    DATA ls_item         TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_filename     TYPE string.
    DATA lv_extension    TYPE string.
    DATA lv_main_file    TYPE string.


    FIELD-SYMBOLS <ls_transport> LIKE LINE OF it_transports.
    FIELD-SYMBOLS <ls_local>     LIKE LINE OF it_local.
    FIELD-SYMBOLS <ls_main_expanded> LIKE LINE OF it_main_expanded.


    LOOP AT it_transports ASSIGNING <ls_transport> WHERE trkorr = iv_trkorr.
      ls_changed-obj_type = <ls_transport>-object.
      ls_changed-obj_name = <ls_transport>-obj_name.
      INSERT ls_changed INTO TABLE cs_feature-changed_objects.

      LOOP AT it_local ASSIGNING <ls_local>
          WHERE file-filename <> zif_abapgit_definitions=>c_dot_abapgit
          AND item-obj_type = <ls_transport>-object
          AND item-obj_name = <ls_transport>-obj_name.

        CLEAR ls_changed_file.
        ls_changed_file-path       = <ls_local>-file-path.
        ls_changed_file-filename   = <ls_local>-file-filename.
        ls_changed_file-local_sha1 = <ls_local>-file-sha1.

        READ TABLE it_main_expanded ASSIGNING <ls_main_expanded>
          WITH TABLE KEY path_name COMPONENTS
          path = ls_changed_file-path
          name = ls_changed_file-filename.
        IF sy-subrc = 0.
          ls_changed_file-remote_sha1 = <ls_main_expanded>-sha1.
        ENDIF.

        INSERT ls_changed_file INTO TABLE cs_feature-changed_files.
      ENDLOOP.
      IF sy-subrc <> 0.
* then its a deletion
        CLEAR ls_item.
        ls_item-obj_type = <ls_transport>-object.
        ls_item-obj_name = <ls_transport>-obj_name.

        IF zcl_abapgit_aff_factory=>get_registry( )->is_supported_object_type( <ls_transport>-object ) = abap_true.
          lv_extension = 'json'.
        ELSE.
          lv_extension = 'xml'.
        ENDIF.

        lv_main_file = zcl_abapgit_filename_logic=>object_to_file(
          is_item = ls_item
          iv_ext  = lv_extension ).
        CONCATENATE '.' lv_extension INTO lv_extension.
        lv_filename = lv_main_file.
        REPLACE FIRST OCCURRENCE OF lv_extension IN lv_filename WITH '*'.

        LOOP AT it_main_expanded ASSIGNING <ls_main_expanded>
            WHERE name CP lv_filename.
          CLEAR ls_changed_file.
          ls_changed_file-filename    = <ls_main_expanded>-name.
          ls_changed_file-path        = <ls_main_expanded>-path.
          ls_changed_file-remote_sha1 = <ls_main_expanded>-sha1.
          INSERT ls_changed_file INTO TABLE cs_feature-changed_files.
        ENDLOOP.
        IF sy-subrc <> 0.
          CLEAR ls_changed_file.
          ls_changed_file-filename    = lv_main_file.
          ls_changed_file-path        = '/src/'. " todo?
* after its deleted locally and remote then remote and local sha1 will match(be empty)
          INSERT ls_changed_file INTO TABLE cs_feature-changed_files.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD build_repo_data.
    rs_data-name = ii_repo->get_name( ).
    rs_data-key = ii_repo->get_key( ).
    rs_data-package = ii_repo->get_package( ).
  ENDMETHOD.


  METHOD check_files.

    DATA ls_missing      LIKE LINE OF ct_missing_remote.
    DATA lv_found_main   TYPE abap_bool.
    DATA ls_feature      LIKE LINE OF it_features.
    DATA lv_found_branch TYPE abap_bool.

    FIELD-SYMBOLS <ls_local> LIKE LINE OF it_local.
    FIELD-SYMBOLS <ls_expanded> LIKE LINE OF ct_main_expanded.

    LOOP AT it_local ASSIGNING <ls_local> WHERE file-filename <> zif_abapgit_definitions=>c_dot_abapgit.
      READ TABLE ct_main_expanded WITH KEY name = <ls_local>-file-filename ASSIGNING <ls_expanded>.
      lv_found_main = boolc( sy-subrc = 0 ).

      lv_found_branch = abap_false.
      LOOP AT it_features INTO ls_feature.
        READ TABLE ls_feature-changed_files TRANSPORTING NO FIELDS
          WITH KEY filename = <ls_local>-file-filename.
        IF sy-subrc = 0.
          lv_found_branch = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_found_main = abap_false AND lv_found_branch = abap_false.
        CLEAR ls_missing.
        ls_missing-path = <ls_local>-file-path.
        ls_missing-filename = <ls_local>-file-filename.
        ls_missing-local_sha1 = <ls_local>-file-sha1.
        INSERT ls_missing INTO TABLE ct_missing_remote.
      ELSEIF lv_found_branch = abap_false AND <ls_expanded>-path <> <ls_local>-file-path.
* todo
      ELSEIF lv_found_branch = abap_false AND <ls_expanded>-sha1 <> <ls_local>-file-sha1.
        CLEAR ls_missing.
        ls_missing-path = <ls_local>-file-path.
        ls_missing-filename = <ls_local>-file-filename.
        ls_missing-local_sha1 = <ls_local>-file-sha1.
        ls_missing-remote_sha1 = <ls_expanded>-sha1.
        INSERT ls_missing INTO TABLE ct_missing_remote.
      ENDIF.

      IF lv_found_main = abap_true OR lv_found_branch = abap_true.
        DELETE ct_main_expanded WHERE name = <ls_local>-file-filename AND path = <ls_local>-file-path.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD consolidate.

    DATA lt_features  TYPE zif_abapgit_flow_logic=>ty_features.
    DATA ls_feature   LIKE LINE OF lt_features.
    DATA lv_string    TYPE string.
    DATA li_repo      TYPE REF TO zif_abapgit_repo.
    DATA ls_transport TYPE zif_abapgit_cts_api=>ty_transport_data.


* todo: handling multiple repositories

    li_repo ?= ii_online.
    lt_features = get( )-features.

    LOOP AT lt_features INTO ls_feature WHERE repo-key = li_repo->get_key( ).
      IF ls_feature-branch-display_name IS NOT INITIAL
          AND ls_feature-branch-up_to_date = abap_false.
        lv_string = |Branch <tt>{ ls_feature-branch-display_name }</tt> is not up to date|.
        INSERT lv_string INTO TABLE rs_consolidate-errors.
      ELSEIF ls_feature-branch-display_name IS NOT INITIAL
          AND ls_feature-transport-trkorr IS INITIAL
          AND lines( ls_feature-changed_files ) > 0.
* its okay if the changes are outside the starting folder
        lv_string = |Branch <tt>{ ls_feature-branch-display_name }</tt> has no transport|.
        INSERT lv_string INTO TABLE rs_consolidate-errors.
      ELSEIF ls_feature-transport-trkorr IS NOT INITIAL
          AND ls_feature-branch-display_name IS INITIAL
          AND ls_feature-full_match = abap_false.
        ls_transport = zcl_abapgit_factory=>get_cts_api( )->read( ls_feature-transport-trkorr ).
        lv_string = |Transport <tt>{ ls_feature-transport-trkorr }</tt> has no branch, created {
          ls_transport-as4date DATE = ISO }|.
        INSERT lv_string INTO TABLE rs_consolidate-errors.
      ENDIF.
* todo: branches without pull requests?
    ENDLOOP.

    consolidate_files(
      EXPORTING
        ii_online      = ii_online
      CHANGING
        cs_information = rs_consolidate ).

  ENDMETHOD.


  METHOD consolidate_files.

    DATA lt_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.
    DATA lt_tadir    TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lt_filter   TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lo_filter   TYPE REF TO zcl_abapgit_object_filter_obj.
    DATA lt_local    TYPE zif_abapgit_definitions=>ty_files_item_tt.
    DATA lt_features TYPE zif_abapgit_flow_logic=>ty_features.
    DATA li_repo     TYPE REF TO zif_abapgit_repo.
    DATA lt_main_expanded TYPE zif_abapgit_git_definitions=>ty_expanded_tt.
    DATA ls_expanded LIKE LINE OF lt_main_expanded.
    DATA ls_branch   LIKE LINE OF lt_branches.
    DATA ls_only_remote TYPE zif_abapgit_flow_logic=>ty_path_name.
    DATA ls_result   LIKE LINE OF lt_features.

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF lt_tadir.

    li_repo ?= ii_online.

* find all that exists local, serialize these, skip if no changes or if in any branch
    lt_branches = zcl_abapgit_git_factory=>get_v2_porcelain( )->list_branches(
      iv_url    = ii_online->get_url( )
      iv_prefix = zif_abapgit_git_definitions=>c_git_branch-heads_prefix )->get_all( ).

    CLEAR lt_features.
    LOOP AT lt_branches INTO ls_branch WHERE display_name <> zif_abapgit_flow_logic=>c_main.
      ls_result-repo = build_repo_data( ii_online ).
      ls_result-branch-display_name = ls_branch-display_name.
      ls_result-branch-sha1 = ls_branch-sha1.
      INSERT ls_result INTO TABLE lt_features.
    ENDLOOP.

    zcl_abapgit_flow_git=>find_changes_in_git(
      EXPORTING
        ii_repo_online   = ii_online
        it_branches      = lt_branches
      IMPORTING
        et_main_expanded = lt_main_expanded
      CHANGING
        ct_features      = lt_features ).

    lt_tadir = zcl_abapgit_factory=>get_tadir( )->read(
      iv_package      = li_repo->get_package( )
      io_dot          = li_repo->get_dot_abapgit( )
      iv_check_exists = abap_true ).

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      INSERT <ls_tadir> INTO TABLE lt_filter.

      IF lines( lt_filter ) >= 500.
        CREATE OBJECT lo_filter EXPORTING it_filter = lt_filter.
        lt_local = li_repo->get_files_local_filtered( lo_filter ).
        CLEAR lt_filter.
        check_files(
          EXPORTING
            it_local          = lt_local
            it_features       = lt_features
          CHANGING
            ct_main_expanded  = lt_main_expanded
            ct_missing_remote = cs_information-missing_remote ).
        IF lines( cs_information-missing_remote ) > 1000.
          INSERT `Only first 1000 missing files shown` INTO TABLE cs_information-warnings.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lines( lt_filter ) > 0.
      CREATE OBJECT lo_filter EXPORTING it_filter = lt_filter.
      lt_local = li_repo->get_files_local_filtered( lo_filter ).
      CLEAR lt_filter.
      check_files(
        EXPORTING
          it_local          = lt_local
          it_features       = lt_features
        CHANGING
          ct_main_expanded  = lt_main_expanded
          ct_missing_remote = cs_information-missing_remote ).
    ENDIF.

* todo: double check, there might have been changes while consolidation is running
* or do smaller batches?

* those left in lt_main_expanded are only in remote, not local
    LOOP AT lt_main_expanded INTO ls_expanded.
      CLEAR ls_only_remote.
      ls_only_remote-path = ls_expanded-path.
      ls_only_remote-filename = ls_expanded-name.
      ls_only_remote-remote_sha1 = ls_expanded-sha1.
      INSERT ls_only_remote INTO TABLE cs_information-only_remote.
    ENDLOOP.

  ENDMETHOD.


  METHOD errors_from_transports.

    DATA lv_message    TYPE string.
    DATA lt_transports LIKE it_transports.
    DATA lv_index      TYPE i.
    DATA ls_next       LIKE LINE OF lt_transports.
    DATA ls_transport  LIKE LINE OF lt_transports.
    DATA ls_duplicate  LIKE LINE OF cs_information-transport_duplicates.

    lt_transports = it_transports.
    SORT lt_transports BY object obj_name trkorr.

    LOOP AT lt_transports INTO ls_transport.
      lv_index = sy-tabix + 1.
      READ TABLE lt_transports INTO ls_next INDEX lv_index.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF ls_next-object = ls_transport-object
          AND ls_next-trkorr <> ls_transport-trkorr
          AND ls_next-obj_name = ls_transport-obj_name.
        lv_message = |Object <tt>{ ls_transport-object }</tt> <tt>{ ls_transport-obj_name
          }</tt> is in multiple transports: <tt>{ ls_transport-trkorr }</tt> and <tt>{ ls_next-trkorr }</tt>|.
        INSERT lv_message INTO TABLE cs_information-errors.

        CLEAR ls_duplicate.
        ls_duplicate-obj_type = ls_transport-object.
        ls_duplicate-obj_name = ls_transport-obj_name.
        INSERT ls_duplicate INTO TABLE cs_information-transport_duplicates.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD find_open_transports.

    DATA lt_trkorr   TYPE zif_abapgit_cts_api=>ty_trkorr_tt.
    DATA lv_trkorr   LIKE LINE OF lt_trkorr.
    DATA ls_result   LIKE LINE OF rt_transports.
    DATA lt_objects  TYPE zif_abapgit_cts_api=>ty_transport_obj_tt.
    DATA lv_obj_name TYPE tadir-obj_name.
    DATA lt_date     TYPE zif_abapgit_cts_api=>ty_date_range.
    DATA ls_date     LIKE LINE OF lt_date.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF lt_objects.

* only look for transports that are created/changed in the last two years
    ls_date-sign = 'I'.
    ls_date-option = 'GE'.
    ls_date-low = sy-datum - 730.
    INSERT ls_date INTO TABLE lt_date.

    lt_trkorr = zcl_abapgit_factory=>get_cts_api( )->list_open_requests( it_date = lt_date ).

    LOOP AT lt_trkorr INTO lv_trkorr.
      ls_result-trkorr = lv_trkorr.
      ls_result-title  = zcl_abapgit_factory=>get_cts_api( )->read_description( lv_trkorr ).

      lt_objects = zcl_abapgit_factory=>get_cts_api( )->list_r3tr_by_request( lv_trkorr ).
      LOOP AT lt_objects ASSIGNING <ls_object>.
        ls_result-object   = <ls_object>-object.
        ls_result-obj_name = <ls_object>-obj_name.

        lv_obj_name = <ls_object>-obj_name.
        ls_result-devclass = zcl_abapgit_factory=>get_tadir( )->read_single(
          iv_object   = ls_result-object
          iv_obj_name = lv_obj_name )-devclass.
        IF ls_result-devclass IS NOT INITIAL.
          INSERT ls_result INTO TABLE rt_transports.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD find_prs.

    DATA lt_pulls TYPE zif_abapgit_pr_enum_provider=>ty_pull_requests.
    DATA ls_pull LIKE LINE OF lt_pulls.

    FIELD-SYMBOLS <ls_branch> LIKE LINE OF ct_features.


    IF lines( ct_features ) = 0.
      " only main branch
      RETURN.
    ENDIF.

    lt_pulls = zcl_abapgit_pr_enumerator=>new( iv_url )->get_pulls( ).

    LOOP AT ct_features ASSIGNING <ls_branch>.
      READ TABLE lt_pulls INTO ls_pull WITH KEY head_branch = <ls_branch>-branch-display_name.
      IF sy-subrc = 0.
        " remove markdown formatting,
        REPLACE ALL OCCURRENCES OF '`' IN ls_pull-title WITH ''.

        <ls_branch>-pr-title = |{ ls_pull-title } #{ ls_pull-number }|.
        <ls_branch>-pr-url = ls_pull-html_url.
        <ls_branch>-pr-number = ls_pull-number.
        <ls_branch>-pr-draft = ls_pull-draft.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD find_up_to_date.

    DATA ls_branch  LIKE LINE OF it_branches.
    DATA lt_commits TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA ls_main    LIKE LINE OF it_branches.
    DATA lv_current TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA lt_sha1    TYPE zif_abapgit_git_definitions=>ty_sha1_tt.
    DATA lo_visit   TYPE REF TO lcl_sha1_stack.
    DATA ls_raw     TYPE zcl_abapgit_git_pack=>ty_commit.

    DATA lt_main_reachable TYPE HASHED TABLE OF zif_abapgit_git_definitions=>ty_sha1 WITH UNIQUE KEY table_line.

    FIELD-SYMBOLS <ls_branch> LIKE LINE OF ct_features.
    FIELD-SYMBOLS <ls_commit> LIKE LINE OF lt_commits.


    IF lines( it_branches ) = 1.
      " only main branch
      RETURN.
    ENDIF.

    READ TABLE it_branches INTO ls_main WITH KEY display_name = zif_abapgit_flow_logic=>c_main.
    ASSERT sy-subrc = 0.

    LOOP AT it_branches INTO ls_branch WHERE is_head = abap_false.
      APPEND ls_branch-sha1 TO lt_sha1.
    ENDLOOP.

    lt_commits = zcl_abapgit_git_factory=>get_v2_porcelain( )->commits_last_year(
      iv_url  = iv_url
      it_sha1 = lt_sha1 ).

    CREATE OBJECT lo_visit.
    lo_visit->clear( )->push( ls_main-sha1 ).
    WHILE lo_visit->size( ) > 0.
      lv_current = lo_visit->pop( ).
      INSERT lv_current INTO TABLE lt_main_reachable.
      READ TABLE lt_commits ASSIGNING <ls_commit> WITH TABLE KEY sha COMPONENTS sha1 = lv_current.
      IF sy-subrc = 0.
        ls_raw = zcl_abapgit_git_pack=>decode_commit( <ls_commit>-data ).
        lo_visit->push( ls_raw-parent ).
        IF ls_raw-parent2 IS NOT INITIAL.
          lo_visit->push( ls_raw-parent2 ).
        ENDIF.
      ENDIF.
    ENDWHILE.

    LOOP AT ct_features ASSIGNING <ls_branch>.
      <ls_branch>-branch-up_to_date = abap_undefined.
      lo_visit->clear( )->push( <ls_branch>-branch-sha1 ).

      WHILE lo_visit->size( ) > 0.
        lv_current = lo_visit->pop( ).
        IF lv_current = ls_main-sha1.
          <ls_branch>-branch-up_to_date = abap_true.
          EXIT.
        ENDIF.

        READ TABLE lt_main_reachable WITH KEY table_line = lv_current TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          <ls_branch>-branch-up_to_date = abap_false.
          EXIT.
        ENDIF.

        READ TABLE lt_commits ASSIGNING <ls_commit> WITH TABLE KEY sha COMPONENTS sha1 = lv_current.
        IF sy-subrc = 0.
          ls_raw = zcl_abapgit_git_pack=>decode_commit( <ls_commit>-data ).
          lo_visit->push( ls_raw-parent ).
          IF ls_raw-parent2 IS NOT INITIAL.
            lo_visit->push( ls_raw-parent2 ).
          ENDIF.
        ENDIF.
      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.


  METHOD get.

    DATA lt_branches            TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.
    DATA ls_branch              LIKE LINE OF lt_branches.
    DATA ls_result              LIKE LINE OF rs_information-features.
    DATA li_repo_online         TYPE REF TO zif_abapgit_repo_online.
    DATA lt_features            LIKE rs_information-features.
    DATA lt_all_transports      TYPE ty_transports_tt.
    DATA lt_relevant_transports TYPE ty_trkorr_tt.
    DATA lt_repos               TYPE ty_repos_tt.
    DATA lt_main_expanded       TYPE zif_abapgit_git_definitions=>ty_expanded_tt.
    DATA lt_local               TYPE zif_abapgit_definitions=>ty_files_item_tt.

    FIELD-SYMBOLS <ls_feature>   LIKE LINE OF lt_features.
    FIELD-SYMBOLS <ls_path_name> LIKE LINE OF <ls_feature>-changed_files.

    lt_all_transports = find_open_transports( ).

    errors_from_transports(
      EXPORTING
        it_transports  = lt_all_transports
      CHANGING
        cs_information = rs_information ).

* list branches on favorite + flow enabled + transported repos
    lt_repos = list_repos( ).
    LOOP AT lt_repos INTO li_repo_online.

      lt_branches = zcl_abapgit_git_factory=>get_v2_porcelain( )->list_branches(
        iv_url    = li_repo_online->get_url( )
        iv_prefix = zif_abapgit_git_definitions=>c_git_branch-heads_prefix )->get_all( ).

      CLEAR lt_features.
      LOOP AT lt_branches INTO ls_branch WHERE display_name <> zif_abapgit_flow_logic=>c_main.
        ls_result-repo = build_repo_data( li_repo_online ).
        ls_result-branch-display_name = ls_branch-display_name.
        ls_result-branch-sha1 = ls_branch-sha1.
        INSERT ls_result INTO TABLE lt_features.
      ENDLOOP.

      zcl_abapgit_flow_git=>find_changes_in_git(
        EXPORTING
          ii_repo_online   = li_repo_online
          it_branches      = lt_branches
        IMPORTING
          et_main_expanded = lt_main_expanded
        CHANGING
          ct_features      = lt_features ).

      lt_relevant_transports = relevant_transports_via_devc(
        ii_repo        = li_repo_online
        it_transports  = lt_all_transports ).

      lt_local = serialize_filtered(
        it_relevant_transports = lt_relevant_transports
        ii_repo                = li_repo_online
        it_features            = lt_features
        it_all_transports      = lt_all_transports ).

      try_matching_transports(
        EXPORTING
          ii_repo          = li_repo_online
          it_local         = lt_local
          it_main_expanded = lt_main_expanded
        CHANGING
          ct_transports    = lt_all_transports
          ct_features      = lt_features ).

      find_prs(
        EXPORTING
          iv_url      = li_repo_online->get_url( )
        CHANGING
          ct_features = lt_features ).

      add_local_status(
        EXPORTING
          it_local     = lt_local
        CHANGING
          ct_features = lt_features ).

*********************

      find_up_to_date(
        EXPORTING
          iv_url      = li_repo_online->get_url( )
          it_branches = lt_branches
        CHANGING
          ct_features = lt_features ).

      LOOP AT lt_features ASSIGNING <ls_feature>.
        <ls_feature>-full_match = abap_true.
        LOOP AT <ls_feature>-changed_files ASSIGNING <ls_path_name>.
          IF <ls_path_name>-remote_sha1 <> <ls_path_name>-local_sha1.
            <ls_feature>-full_match = abap_false.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      INSERT LINES OF lt_features INTO TABLE rs_information-features.
    ENDLOOP.

    SORT rs_information-features BY full_match transport-trkorr DESCENDING.

  ENDMETHOD.


  METHOD list_repos.

    DATA lt_repos  TYPE zif_abapgit_repo_srv=>ty_repo_list.
    DATA li_repo   LIKE LINE OF lt_repos.
    DATA li_online TYPE REF TO zif_abapgit_repo_online.

    IF iv_favorites_only = abap_true.
      lt_repos = zcl_abapgit_repo_srv=>get_instance( )->list_favorites( abap_false ).
    ELSE.
      lt_repos = zcl_abapgit_repo_srv=>get_instance( )->list( abap_false ).
    ENDIF.

    LOOP AT lt_repos INTO li_repo.
      IF li_repo->get_local_settings( )-flow = abap_false.
        CONTINUE.
      ELSEIF zcl_abapgit_factory=>get_sap_package( li_repo->get_package( )
          )->are_changes_recorded_in_tr_req( ) = abap_false.
        CONTINUE.
      ENDIF.

      li_online ?= li_repo.
      INSERT li_online INTO TABLE rt_repos.
    ENDLOOP.
  ENDMETHOD.


  METHOD relevant_transports_via_devc.

    DATA ls_trkorr   LIKE LINE OF it_transports.
    DATA lt_packages TYPE zif_abapgit_sap_package=>ty_devclass_tt.
    DATA lv_found    TYPE abap_bool.
    DATA lv_package  LIKE LINE OF lt_packages.
    DATA lt_trkorr   TYPE ty_transports_tt.

    FIELD-SYMBOLS <ls_transport> LIKE LINE OF it_transports.


    lt_trkorr = it_transports.
    SORT lt_trkorr BY trkorr.
    DELETE ADJACENT DUPLICATES FROM lt_trkorr COMPARING trkorr.

    lt_packages = zcl_abapgit_factory=>get_sap_package( ii_repo->get_package( ) )->list_subpackages( ).
    INSERT ii_repo->get_package( ) INTO TABLE lt_packages.

    LOOP AT lt_trkorr INTO ls_trkorr.
      lv_found = abap_false.

      LOOP AT lt_packages INTO lv_package.
        READ TABLE it_transports ASSIGNING <ls_transport> WITH KEY trkorr = ls_trkorr-trkorr devclass = lv_package.
        IF sy-subrc = 0.
          lv_found = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF lv_found = abap_false.
        CONTINUE.
      ENDIF.

      IF lv_found = abap_true.
        INSERT ls_trkorr-trkorr INTO TABLE rt_transports.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_filtered.

    DATA lv_trkorr         TYPE trkorr.
    DATA lt_filter         TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lo_filter         TYPE REF TO zcl_abapgit_object_filter_obj.
    DATA ls_feature        LIKE LINE OF it_features.
    DATA ls_changed_object LIKE LINE OF ls_feature-changed_objects.

    FIELD-SYMBOLS <ls_transport> LIKE LINE OF it_all_transports.
    FIELD-SYMBOLS <ls_filter> LIKE LINE OF lt_filter.

* from all relevant transports(matched via package)
    LOOP AT it_relevant_transports INTO lv_trkorr.
      LOOP AT it_all_transports ASSIGNING <ls_transport> WHERE trkorr = lv_trkorr.
        APPEND INITIAL LINE TO lt_filter ASSIGNING <ls_filter>.
        <ls_filter>-object = <ls_transport>-object.
        <ls_filter>-obj_name = <ls_transport>-obj_name.
      ENDLOOP.
    ENDLOOP.

* and from git
    LOOP AT it_features INTO ls_feature.
      LOOP AT ls_feature-changed_objects INTO ls_changed_object.
        APPEND INITIAL LINE TO lt_filter ASSIGNING <ls_filter>.
        <ls_filter>-object = ls_changed_object-obj_type.
        <ls_filter>-obj_name = ls_changed_object-obj_name.
      ENDLOOP.
    ENDLOOP.

    SORT lt_filter BY object obj_name.
    DELETE ADJACENT DUPLICATES FROM lt_filter COMPARING object obj_name.

    CREATE OBJECT lo_filter EXPORTING it_filter = lt_filter.
    rt_local = ii_repo->get_files_local_filtered( lo_filter ).

  ENDMETHOD.


  METHOD try_matching_transports.

    DATA lt_trkorr       LIKE ct_transports.
    DATA ls_trkorr       LIKE LINE OF lt_trkorr.
    DATA ls_result       LIKE LINE OF ct_features.
    DATA lt_packages     TYPE zif_abapgit_sap_package=>ty_devclass_tt.
    DATA lv_package      LIKE LINE OF lt_packages.
    DATA lv_found        TYPE abap_bool.

    FIELD-SYMBOLS <ls_feature>   LIKE LINE OF ct_features.
    FIELD-SYMBOLS <ls_transport> LIKE LINE OF ct_transports.
    FIELD-SYMBOLS <ls_changed>   LIKE LINE OF <ls_feature>-changed_objects.


    SORT ct_transports BY object obj_name.

    LOOP AT ct_features ASSIGNING <ls_feature>.
      LOOP AT <ls_feature>-changed_objects ASSIGNING <ls_changed>.
        READ TABLE ct_transports ASSIGNING <ls_transport>
          WITH KEY object = <ls_changed>-obj_type obj_name = <ls_changed>-obj_name BINARY SEARCH.
        IF sy-subrc = 0.
          <ls_feature>-transport-trkorr = <ls_transport>-trkorr.
          <ls_feature>-transport-title = <ls_transport>-title.

          add_objects_and_files_from_tr(
            EXPORTING
              iv_trkorr        = <ls_transport>-trkorr
              it_local         = it_local
              it_main_expanded = it_main_expanded
              it_transports    = ct_transports
            CHANGING
              cs_feature       = <ls_feature> ).

          DELETE ct_transports WHERE trkorr = <ls_transport>-trkorr.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

* unmatched transports
    lt_trkorr = ct_transports.
    SORT lt_trkorr BY trkorr.
    DELETE ADJACENT DUPLICATES FROM lt_trkorr COMPARING trkorr.

    lt_packages = zcl_abapgit_factory=>get_sap_package( ii_repo->get_package( ) )->list_subpackages( ).
    INSERT ii_repo->get_package( ) INTO TABLE lt_packages.

    LOOP AT lt_trkorr INTO ls_trkorr.
      lv_found = abap_false.
      LOOP AT lt_packages INTO lv_package.
        READ TABLE ct_transports ASSIGNING <ls_transport> WITH KEY trkorr = ls_trkorr-trkorr devclass = lv_package.
        IF sy-subrc = 0.
          lv_found = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF lv_found = abap_false.
        CONTINUE.
      ENDIF.

      CLEAR ls_result.
      ls_result-repo = build_repo_data( ii_repo ).
      ls_result-transport-trkorr = <ls_transport>-trkorr.
      ls_result-transport-title = <ls_transport>-title.

      add_objects_and_files_from_tr(
        EXPORTING
          iv_trkorr        = ls_trkorr-trkorr
          it_local         = it_local
          it_main_expanded = it_main_expanded
          it_transports    = ct_transports
        CHANGING
          cs_feature       = ls_result ).

      INSERT ls_result INTO TABLE ct_features.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
