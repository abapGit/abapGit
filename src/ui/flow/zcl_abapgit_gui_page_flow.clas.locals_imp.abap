*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_sha1_stack DEFINITION.
  PUBLIC SECTION.
    METHODS clear
      RETURNING
        VALUE(ro_stack) TYPE REF TO lcl_sha1_stack.

    METHODS push
      IMPORTING
        iv_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1.

    METHODS pop
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_git_definitions=>ty_sha1.

    METHODS size
      RETURNING
        VALUE(rv_size) TYPE i.
  PRIVATE SECTION.
    DATA mt_list TYPE STANDARD TABLE OF zif_abapgit_git_definitions=>ty_sha1 WITH DEFAULT KEY.
ENDCLASS.

CLASS lcl_sha1_stack IMPLEMENTATION.
  METHOD clear.
    CLEAR mt_list.
    ro_stack = me.
  ENDMETHOD.

  METHOD push.
    INSERT iv_sha1 INTO mt_list INDEX 1.
  ENDMETHOD.

  METHOD pop.
    READ TABLE mt_list INDEX 1 INTO rv_sha1.
    ASSERT sy-subrc = 0.
    DELETE mt_list INDEX 1.
  ENDMETHOD.

  METHOD size.
    rv_size = lines( mt_list ).
  ENDMETHOD.
ENDCLASS.

***************************************************

CLASS lcl_filter DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_object_filter.

    METHODS constructor
      IMPORTING
        it_filter TYPE zif_abapgit_definitions=>ty_tadir_tt.

  PRIVATE SECTION.
    DATA mt_filter TYPE zif_abapgit_definitions=>ty_tadir_tt.
ENDCLASS.

CLASS lcl_filter IMPLEMENTATION.
  METHOD constructor.
    mt_filter = it_filter.
  ENDMETHOD.

  METHOD zif_abapgit_object_filter~get_filter.
    rt_filter = mt_filter.
  ENDMETHOD.
ENDCLASS.

***************************************************

CLASS lcl_helper DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS get_information
      RETURNING
        VALUE(rt_features) TYPE zif_abapgit_gui_page_flow=>ty_features
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
    CONSTANTS c_main TYPE string VALUE 'main'.

    TYPES: BEGIN OF ty_transport,
             trkorr   TYPE trkorr,
             title    TYPE string,
             object   TYPE e071-object,
             obj_name TYPE e071-obj_name,
             devclass TYPE tadir-devclass,
           END OF ty_transport.

    TYPES ty_transports_tt TYPE STANDARD TABLE OF ty_transport WITH DEFAULT KEY.

    CLASS-METHODS build_repo_data
      IMPORTING
        io_online      TYPE REF TO zif_abapgit_repo
      RETURNING
        VALUE(rs_data) TYPE zif_abapgit_gui_page_flow=>ty_feature-repo.

    CLASS-METHODS map_files_to_objects
      IMPORTING
        it_files                  TYPE zif_abapgit_gui_page_flow=>ty_path_name_tt
        io_online                 TYPE REF TO zcl_abapgit_repo_online
      RETURNING
        VALUE(rt_changed_objects) TYPE zif_abapgit_definitions=>ty_items_ts
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS find_changed_files_all
      IMPORTING
        io_online        TYPE REF TO zcl_abapgit_repo_online
        it_branches      TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
      EXPORTING
        et_main_expanded TYPE zif_abapgit_git_definitions=>ty_expanded_tt
      CHANGING
        ct_features      TYPE zif_abapgit_gui_page_flow=>ty_features
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS try_matching_transports
      IMPORTING
        ii_repo          TYPE REF TO zif_abapgit_repo
        it_main_expanded TYPE zif_abapgit_git_definitions=>ty_expanded_tt
      CHANGING
        ct_features      TYPE zif_abapgit_gui_page_flow=>ty_features
        ct_transports    TYPE ty_transports_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS add_objects_and_files_from_tr
      IMPORTING
        iv_trkorr        TYPE trkorr
        ii_repo          TYPE REF TO zif_abapgit_repo
        it_transports    TYPE ty_transports_tt
        it_main_expanded TYPE zif_abapgit_git_definitions=>ty_expanded_tt
      CHANGING
        cs_feature       TYPE zif_abapgit_gui_page_flow=>ty_feature
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS find_up_to_date
      IMPORTING
        iv_url      TYPE string
        it_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
      CHANGING
        ct_features TYPE zif_abapgit_gui_page_flow=>ty_features
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS find_prs
      IMPORTING
        iv_url      TYPE string
      CHANGING
        ct_features TYPE zif_abapgit_gui_page_flow=>ty_features
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS add_local_status
      IMPORTING
        io_online   TYPE REF TO zcl_abapgit_repo_online
      CHANGING
        ct_features TYPE zif_abapgit_gui_page_flow=>ty_features
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS find_open_transports
      RETURNING
        VALUE(rt_transports) TYPE ty_transports_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS find_changed_files
      IMPORTING
        it_expanded1    TYPE zif_abapgit_git_definitions=>ty_expanded_tt
        it_expanded2    TYPE zif_abapgit_git_definitions=>ty_expanded_tt
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_gui_page_flow=>ty_path_name_tt.
ENDCLASS.

CLASS lcl_helper IMPLEMENTATION.

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
        <ls_branch>-pr-title = |{ ls_pull-title } #{ ls_pull-number }|.
        <ls_branch>-pr-url = ls_pull-html_url.
        <ls_branch>-pr-draft = ls_pull-draft.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD build_repo_data.
    rs_data-name = io_online->get_name( ).
    rs_data-key = io_online->get_key( ).
    rs_data-package = io_online->get_package( ).
  ENDMETHOD.

  METHOD get_information.

    DATA lt_branches   TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.
    DATA ls_branch     LIKE LINE OF lt_branches.
    DATA ls_result     LIKE LINE OF rt_features.
    DATA lt_favorites  TYPE zif_abapgit_repo_srv=>ty_repo_list.
    DATA li_favorite   LIKE LINE OF lt_favorites.
    DATA lo_online     TYPE REF TO zcl_abapgit_repo_online.
    DATA lt_features   LIKE rt_features.
    DATA lt_transports TYPE ty_transports_tt.
    DATA lt_main_expanded TYPE zif_abapgit_git_definitions=>ty_expanded_tt.

    FIELD-SYMBOLS <ls_feature> LIKE LINE OF lt_features.
    FIELD-SYMBOLS <ls_path_name> LIKE LINE OF <ls_feature>-changed_files.

    lt_transports = find_open_transports( ).

* list branches on favorite + flow enabled + transported repos
    lt_favorites = zcl_abapgit_repo_srv=>get_instance( )->list_favorites( abap_false ).
    LOOP AT lt_favorites INTO li_favorite.
      IF li_favorite->get_local_settings( )-flow = abap_false.
        CONTINUE.
      ELSEIF zcl_abapgit_factory=>get_sap_package( li_favorite->get_package( )
          )->are_changes_recorded_in_tr_req( ) = abap_false.
        CONTINUE.
      ENDIF.

      lo_online ?= li_favorite.

      lt_branches = zcl_abapgit_git_factory=>get_v2_porcelain( )->list_branches(
        iv_url    = lo_online->get_url( )
        iv_prefix = 'refs/heads/' )->get_all( ).

      CLEAR lt_features.
      LOOP AT lt_branches INTO ls_branch WHERE display_name <> c_main.
        ls_result-repo = build_repo_data( lo_online ).
        ls_result-branch-display_name = ls_branch-display_name.
        ls_result-branch-sha1 = ls_branch-sha1.
        INSERT ls_result INTO TABLE lt_features.
      ENDLOOP.

      find_changed_files_all(
        EXPORTING
          io_online        = lo_online
          it_branches      = lt_branches
        IMPORTING
          et_main_expanded = lt_main_expanded
        CHANGING
          ct_features      = lt_features ).

      try_matching_transports(
        EXPORTING
          ii_repo          = li_favorite
          it_main_expanded = lt_main_expanded
        CHANGING
          ct_transports    = lt_transports
          ct_features      = lt_features ).

      find_up_to_date(
        EXPORTING
          iv_url      = lo_online->get_url( )
          it_branches = lt_branches
        CHANGING
          ct_features = lt_features ).

      find_prs(
        EXPORTING
          iv_url      = lo_online->get_url( )
        CHANGING
          ct_features = lt_features ).

      add_local_status(
        EXPORTING
          io_online   = lo_online
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

      INSERT LINES OF lt_features INTO TABLE rt_features.
    ENDLOOP.

    SORT rt_features BY full_match transport-trkorr DESCENDING.

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
              ii_repo          = ii_repo
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
          ii_repo          = ii_repo
          it_main_expanded = it_main_expanded
          it_transports    = ct_transports
        CHANGING
          cs_feature       = ls_result ).

      INSERT ls_result INTO TABLE ct_features.
    ENDLOOP.

  ENDMETHOD.

  METHOD add_objects_and_files_from_tr.

    DATA ls_changed      LIKE LINE OF cs_feature-changed_objects.
    DATA lo_filter       TYPE REF TO lcl_filter.
    DATA lt_filter       TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lt_local        TYPE zif_abapgit_definitions=>ty_files_item_tt.
    DATA ls_changed_file LIKE LINE OF cs_feature-changed_files.

    FIELD-SYMBOLS <ls_transport> LIKE LINE OF it_transports.
    FIELD-SYMBOLS <ls_local>     LIKE LINE OF lt_local.
    FIELD-SYMBOLS <ls_filter>    LIKE LINE OF lt_filter.
    FIELD-SYMBOLS <ls_main_expanded> LIKE LINE OF it_main_expanded.


    LOOP AT it_transports ASSIGNING <ls_transport> WHERE trkorr = iv_trkorr.
      ls_changed-obj_type = <ls_transport>-object.
      ls_changed-obj_name = <ls_transport>-obj_name.
      INSERT ls_changed INTO TABLE cs_feature-changed_objects.

      APPEND INITIAL LINE TO lt_filter ASSIGNING <ls_filter>.
      <ls_filter>-object = <ls_transport>-object.
      <ls_filter>-obj_name = <ls_transport>-obj_name.
    ENDLOOP.

    CREATE OBJECT lo_filter EXPORTING it_filter = lt_filter.
    lt_local = ii_repo->get_files_local_filtered( lo_filter ).
    LOOP AT lt_local ASSIGNING <ls_local> WHERE file-filename <> zif_abapgit_definitions=>c_dot_abapgit.
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

  ENDMETHOD.

  METHOD find_open_transports.

    DATA lt_trkorr   TYPE zif_abapgit_cts_api=>ty_trkorr_tt.
    DATA lv_trkorr   LIKE LINE OF lt_trkorr.
    DATA ls_result   LIKE LINE OF rt_transports.
    DATA lt_objects  TYPE zif_abapgit_cts_api=>ty_transport_obj_tt.
    DATA lv_obj_name TYPE tadir-obj_name.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF lt_objects.


    lt_trkorr = zcl_abapgit_factory=>get_cts_api( )->list_open_requests_by_user( ).

    LOOP AT lt_trkorr INTO lv_trkorr.
      ls_result-trkorr  = lv_trkorr.
      ls_result-title   = zcl_abapgit_factory=>get_cts_api( )->read_description( lv_trkorr ).

      lt_objects = zcl_abapgit_factory=>get_cts_api( )->list_r3tr_by_request( lv_trkorr ).
      LOOP AT lt_objects ASSIGNING <ls_object>.
        ls_result-object = <ls_object>-object.
        ls_result-obj_name = <ls_object>-obj_name.

        lv_obj_name = <ls_object>-obj_name.
        ls_result-devclass = zcl_abapgit_factory=>get_tadir( )->read_single(
          iv_object   = ls_result-object
          iv_obj_name = lv_obj_name )-devclass.
        INSERT ls_result INTO TABLE rt_transports.
      ENDLOOP.

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

    READ TABLE it_branches INTO ls_main WITH KEY display_name = c_main.
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

  METHOD find_changed_files_all.

    DATA ls_branch          LIKE LINE OF it_branches.
    DATA lt_sha1            TYPE zif_abapgit_git_definitions=>ty_sha1_tt.
    DATA lt_objects         TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA lv_starting_folder TYPE string.
    DATA ls_main            LIKE LINE OF it_branches.
    DATA lt_expanded        TYPE zif_abapgit_git_definitions=>ty_expanded_tt.

    FIELD-SYMBOLS <ls_branch> LIKE LINE OF ct_features.


    LOOP AT it_branches INTO ls_branch WHERE is_head = abap_false.
      APPEND ls_branch-sha1 TO lt_sha1.
    ENDLOOP.

    lt_objects = zcl_abapgit_git_factory=>get_v2_porcelain( )->list_no_blobs_multi(
      iv_url  = io_online->get_url( )
      it_sha1 = lt_sha1 ).

    lv_starting_folder = io_online->get_dot_abapgit( )->get_starting_folder( ) && '*'.

    READ TABLE it_branches INTO ls_main WITH KEY display_name = c_main.
    ASSERT sy-subrc = 0.

    et_main_expanded = zcl_abapgit_git_porcelain=>full_tree(
      it_objects = lt_objects
      iv_parent  = ls_main-sha1 ).
    DELETE et_main_expanded WHERE path NP lv_starting_folder.

    LOOP AT ct_features ASSIGNING <ls_branch> WHERE branch-display_name <> c_main.
      lt_expanded = zcl_abapgit_git_porcelain=>full_tree(
        it_objects = lt_objects
        iv_parent  = <ls_branch>-branch-sha1 ).
      DELETE lt_expanded WHERE path NP lv_starting_folder.

      <ls_branch>-changed_files = find_changed_files(
        it_expanded1 = lt_expanded
        it_expanded2 = et_main_expanded ).

      <ls_branch>-changed_objects = map_files_to_objects(
        io_online = io_online
        it_files  = <ls_branch>-changed_files ).
    ENDLOOP.

  ENDMETHOD.

  METHOD add_local_status.

    DATA lt_local  TYPE zif_abapgit_definitions=>ty_files_item_tt.
    DATA lo_filter TYPE REF TO lcl_filter.
    DATA lt_filter TYPE zif_abapgit_definitions=>ty_tadir_tt.

    FIELD-SYMBOLS <ls_branch>       LIKE LINE OF ct_features.
    FIELD-SYMBOLS <ls_local>        LIKE LINE OF lt_local.
    FIELD-SYMBOLS <ls_changed_file> TYPE zif_abapgit_gui_page_flow=>ty_path_name.
    FIELD-SYMBOLS <ls_filter>       LIKE LINE OF lt_filter.
    FIELD-SYMBOLS <ls_object>       LIKE LINE OF <ls_branch>-changed_objects.


    LOOP AT ct_features ASSIGNING <ls_branch>.
      LOOP AT <ls_branch>-changed_objects ASSIGNING <ls_object>.
        APPEND INITIAL LINE TO lt_filter ASSIGNING <ls_filter>.
        <ls_filter>-object = <ls_object>-obj_type.
        <ls_filter>-obj_name = <ls_object>-obj_name.
      ENDLOOP.
    ENDLOOP.
    SORT lt_filter BY object obj_name.
    DELETE ADJACENT DUPLICATES FROM lt_filter COMPARING object obj_name.

    IF lines( lt_filter ) = 0.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_filter EXPORTING it_filter = lt_filter.
    lt_local = io_online->get_files_local_filtered( lo_filter ).

    LOOP AT ct_features ASSIGNING <ls_branch>.
      LOOP AT <ls_branch>-changed_files ASSIGNING <ls_changed_file>.
        READ TABLE lt_local ASSIGNING <ls_local>
          WITH KEY file-filename = <ls_changed_file>-filename
          file-path = <ls_changed_file>-path.
        IF sy-subrc = 0.
          <ls_changed_file>-local_sha1 = <ls_local>-file-sha1.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD map_files_to_objects.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF it_files.

    LOOP AT it_files ASSIGNING <ls_file>.
      zcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = <ls_file>-filename
          iv_path     = <ls_file>-path
          iv_devclass = io_online->get_package( )
          io_dot      = io_online->get_dot_abapgit( )
        IMPORTING
          es_item     = ls_item ).
      INSERT ls_item INTO TABLE rt_changed_objects.
    ENDLOOP.

  ENDMETHOD.

  METHOD find_changed_files.
* dont care if its added or removed or changed, just remove identical
* also list identical moved files

    DATA ls_path_name LIKE LINE OF rt_files.

    FIELD-SYMBOLS <ls_expanded1> LIKE LINE OF it_expanded1.
    FIELD-SYMBOLS <ls_expanded2> LIKE LINE OF it_expanded1.

    LOOP AT it_expanded1 ASSIGNING <ls_expanded1>.
      READ TABLE it_expanded2 ASSIGNING <ls_expanded2>
        WITH TABLE KEY path_name COMPONENTS
        path = <ls_expanded1>-path
        name = <ls_expanded1>-name.
      IF sy-subrc = 0 AND <ls_expanded1>-sha1 = <ls_expanded2>-sha1.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING <ls_expanded1> TO ls_path_name.
      ls_path_name-filename = <ls_expanded1>-name.
      ls_path_name-remote_sha1 = <ls_expanded1>-sha1.
      INSERT ls_path_name INTO TABLE rt_files.
    ENDLOOP.

    LOOP AT it_expanded2 ASSIGNING <ls_expanded2>.
      READ TABLE it_expanded1 ASSIGNING <ls_expanded1>
        WITH TABLE KEY path_name COMPONENTS
        path = <ls_expanded2>-path
        name = <ls_expanded2>-name.
      IF sy-subrc = 0 AND <ls_expanded1>-sha1 = <ls_expanded2>-sha1.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING <ls_expanded2> TO ls_path_name.
      ls_path_name-filename = <ls_expanded2>-name.
      ls_path_name-remote_sha1 = <ls_expanded2>-sha1.
      INSERT ls_path_name INTO TABLE rt_files.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
