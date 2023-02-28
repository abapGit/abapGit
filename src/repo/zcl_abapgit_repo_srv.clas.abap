CLASS zcl_abapgit_repo_srv DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_repo_srv .
    INTERFACES zif_abapgit_repo_listener .

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ri_srv) TYPE REF TO zif_abapgit_repo_srv .
    CLASS-METHODS inject_instance
      IMPORTING
        ii_srv TYPE REF TO zif_abapgit_repo_srv.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gi_ref TYPE REF TO zif_abapgit_repo_srv .
    DATA mv_init TYPE abap_bool.
    DATA mv_only_favorites TYPE abap_bool.
    DATA mt_list TYPE zif_abapgit_repo_srv=>ty_repo_list .

    METHODS determine_branch_name
      IMPORTING
        !iv_name       TYPE string
        !iv_url        TYPE string
      RETURNING
        VALUE(rv_name) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS refresh_all
      RAISING
        zcx_abapgit_exception .
    METHODS refresh_favorites
      RAISING
        zcx_abapgit_exception .
    METHODS instantiate_and_add
      IMPORTING
        !is_repo_meta  TYPE zif_abapgit_persistence=>ty_repo
      RETURNING
        VALUE(ri_repo) TYPE REF TO zif_abapgit_repo
      RAISING
        zcx_abapgit_exception .
    METHODS add
      IMPORTING
        !ii_repo TYPE REF TO zif_abapgit_repo
      RAISING
        zcx_abapgit_exception .
    METHODS reinstantiate_repo
      IMPORTING
        !iv_key  TYPE zif_abapgit_persistence=>ty_repo-key
        !is_meta TYPE zif_abapgit_persistence=>ty_repo_xml
      RAISING
        zcx_abapgit_exception .
    METHODS validate_sub_super_packages
      IMPORTING
        !iv_package    TYPE devclass
        !it_repos      TYPE zif_abapgit_persistence=>ty_repos
        !iv_ign_subpkg TYPE abap_bool DEFAULT abap_false
      EXPORTING
        VALUE(ei_repo) TYPE REF TO zif_abapgit_repo
        !ev_reason     TYPE string
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_repo_srv IMPLEMENTATION.


  METHOD add.

    DATA li_repo LIKE LINE OF mt_list.
    DATA lo_repo TYPE REF TO zcl_abapgit_repo.

    LOOP AT mt_list INTO li_repo.
      IF li_repo->ms_data-key = ii_repo->ms_data-key.
        IF li_repo = ii_repo.
          RETURN.
        ENDIF.
        zcx_abapgit_exception=>raise( 'identical keys' ).
      ENDIF.
    ENDLOOP.

    lo_repo ?= ii_repo. " TODO, refactor later
    lo_repo->bind_listener( me ).
    APPEND ii_repo TO mt_list.

  ENDMETHOD.


  METHOD determine_branch_name.

    DATA lo_branch_list TYPE REF TO zcl_abapgit_git_branch_list.

    rv_name = iv_name.
    IF rv_name IS INITIAL.
      ASSERT NOT iv_url IS INITIAL.
      lo_branch_list = zcl_abapgit_git_transport=>branches( iv_url ).
      rv_name = lo_branch_list->get_head_symref( ).
    ELSEIF -1 = find(
        val = rv_name
        sub = zif_abapgit_definitions=>c_git_branch-heads_prefix ).
      " Assume short branch name was received
      rv_name = zif_abapgit_definitions=>c_git_branch-heads_prefix && rv_name.
    ENDIF.

  ENDMETHOD.


  METHOD get_instance.
    IF gi_ref IS INITIAL.
      CREATE OBJECT gi_ref TYPE zcl_abapgit_repo_srv.
    ENDIF.
    ri_srv = gi_ref.
  ENDMETHOD.


  METHOD inject_instance.
    gi_ref = ii_srv.
  ENDMETHOD.


  METHOD instantiate_and_add.

    IF is_repo_meta-offline = abap_false.
      CREATE OBJECT ri_repo TYPE zcl_abapgit_repo_online
        EXPORTING
          is_data = is_repo_meta.
    ELSE.
      CREATE OBJECT ri_repo TYPE zcl_abapgit_repo_offline
        EXPORTING
          is_data = is_repo_meta.
    ENDIF.
    add( ri_repo ).

  ENDMETHOD.


  METHOD refresh_all.

    DATA: lt_list TYPE zif_abapgit_persistence=>ty_repos.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.

    CLEAR mt_list.

    lt_list = zcl_abapgit_persist_factory=>get_repo( )->list( ).
    LOOP AT lt_list ASSIGNING <ls_list>.
      instantiate_and_add( <ls_list> ).
    ENDLOOP.

    mv_init = abap_true.
    mv_only_favorites = abap_false.

  ENDMETHOD.


  METHOD refresh_favorites.

    DATA: lt_list           TYPE zif_abapgit_persistence=>ty_repos,
          lt_user_favorites TYPE zif_abapgit_persist_user=>ty_favorites.

    DATA li_repo TYPE REF TO zif_abapgit_repo.
    DATA lv_repo_index TYPE i.
    DATA lo_repo_db TYPE REF TO zif_abapgit_persist_repo.

    FIELD-SYMBOLS: <ls_repo_record> LIKE LINE OF lt_list.

    lo_repo_db        = zcl_abapgit_persist_factory=>get_repo( ).
    lt_user_favorites = zcl_abapgit_persistence_user=>get_instance( )->get_favorites( ).
    lt_list           = lo_repo_db->list_by_keys( lt_user_favorites ).

    SORT lt_list BY package.

    LOOP AT mt_list INTO li_repo.
      lv_repo_index = sy-tabix.

      READ TABLE lt_list TRANSPORTING NO FIELDS WITH KEY package = li_repo->get_package( ).
      IF sy-subrc = 0.
        DELETE lt_list INDEX sy-tabix.
        CONTINUE. " Leave the repo be
      ELSEIF lo_repo_db->exists( li_repo->get_key( ) ) = abap_false.
        " Not a fav, and also does not exist, probably uninstalled
        DELETE mt_list INDEX lv_repo_index.
      ENDIF.

    ENDLOOP.

    " Create remaining (new) favs
    LOOP AT lt_list ASSIGNING <ls_repo_record>.
      instantiate_and_add( <ls_repo_record> ).
    ENDLOOP.

    mv_init = abap_true.
    mv_only_favorites = abap_true.

  ENDMETHOD.


  METHOD reinstantiate_repo.

    DATA li_repo      TYPE REF TO zif_abapgit_repo.
    DATA ls_full_meta TYPE zif_abapgit_persistence=>ty_repo.

    li_repo = zif_abapgit_repo_srv~get( iv_key ).
    DELETE TABLE mt_list FROM li_repo.
    ASSERT sy-subrc IS INITIAL.

    MOVE-CORRESPONDING is_meta TO ls_full_meta.
    ls_full_meta-key = iv_key.

    instantiate_and_add( ls_full_meta ).

  ENDMETHOD.


  METHOD validate_sub_super_packages.

    DATA:
      ls_repo     LIKE LINE OF it_repos,
      li_package  TYPE REF TO zif_abapgit_sap_package,
      lt_packages TYPE zif_abapgit_sap_package=>ty_devclass_tt,
      li_repo     TYPE REF TO zif_abapgit_repo.

    LOOP AT it_repos INTO ls_repo.
      li_repo = zif_abapgit_repo_srv~get( ls_repo-key ).

      li_package = zcl_abapgit_factory=>get_sap_package( ls_repo-package ).
      IF li_package->exists( ) = abap_false.
        " Skip dangling repository
        CONTINUE.
      ENDIF.

      CLEAR lt_packages.
      IF li_repo->get_local_settings( )-ignore_subpackages = abap_false.
        APPEND LINES OF li_package->list_subpackages( ) TO lt_packages.
        READ TABLE lt_packages TRANSPORTING NO FIELDS
          WITH KEY table_line = iv_package.
        IF sy-subrc = 0.
          ei_repo = li_repo.
          ev_reason = |Repository { li_repo->get_name( ) } already contains { iv_package } |.
          RETURN.
        ENDIF.
      ENDIF.

      IF iv_ign_subpkg = abap_false.
        APPEND LINES OF li_package->list_superpackages( ) TO lt_packages.
        READ TABLE lt_packages TRANSPORTING NO FIELDS
          WITH KEY table_line = iv_package.
        IF sy-subrc = 0.
          ei_repo = li_repo.
          ev_reason = |Repository { li_repo->get_name( ) } already contains subpackage of { iv_package } |.
          RETURN.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_repo_listener~on_meta_change.

    DATA li_persistence TYPE REF TO zif_abapgit_persist_repo.

    li_persistence = zcl_abapgit_persist_factory=>get_repo( ).
    li_persistence->update_metadata(
      iv_key         = iv_key
      is_meta        = is_meta
      is_change_mask = is_change_mask ).


    " Recreate repo instance if type changed
    " Instances in mt_list are of *_online and *_offline type
    " If type is changed object should be recreated from the proper class
    " TODO refactor, e.g. unify repo logic in one class
    IF is_change_mask-offline = abap_true.
      reinstantiate_repo(
        iv_key  = iv_key
        is_meta = is_meta ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~delete.

    zcl_abapgit_persist_factory=>get_repo( )->delete( ii_repo->get_key( ) ).
    zcl_abapgit_persist_factory=>get_repo_cs( )->delete( ii_repo->get_key( ) ).

    " If favorite, remove it
    IF zcl_abapgit_persistence_user=>get_instance( )->is_favorite_repo( ii_repo->get_key( ) ) = abap_true.
      zcl_abapgit_persistence_user=>get_instance( )->toggle_favorite( ii_repo->get_key( ) ).
    ENDIF.

    DELETE TABLE mt_list FROM ii_repo.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~get.

    FIELD-SYMBOLS: <li_repo> LIKE LINE OF mt_list.

    IF mv_init = abap_false.
      refresh_all( ).
    ENDIF.

    DO 2 TIMES.
      " Repo might have been created in another session. Try again after refresh
      IF sy-index = 2.
        refresh_all( ).
      ENDIF.
      LOOP AT mt_list ASSIGNING <li_repo>.
        IF <li_repo>->ms_data-key = iv_key.
          ri_repo = <li_repo>.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDDO.

    zcx_abapgit_exception=>raise( |Repository not found in database. Key: REPO, { iv_key }| ).

  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~get_label_list.

    DATA:
      lt_repo           TYPE zif_abapgit_repo_srv=>ty_repo_list,
      ls_local_settings TYPE zif_abapgit_persistence=>ty_repo-local_settings,
      lt_labels         TYPE string_table,
      ls_label          LIKE LINE OF rt_labels.

    FIELD-SYMBOLS:
      <ls_repo>  TYPE REF TO zif_abapgit_repo,
      <lv_label> TYPE LINE OF string_table.

    lt_repo = zif_abapgit_repo_srv~list( ).

    LOOP AT lt_repo ASSIGNING <ls_repo>.

      ls_local_settings = <ls_repo>->get_local_settings( ).
      lt_labels = zcl_abapgit_repo_labels=>split( ls_local_settings-labels ).

      LOOP AT lt_labels ASSIGNING <lv_label>.
        ls_label-label = <lv_label>.
        INSERT ls_label INTO TABLE rt_labels.
      ENDLOOP.

    ENDLOOP.

    SORT rt_labels.
    DELETE ADJACENT DUPLICATES FROM rt_labels.

  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~get_repo_from_package.

    DATA:
      lt_repos TYPE zif_abapgit_persistence=>ty_repos,
      lv_name  TYPE zif_abapgit_persistence=>ty_local_settings-display_name,
      lv_owner TYPE zif_abapgit_persistence=>ty_local_settings-display_name.

    FIELD-SYMBOLS:
      <ls_repo> LIKE LINE OF lt_repos.

    " check if package is already in use for a different repository
    lt_repos = zcl_abapgit_persist_factory=>get_repo( )->list( ).
    READ TABLE lt_repos WITH KEY package = iv_package ASSIGNING <ls_repo>.
    IF sy-subrc = 0.
      ei_repo = get_instance( )->get( <ls_repo>-key ).
      lv_name = ei_repo->get_name( ).
      lv_owner = <ls_repo>-created_by.
      ev_reason = |Package { iv_package } already versioned as { lv_name } by { lv_owner }|.
    ELSE.
      " check if package is include as sub-package in a different repo
      validate_sub_super_packages(
        EXPORTING
          iv_package    = iv_package
          it_repos      = lt_repos
          iv_ign_subpkg = iv_ign_subpkg
        IMPORTING
          ei_repo       = ei_repo
          ev_reason     = ev_reason ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~get_repo_from_url.

    DATA:
      lt_repos                TYPE zif_abapgit_persistence=>ty_repos,
      lv_current_repo_address TYPE string,
      lv_check_repo_address   TYPE string,
      lv_repo_path            TYPE string,
      lv_name                 TYPE zif_abapgit_persistence=>ty_local_settings-display_name,
      lv_owner                TYPE zif_abapgit_persistence=>ty_local_settings-display_name.

    FIELD-SYMBOLS:
      <ls_repo> LIKE LINE OF lt_repos.

    CLEAR:
      ei_repo, ev_reason.

    lv_current_repo_address = zcl_abapgit_url=>url_address( iv_url ).

    " check if url is already in use for a different package
    lt_repos = zcl_abapgit_persist_factory=>get_repo( )->list( ).
    LOOP AT lt_repos ASSIGNING <ls_repo> WHERE offline = abap_false.

      lv_check_repo_address = zcl_abapgit_url=>url_address( <ls_repo>-url ).

      IF lv_current_repo_address = lv_check_repo_address.
        ei_repo      = get_instance( )->get( <ls_repo>-key ).
        lv_repo_path = zcl_abapgit_url=>path_name( iv_url ).
        lv_name      = ei_repo->get_name( ).
        lv_owner     = <ls_repo>-created_by.
        ev_reason    = |Repository { lv_repo_path } already versioned as { lv_name } by { lv_owner }|.
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~init.
    CLEAR mv_init.
  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~is_repo_installed.

    DATA: lt_repo        TYPE zif_abapgit_repo_srv=>ty_repo_list,
          li_repo        TYPE REF TO zif_abapgit_repo,
          lv_url         TYPE string,
          lv_package     TYPE devclass,
          lo_repo_online TYPE REF TO zcl_abapgit_repo_online,
          lv_err         TYPE string.

    lt_repo = zif_abapgit_repo_srv~list( ).

    LOOP AT lt_repo INTO li_repo.
      CHECK li_repo->is_offline( ) = abap_false.
      lo_repo_online ?= li_repo.

      lv_url     = lo_repo_online->get_url( ).
      lv_package = lo_repo_online->get_package( ).
      CHECK to_upper( lv_url ) = to_upper( iv_url ).

      " Validate bindings
      "TODO refactor: move this message out of this method
      IF iv_target_package IS NOT INITIAL AND iv_target_package <> lv_package.
        lv_err = |Installation to package { lv_package } detected. |
              && |Cancelling installation|.
        zcx_abapgit_exception=>raise( lv_err ).
      ENDIF.

      rv_installed = abap_true.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~list.

    IF mv_init = abap_false OR mv_only_favorites = abap_true.
      refresh_all( ).
    ENDIF.

    rt_list = mt_list.

  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~list_favorites.

    DATA lt_user_favorites TYPE zif_abapgit_persist_user=>ty_favorites.
    DATA li_repo TYPE REF TO zif_abapgit_repo.

    lt_user_favorites = zcl_abapgit_persistence_user=>get_instance( )->get_favorites( ).
    SORT lt_user_favorites BY table_line.

    IF mv_init = abap_false OR mv_only_favorites = abap_false.
      refresh_favorites( ).
    ENDIF.

    LOOP AT mt_list INTO li_repo.
      READ TABLE lt_user_favorites
        TRANSPORTING NO FIELDS
        WITH KEY table_line = li_repo->get_key( ).
      IF sy-subrc = 0.
        APPEND li_repo TO rt_list.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~new_offline.

    DATA: ls_repo        TYPE zif_abapgit_persistence=>ty_repo,
          lv_key         TYPE zif_abapgit_persistence=>ty_repo-key,
          lo_repo        TYPE REF TO zcl_abapgit_repo_offline,
          lo_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit.


    IF zcl_abapgit_auth=>is_allowed( zif_abapgit_auth=>c_authorization-create_repo ) = abap_false.
      zcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    zif_abapgit_repo_srv~validate_package(
      iv_package    = iv_package
      iv_ign_subpkg = iv_ign_subpkg ).

    IF iv_url IS INITIAL.
      zcx_abapgit_exception=>raise( 'Missing display name for repo' ).
    ENDIF.

    lo_dot_abapgit = zcl_abapgit_dot_abapgit=>build_default( ).
    lo_dot_abapgit->set_folder_logic( iv_folder_logic ).

    lv_key = zcl_abapgit_persist_factory=>get_repo( )->add(
      iv_url          = iv_url
      iv_branch_name  = ''
      iv_package      = iv_package
      iv_offline      = abap_true
      is_dot_abapgit  = lo_dot_abapgit->get_data( ) ).

    TRY.
        ls_repo = zcl_abapgit_persist_factory=>get_repo( )->read( lv_key ).
      CATCH zcx_abapgit_not_found.
        zcx_abapgit_exception=>raise( 'new_offline not found' ).
    ENDTRY.

    lo_repo ?= instantiate_and_add( ls_repo ).

    IF ls_repo-local_settings-ignore_subpackages <> iv_ign_subpkg.
      ls_repo-local_settings-ignore_subpackages = iv_ign_subpkg.
    ENDIF.
    ls_repo-local_settings-main_language_only = iv_main_lang_only.
    ls_repo-local_settings-labels = iv_labels.

    lo_repo->set_local_settings( ls_repo-local_settings ).
    lo_repo->check_and_create_package( iv_package ).

    ri_repo = lo_repo.

  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~new_online.

    DATA: ls_repo        TYPE zif_abapgit_persistence=>ty_repo,
          lo_repo        TYPE REF TO zcl_abapgit_repo_online,
          lv_branch_name LIKE iv_branch_name,
          lv_key         TYPE zif_abapgit_persistence=>ty_repo-key,
          ls_dot_abapgit TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit,
          lv_url         TYPE string.


    ASSERT NOT iv_url IS INITIAL
      AND NOT iv_package IS INITIAL.

    lv_url = condense( iv_url ).

    IF zcl_abapgit_auth=>is_allowed( zif_abapgit_auth=>c_authorization-create_repo ) = abap_false.
      zcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    zif_abapgit_repo_srv~validate_package(
      iv_package    = iv_package
      iv_ign_subpkg = iv_ign_subpkg ).

    zif_abapgit_repo_srv~validate_url( lv_url ).

    lv_branch_name = determine_branch_name(
      iv_name = iv_branch_name
      iv_url  = lv_url ).

    ls_dot_abapgit = zcl_abapgit_dot_abapgit=>build_default( )->get_data( ).
    ls_dot_abapgit-folder_logic = iv_folder_logic.

    lv_key = zcl_abapgit_persist_factory=>get_repo( )->add(
      iv_url          = lv_url
      iv_branch_name  = lv_branch_name " local !
      iv_display_name = iv_display_name
      iv_package      = iv_package
      iv_offline      = abap_false
      is_dot_abapgit  = ls_dot_abapgit ).

    TRY.
        ls_repo = zcl_abapgit_persist_factory=>get_repo( )->read( lv_key ).
      CATCH zcx_abapgit_not_found.
        zcx_abapgit_exception=>raise( 'new_online not found' ).
    ENDTRY.

    lo_repo ?= instantiate_and_add( ls_repo ).

    IF ls_repo-local_settings-ignore_subpackages <> iv_ign_subpkg.
      ls_repo-local_settings-ignore_subpackages = iv_ign_subpkg.
    ENDIF.
    ls_repo-local_settings-main_language_only = iv_main_lang_only.
    ls_repo-local_settings-labels = iv_labels.

    lo_repo->set_local_settings( ls_repo-local_settings ).
    lo_repo->refresh( ).
    lo_repo->find_remote_dot_abapgit( ).
    lo_repo->check_and_create_package( iv_package ).

    ri_repo = lo_repo.

  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~purge.

* uninstalls all objects, no UI or popups in this class

* todo, this should be a method on the repo instead?

    DATA: lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA: lx_error TYPE REF TO zcx_abapgit_exception.
    DATA lo_repo TYPE REF TO zcl_abapgit_repo.

    lo_repo ?= ii_repo. " TODO, remove later
    ri_log = lo_repo->create_new_log( 'Uninstall Log' ).

    IF ii_repo->get_local_settings( )-write_protected = abap_true.
      zcx_abapgit_exception=>raise( 'Cannot purge. Local code is write-protected by repo config' ).
    ELSEIF zcl_abapgit_auth=>is_allowed( zif_abapgit_auth=>c_authorization-uninstall ) = abap_false.
      zcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    lt_tadir = zcl_abapgit_factory=>get_tadir( )->read( ii_repo->get_package( ) ).

    TRY.
        zcl_abapgit_objects=>delete( it_tadir  = lt_tadir
                                     is_checks = is_checks
                                     ii_log    = ri_log ).
      CATCH zcx_abapgit_exception INTO lx_error.
        " If uninstall fails, repo needs a refresh to show which objects where deleted and which not
        ii_repo->refresh( iv_drop_log = abap_false ).
        RAISE EXCEPTION lx_error.
    ENDTRY.

    zif_abapgit_repo_srv~delete( ii_repo ).

  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~validate_package.

    DATA: lv_as4user TYPE tdevc-as4user,
          li_repo    TYPE REF TO zif_abapgit_repo,
          lv_reason  TYPE string.

    zcl_abapgit_factory=>get_sap_package( iv_package )->validate_name( ).

    " Check if package owned by SAP is allowed (new packages are ok, since they are created automatically)
    lv_as4user = zcl_abapgit_factory=>get_sap_package( iv_package )->read_responsible( ).

    IF sy-subrc = 0 AND lv_as4user = 'SAP' AND
      zcl_abapgit_factory=>get_environment( )->is_sap_object_allowed( ) = abap_false.
      zcx_abapgit_exception=>raise( |Package { iv_package } not allowed, responsible user = 'SAP'| ).
    ENDIF.

    " Check if package is already used in another repo
    IF iv_chk_exists = abap_true.
      zif_abapgit_repo_srv~get_repo_from_package(
        EXPORTING
          iv_package    = iv_package
          iv_ign_subpkg = iv_ign_subpkg
        IMPORTING
          ei_repo       = li_repo
          ev_reason     = lv_reason ).

      IF li_repo IS BOUND.
        zcx_abapgit_exception=>raise( lv_reason ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~validate_url.

    DATA:
      li_repo   TYPE REF TO zif_abapgit_repo,
      lv_reason TYPE string.

    zcl_abapgit_url=>validate( iv_url ).

    IF iv_chk_exists = abap_true.
      zif_abapgit_repo_srv~get_repo_from_url(
        EXPORTING
          iv_url    = iv_url
        IMPORTING
          ei_repo   = li_repo
          ev_reason = lv_reason ).
      IF li_repo IS BOUND.
        zcx_abapgit_exception=>raise( lv_reason ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
