CLASS zcl_abapgit_repo_srv DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_repo_srv .
    INTERFACES zif_abapgit_repo_listener .

    ALIASES get_repo_from_package
      FOR zif_abapgit_repo_srv~get_repo_from_package .

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ri_srv) TYPE REF TO zif_abapgit_repo_srv .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES delete
      FOR zif_abapgit_repo_srv~delete .
    ALIASES get
      FOR zif_abapgit_repo_srv~get .
    ALIASES list
      FOR zif_abapgit_repo_srv~list .
    ALIASES validate_package
      FOR zif_abapgit_repo_srv~validate_package .

    CLASS-DATA gi_ref TYPE REF TO zif_abapgit_repo_srv .
    DATA mv_init TYPE abap_bool VALUE abap_false ##NO_TEXT.
    DATA mt_list TYPE zif_abapgit_definitions=>ty_repo_ref_tt .

    METHODS determine_branch_name
      IMPORTING
        !iv_name       TYPE string
        !iv_url        TYPE string
      RETURNING
        VALUE(rv_name) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS refresh
      RAISING
        zcx_abapgit_exception .
    METHODS is_sap_object_allowed
      RETURNING
        VALUE(rv_allowed) TYPE abap_bool .
    METHODS instantiate_and_add
      IMPORTING
        !is_repo_meta  TYPE zif_abapgit_persistence=>ty_repo
      RETURNING
        VALUE(ro_repo) TYPE REF TO zcl_abapgit_repo
      RAISING
        zcx_abapgit_exception .
    METHODS add
      IMPORTING
        !io_repo TYPE REF TO zcl_abapgit_repo
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
        VALUE(eo_repo) TYPE REF TO zcl_abapgit_repo
        !ev_reason     TYPE string
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_REPO_SRV IMPLEMENTATION.


  METHOD add.

    DATA: lo_repo LIKE LINE OF mt_list.


    LOOP AT mt_list INTO lo_repo.
      IF lo_repo->get_key( ) = io_repo->get_key( ).
        IF lo_repo = io_repo.
          RETURN.
        ENDIF.
        zcx_abapgit_exception=>raise( 'identical keys' ).
      ENDIF.
    ENDLOOP.

    io_repo->bind_listener( me ).
    APPEND io_repo TO mt_list.

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


  METHOD instantiate_and_add.

    IF is_repo_meta-offline = abap_false.
      CREATE OBJECT ro_repo TYPE zcl_abapgit_repo_online
        EXPORTING
          is_data = is_repo_meta.
    ELSE.
      CREATE OBJECT ro_repo TYPE zcl_abapgit_repo_offline
        EXPORTING
          is_data = is_repo_meta.
    ENDIF.
    add( ro_repo ).

  ENDMETHOD.


  METHOD is_sap_object_allowed.

    rv_allowed = cl_enh_badi_def_utility=>is_sap_system( ).
    IF rv_allowed = abap_true.
      RETURN.
    ENDIF.

    rv_allowed = zcl_abapgit_exit=>get_instance( )->allow_sap_objects( ).

  ENDMETHOD.


  METHOD refresh.

    DATA: lt_list TYPE zif_abapgit_persistence=>ty_repos.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    CLEAR mt_list.

    lt_list = zcl_abapgit_persist_factory=>get_repo( )->list( ).
    LOOP AT lt_list ASSIGNING <ls_list>.
      instantiate_and_add( <ls_list> ).
    ENDLOOP.

    mv_init = abap_true.

  ENDMETHOD.


  METHOD reinstantiate_repo.

    DATA lo_repo      TYPE REF TO zcl_abapgit_repo.
    DATA ls_full_meta TYPE zif_abapgit_persistence=>ty_repo.

    lo_repo = get( iv_key ).
    DELETE TABLE mt_list FROM lo_repo.
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
      lo_repo     TYPE REF TO zcl_abapgit_repo.

    LOOP AT it_repos INTO ls_repo.
      lo_repo = get( ls_repo-key ).

      li_package = zcl_abapgit_factory=>get_sap_package( ls_repo-package ).
      IF li_package->exists( ) = abap_false.
        " Skip dangling repository
        CONTINUE.
      ENDIF.

      CLEAR lt_packages.
      IF lo_repo->get_local_settings( )-ignore_subpackages = abap_false.
        APPEND LINES OF li_package->list_subpackages( ) TO lt_packages.
        READ TABLE lt_packages TRANSPORTING NO FIELDS
          WITH KEY table_line = iv_package.
        IF sy-subrc = 0.
          eo_repo = lo_repo.
          ev_reason = |Repository { lo_repo->get_name( ) } already contains { iv_package } |.
          RETURN.
        ENDIF.
      ENDIF.

      IF iv_ign_subpkg = abap_false.
        APPEND LINES OF li_package->list_superpackages( ) TO lt_packages.
        READ TABLE lt_packages TRANSPORTING NO FIELDS
          WITH KEY table_line = iv_package.
        IF sy-subrc = 0.
          eo_repo = lo_repo.
          ev_reason = |Repository { lo_repo->get_name( ) } already contains subpackage of { iv_package } |.
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

    zcl_abapgit_persist_factory=>get_repo( )->delete( io_repo->get_key( ) ).

    " If favorite, remove it
    IF zcl_abapgit_persistence_user=>get_instance( )->is_favorite_repo( io_repo->get_key( ) ) = abap_true.
      zcl_abapgit_persistence_user=>get_instance( )->toggle_favorite( io_repo->get_key( ) ).
    ENDIF.

    DELETE TABLE mt_list FROM io_repo.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~get.

    FIELD-SYMBOLS: <lo_list> LIKE LINE OF mt_list.

    IF mv_init = abap_false.
      refresh( ).
    ENDIF.

    DO 2 TIMES.
      " Repo might have been created in another session. Try again after refresh
      IF sy-index = 2.
        refresh( ).
      ENDIF.
      LOOP AT mt_list ASSIGNING <lo_list>.
        IF <lo_list>->get_key( ) = iv_key.
          ro_repo = <lo_list>.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDDO.

    zcx_abapgit_exception=>raise( 'repo not found, get' ).

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
      eo_repo = get_instance( )->get( <ls_repo>-key ).
      lv_name = eo_repo->get_name( ).
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
          eo_repo       = eo_repo
          ev_reason     = ev_reason ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~is_repo_installed.

    DATA: lt_repo        TYPE zif_abapgit_definitions=>ty_repo_ref_tt,
          lo_repo        TYPE REF TO zcl_abapgit_repo,
          lv_url         TYPE string,
          lv_package     TYPE devclass,
          lo_repo_online TYPE REF TO zcl_abapgit_repo_online,
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
        zcx_abapgit_exception=>raise( lv_err ).
      ENDIF.

      rv_installed = abap_true.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~list.

    IF mv_init = abap_false.
      refresh( ).
    ENDIF.

    rt_list = mt_list.

  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~new_offline.

    DATA: ls_repo        TYPE zif_abapgit_persistence=>ty_repo,
          lv_key         TYPE zif_abapgit_persistence=>ty_repo-key,
          lo_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit.


    IF zcl_abapgit_auth=>is_allowed( zif_abapgit_auth=>gc_authorization-create_repo ) = abap_false.
      zcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    validate_package( iv_package ).

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

    ro_repo ?= instantiate_and_add( ls_repo ).

    ls_repo-local_settings-serialize_master_lang_only = iv_master_lang_only.
    ro_repo->set_local_settings( ls_repo-local_settings ).

  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~new_online.

    DATA: ls_repo        TYPE zif_abapgit_persistence=>ty_repo,
          lv_branch_name LIKE iv_branch_name,
          lv_key         TYPE zif_abapgit_persistence=>ty_repo-key,
          ls_dot_abapgit TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit.


    ASSERT NOT iv_url IS INITIAL
      AND NOT iv_package IS INITIAL.

    IF zcl_abapgit_auth=>is_allowed( zif_abapgit_auth=>gc_authorization-create_repo ) = abap_false.
      zcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    validate_package( iv_package    = iv_package
                      iv_ign_subpkg = iv_ign_subpkg ).

    zcl_abapgit_url=>validate( iv_url ).

    lv_branch_name = determine_branch_name(
      iv_name = iv_branch_name
      iv_url  = iv_url ).

    ls_dot_abapgit = zcl_abapgit_dot_abapgit=>build_default( )->get_data( ).
    ls_dot_abapgit-folder_logic = iv_folder_logic.

    lv_key = zcl_abapgit_persist_factory=>get_repo( )->add(
      iv_url          = iv_url
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

    ro_repo ?= instantiate_and_add( ls_repo ).

    IF ls_repo-local_settings-ignore_subpackages <> iv_ign_subpkg.
      ls_repo-local_settings-ignore_subpackages = iv_ign_subpkg.
    ENDIF.
    ls_repo-local_settings-serialize_master_lang_only = iv_master_lang_only.
    ro_repo->set_local_settings( ls_repo-local_settings ).

    ro_repo->refresh( ).
    ro_repo->find_remote_dot_abapgit( ).

  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~purge.

* todo, this should be a method on the repo instead

    DATA: lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.


    IF io_repo->get_local_settings( )-write_protected = abap_true.
      zcx_abapgit_exception=>raise( 'Cannot purge. Local code is write-protected by repo config' ).
    ELSEIF zcl_abapgit_auth=>is_allowed( zif_abapgit_auth=>gc_authorization-uninstall ) = abap_false.
      zcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    lt_tadir = zcl_abapgit_factory=>get_tadir( )->read( io_repo->get_package( ) ).

    zcl_abapgit_objects=>delete( it_tadir  = lt_tadir
                                 is_checks = is_checks ).

    delete( io_repo ).

  ENDMETHOD.


  METHOD zif_abapgit_repo_srv~validate_package.

    DATA: lv_as4user TYPE tdevc-as4user,
          lo_repo    TYPE REF TO zcl_abapgit_repo,
          lv_reason  TYPE string.

    IF iv_package IS INITIAL.
      zcx_abapgit_exception=>raise( 'add, package empty' ).
    ENDIF.

    IF iv_package = '$TMP'.
      zcx_abapgit_exception=>raise( 'not possible to use $TMP, create new (local) package' ).
    ENDIF.

    SELECT SINGLE as4user FROM tdevc
      INTO lv_as4user
      WHERE devclass = iv_package.                      "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Package { iv_package } not found| ).
    ENDIF.

    IF is_sap_object_allowed( ) = abap_false AND lv_as4user = 'SAP'.
      zcx_abapgit_exception=>raise( |Package { iv_package } not allowed, responsible user = 'SAP'| ).
    ENDIF.

    IF iv_chk_exists = abap_true.
      get_repo_from_package(
        EXPORTING
          iv_package = iv_package
          iv_ign_subpkg = iv_ign_subpkg
        IMPORTING
          eo_repo    = lo_repo
          ev_reason  = lv_reason ).

      IF lo_repo IS BOUND.
        zcx_abapgit_exception=>raise( lv_reason ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
