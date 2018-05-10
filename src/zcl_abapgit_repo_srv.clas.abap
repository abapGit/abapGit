CLASS zcl_abapgit_repo_srv DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(rv_srv) TYPE REF TO zcl_abapgit_repo_srv .
    METHODS list
      RETURNING
        VALUE(rt_list) TYPE zif_abapgit_definitions=>ty_repo_ref_tt
      RAISING
        zcx_abapgit_exception .
    METHODS new_online
      IMPORTING
        !iv_url         TYPE string
        !iv_branch_name TYPE string
        !iv_package     TYPE devclass
      RETURNING
        VALUE(ro_repo)  TYPE REF TO zcl_abapgit_repo_online
      RAISING
        zcx_abapgit_exception .
    METHODS new_offline
      IMPORTING
        !iv_url        TYPE string
        !iv_package    TYPE devclass
      RETURNING
        VALUE(ro_repo) TYPE REF TO zcl_abapgit_repo_offline
      RAISING
        zcx_abapgit_exception .
    METHODS delete
      IMPORTING
        !io_repo TYPE REF TO zcl_abapgit_repo
      RAISING
        zcx_abapgit_exception .
    METHODS get
      IMPORTING
        !iv_key        TYPE zif_abapgit_persistence=>ty_value
      RETURNING
        VALUE(ro_repo) TYPE REF TO zcl_abapgit_repo
      RAISING
        zcx_abapgit_exception .
    METHODS is_repo_installed
      IMPORTING
        !iv_url             TYPE string
        !iv_target_package  TYPE devclass OPTIONAL
      RETURNING
        VALUE(rv_installed) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS switch_repo_type
      IMPORTING
        !iv_key     TYPE zif_abapgit_persistence=>ty_value
        !iv_offline TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS validate_package
      IMPORTING
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
    METHODS purge
      IMPORTING
        !io_repo TYPE REF TO zcl_abapgit_repo
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.

    CLASS-DATA go_ref TYPE REF TO zcl_abapgit_repo_srv .
    DATA mv_init TYPE abap_bool VALUE abap_false ##NO_TEXT.
    DATA mo_persistence TYPE REF TO zcl_abapgit_persistence_repo .
    DATA mt_list TYPE zif_abapgit_definitions=>ty_repo_ref_tt .

    METHODS refresh
      RAISING
        zcx_abapgit_exception .
    METHODS constructor .
    METHODS is_sap_object_allowed
      RETURNING
        VALUE(rv_allowed) TYPE abap_bool .
    METHODS add
      IMPORTING
        !io_repo TYPE REF TO zcl_abapgit_repo
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

    APPEND io_repo TO mt_list.

  ENDMETHOD.                    "add


  METHOD constructor.
    CREATE OBJECT mo_persistence.
  ENDMETHOD.                    "class_constructor


  METHOD delete.

    io_repo->delete( ).

    DELETE TABLE mt_list FROM io_repo.
    ASSERT sy-subrc = 0.

  ENDMETHOD.                    "delete


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

    zcx_abapgit_exception=>raise( 'repo not found, get' ).

  ENDMETHOD.                    "get


  METHOD get_instance.
    IF go_ref IS INITIAL.
      CREATE OBJECT go_ref.
    ENDIF.
    rv_srv = go_ref.
  ENDMETHOD.


  METHOD is_repo_installed.

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

  ENDMETHOD. "is_repo_installed


  METHOD is_sap_object_allowed.

    rv_allowed = cl_enh_badi_def_utility=>is_sap_system( ).
    IF rv_allowed = abap_true.
      RETURN.
    ENDIF.

    rv_allowed = zcl_abapgit_exit=>get_instance( )->allow_sap_objects( ).

  ENDMETHOD.


  METHOD list.

    IF mv_init = abap_false.
      refresh( ).
    ENDIF.

    rt_list = mt_list.

  ENDMETHOD.                    "list


  METHOD new_offline.

    DATA: ls_repo TYPE zif_abapgit_persistence=>ty_repo,
          lv_key  TYPE zif_abapgit_persistence=>ty_repo-key.


    validate_package( iv_package ).

    lv_key = mo_persistence->add(
      iv_url         = iv_url
      iv_branch_name = ''
      iv_package     = iv_package
      iv_offline     = abap_true
      is_dot_abapgit = zcl_abapgit_dot_abapgit=>build_default( )->get_data( ) ).

    TRY.
        ls_repo = mo_persistence->read( lv_key ).
      CATCH zcx_abapgit_not_found.
        zcx_abapgit_exception=>raise( 'new_offline not found' ).
    ENDTRY.

    CREATE OBJECT ro_repo
      EXPORTING
        is_data = ls_repo.

    add( ro_repo ).

  ENDMETHOD.                    "new_offline


  METHOD new_online.

    DATA: ls_repo TYPE zif_abapgit_persistence=>ty_repo,
          lv_key  TYPE zif_abapgit_persistence=>ty_repo-key.


    ASSERT NOT iv_url IS INITIAL
      AND NOT iv_branch_name IS INITIAL
      AND NOT iv_package IS INITIAL.

    validate_package( iv_package ).
    zcl_abapgit_url=>validate( |{ iv_url }| ).

    lv_key = mo_persistence->add(
      iv_url         = iv_url
      iv_branch_name = iv_branch_name
      iv_package     = iv_package
      iv_offline     = abap_false
      is_dot_abapgit = zcl_abapgit_dot_abapgit=>build_default( )->get_data( ) ).
    TRY.
        ls_repo = mo_persistence->read( lv_key ).
      CATCH zcx_abapgit_not_found.
        zcx_abapgit_exception=>raise( 'new_online not found' ).
    ENDTRY.

    CREATE OBJECT ro_repo
      EXPORTING
        is_data = ls_repo.

    add( ro_repo ).

    ro_repo->initialize( ).
    ro_repo->find_remote_dot_abapgit( ).

  ENDMETHOD.                    "new_online


  METHOD purge.

    DATA: lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.


    IF io_repo->get_local_settings( )-write_protected = abap_true.
      zcx_abapgit_exception=>raise( 'Cannot purge. Local code is write-protected by repo config' ).
    ELSEIF zcl_abapgit_auth=>is_allowed( zif_abapgit_auth=>gc_authorization-uninstall ) = abap_false.
      zcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    lt_tadir = zcl_abapgit_tadir=>read( io_repo->get_package( ) ).

    zcl_abapgit_objects=>delete( lt_tadir ).

    delete( io_repo ).

  ENDMETHOD.


  METHOD refresh.

    DATA: lt_list    TYPE zif_abapgit_persistence=>tt_repo,
          lo_online  TYPE REF TO zcl_abapgit_repo_online,
          lo_offline TYPE REF TO zcl_abapgit_repo_offline.

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


  METHOD switch_repo_type.

    DATA lo_repo TYPE REF TO zcl_abapgit_repo.

    FIELD-SYMBOLS <lo_repo> LIKE LINE OF mt_list.

    lo_repo = get( iv_key ).
    READ TABLE mt_list ASSIGNING <lo_repo> FROM lo_repo.
    ASSERT sy-subrc IS INITIAL.
    ASSERT iv_offline <> lo_repo->ms_data-offline.

    IF iv_offline = abap_true. " On-line -> OFFline
      lo_repo->set(
        iv_url         = zcl_abapgit_url=>name( lo_repo->ms_data-url )
        iv_branch_name = ''
        iv_sha1        = ''
        iv_head_branch = ''
        iv_offline     = abap_true ).
      CREATE OBJECT <lo_repo> TYPE zcl_abapgit_repo_offline
        EXPORTING
          is_data = lo_repo->ms_data.
    ELSE. " OFFline -> On-line
      lo_repo->set( iv_offline = abap_false ).
      CREATE OBJECT <lo_repo> TYPE zcl_abapgit_repo_online
        EXPORTING
          is_data = lo_repo->ms_data.
    ENDIF.

  ENDMETHOD.  "switch_repo_type


  METHOD validate_package.

    DATA: lv_as4user TYPE tdevc-as4user,
          lt_repos   TYPE zif_abapgit_persistence=>tt_repo.


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
      zcx_abapgit_exception=>raise( |Package { iv_package } not allowed| ).
    ENDIF.

    " make sure its not already in use for a different repository
    lt_repos = mo_persistence->list( ).
    READ TABLE lt_repos WITH KEY package = iv_package TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise( |Package { iv_package } already in use| ).
    ENDIF.

  ENDMETHOD.                    "validate_package
ENDCLASS.
