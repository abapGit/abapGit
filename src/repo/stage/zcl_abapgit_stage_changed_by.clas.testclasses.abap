CLASS ltd_memory_settings DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_persist_settings.
ENDCLASS.

CLASS ltd_memory_settings IMPLEMENTATION.
  METHOD zif_abapgit_persist_settings~modify.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_persist_settings~read.
    CREATE OBJECT ro_settings.
  ENDMETHOD.
ENDCLASS.

" Objects locked in transports, and the owners of those transports
CLASS ltd_cts_api DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_cts_api.
    METHODS add_lock
      IMPORTING
        iv_obj_name TYPE csequence
        iv_trkorr   TYPE trkorr
        iv_user     TYPE uname.
    DATA mv_read_user_count TYPE i READ-ONLY.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_owner,
        trkorr TYPE trkorr,
        user   TYPE uname,
      END OF ty_owner.
    DATA mt_locks TYPE zif_abapgit_cts_api=>ty_transport_list.
    DATA mt_owners TYPE SORTED TABLE OF ty_owner WITH UNIQUE KEY trkorr.
ENDCLASS.

CLASS ltd_cts_api IMPLEMENTATION.
  METHOD add_lock.
    DATA ls_lock LIKE LINE OF mt_locks.
    DATA ls_owner LIKE LINE OF mt_owners.

    ls_lock-obj_type = 'CLAS'.
    ls_lock-obj_name = to_upper( iv_obj_name ).
    ls_lock-trkorr   = iv_trkorr.
    INSERT ls_lock INTO TABLE mt_locks.

    ls_owner-trkorr = iv_trkorr.
    ls_owner-user   = iv_user.
    INSERT ls_owner INTO TABLE mt_owners.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~get_transports_for_list.
    DATA ls_lock LIKE LINE OF mt_locks.
    FIELD-SYMBOLS <ls_item> LIKE LINE OF it_items.

    LOOP AT it_items ASSIGNING <ls_item>.
      READ TABLE mt_locks INTO ls_lock WITH TABLE KEY
        obj_type = <ls_item>-obj_type
        obj_name = <ls_item>-obj_name.
      IF sy-subrc = 0.
        INSERT ls_lock INTO TABLE rt_transports.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~read_user.
    DATA ls_owner LIKE LINE OF mt_owners.

    mv_read_user_count = mv_read_user_count + 1.

    READ TABLE mt_owners INTO ls_owner WITH TABLE KEY trkorr = iv_trkorr.
    IF sy-subrc = 0.
      rv_uname = ls_owner-user.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~is_chrec_possible_for_package.
    rv_possible = abap_true.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~confirm_transport_messages.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~create_transport_entries.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~get_r3tr_obj_for_limu_obj.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~get_transport_for_object.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~insert_transport_object.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~list_open_requests.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~list_r3tr_by_request.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~read.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~read_description.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~read_creation_dates.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~validate_transport_request.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~change_transport_type.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~read_request_and_tasks.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~is_object_type_customizing.
    RETURN.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_changed_by DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    CONSTANTS c_me TYPE syuname VALUE 'ME'.

    DATA mo_dot TYPE REF TO zcl_abapgit_dot_abapgit.
    DATA mt_remote TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA mo_cts_api TYPE REF TO ltd_cts_api.
    DATA mi_previous_settings TYPE REF TO zif_abapgit_persist_settings.
    DATA mi_previous_cts_api TYPE REF TO zif_abapgit_cts_api.

    METHODS setup.
    METHODS teardown.
    METHODS add_remote
      IMPORTING
        iv_filename TYPE string.
    METHODS get_users
      RETURNING
        VALUE(rt_users) TYPE zcl_abapgit_stage_changed_by=>ty_file_users_tt.
    METHODS assert_user
      IMPORTING
        it_users    TYPE zcl_abapgit_stage_changed_by=>ty_file_users_tt
        iv_filename TYPE string
        iv_user     TYPE syuname.

    METHODS remote_without_transport FOR TESTING.
    METHODS remote_users_from_transport FOR TESTING.
    METHODS read_owner_once_per_transport FOR TESTING.

ENDCLASS.

CLASS ltcl_changed_by IMPLEMENTATION.

  METHOD setup.

    DATA li_memory TYPE REF TO ltd_memory_settings.

    mo_dot = zcl_abapgit_dot_abapgit=>build_default( ).

    mi_previous_settings = zcl_abapgit_persist_factory=>get_settings( ).
    CREATE OBJECT li_memory.
    zcl_abapgit_persist_injector=>set_settings( li_memory ).

    mi_previous_cts_api = zcl_abapgit_factory=>get_cts_api( ).
    CREATE OBJECT mo_cts_api.
    zcl_abapgit_injector=>set_cts_api( mo_cts_api ).

  ENDMETHOD.

  METHOD teardown.

    zcl_abapgit_persist_injector=>set_settings( mi_previous_settings ).
    zcl_abapgit_injector=>set_cts_api( mi_previous_cts_api ).

  ENDMETHOD.

  METHOD add_remote.

    DATA ls_remote LIKE LINE OF mt_remote.

    ls_remote-path     = '/src/'.
    ls_remote-filename = iv_filename.
    APPEND ls_remote TO mt_remote.

  ENDMETHOD.

  METHOD get_users.

    rt_users = zcl_abapgit_stage_changed_by=>get_users_of_remote_files(
      it_remote = mt_remote
      io_dot    = mo_dot ).

  ENDMETHOD.

  METHOD assert_user.

    DATA ls_user LIKE LINE OF it_users.

    READ TABLE it_users INTO ls_user WITH TABLE KEY
      path     = '/src/'
      filename = iv_filename.
    cl_abap_unit_assert=>assert_subrc(
      exp = 0
      msg = |No user for { iv_filename }| ).
    cl_abap_unit_assert=>assert_equals(
      exp = iv_user
      act = ls_user-name ).

  ENDMETHOD.

  METHOD remote_without_transport.

    DATA lt_changed_by TYPE zcl_abapgit_stage_changed_by=>ty_changed_by_tt.
    DATA lt_transports TYPE zif_abapgit_cts_api=>ty_transport_list.
    DATA ls_files TYPE zif_abapgit_definitions=>ty_stage_files.
    DATA ls_changed_by LIKE LINE OF lt_changed_by.

    add_remote( 'zcl_deleted.clas.abap' ).
    ls_files-remote = mt_remote.

    lt_changed_by = zcl_abapgit_stage_changed_by=>find_changed_by(
      it_files      = ls_files
      io_dot        = mo_dot
      it_transports = lt_transports ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( lt_changed_by ) ).
    READ TABLE lt_changed_by INTO ls_changed_by INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      exp = 'ZCL_DELETED'
      act = ls_changed_by-obj_name ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'zcl_deleted.clas.abap'
      act = ls_changed_by-filename ).
    cl_abap_unit_assert=>assert_equals(
      exp = zcl_abapgit_objects_super=>c_user_unknown
      act = ls_changed_by-name ).

  ENDMETHOD.

  METHOD remote_users_from_transport.

    DATA lt_users TYPE zcl_abapgit_stage_changed_by=>ty_file_users_tt.

    add_remote( 'zcl_mine_deleted.clas.abap' ).
    add_remote( 'zcl_unknown_deleted.clas.abap' ).
    mo_cts_api->add_lock( iv_obj_name = 'zcl_mine_deleted'
                          iv_trkorr   = 'A4HK900001'
                          iv_user     = c_me ).

    lt_users = get_users( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lines( lt_users ) ).
    assert_user( it_users    = lt_users
                 iv_filename = 'zcl_mine_deleted.clas.abap'
                 iv_user     = c_me ).
    assert_user( it_users    = lt_users
                 iv_filename = 'zcl_unknown_deleted.clas.abap'
                 iv_user     = zcl_abapgit_objects_super=>c_user_unknown ).

  ENDMETHOD.

  METHOD read_owner_once_per_transport.

    DATA lt_users TYPE zcl_abapgit_stage_changed_by=>ty_file_users_tt.

    add_remote( 'zcl_mine_deleted.clas.abap' ).
    add_remote( 'zcl_mine_deleted.clas.xml' ).
    add_remote( 'zcl_mine_deleted.clas.testclasses.abap' ).
    mo_cts_api->add_lock( iv_obj_name = 'zcl_mine_deleted'
                          iv_trkorr   = 'A4HK900001'
                          iv_user     = c_me ).

    lt_users = get_users( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = mo_cts_api->mv_read_user_count ).
    assert_user( it_users    = lt_users
                 iv_filename = 'zcl_mine_deleted.clas.abap'
                 iv_user     = c_me ).
    assert_user( it_users    = lt_users
                 iv_filename = 'zcl_mine_deleted.clas.xml'
                 iv_user     = c_me ).
    assert_user( it_users    = lt_users
                 iv_filename = 'zcl_mine_deleted.clas.testclasses.abap'
                 iv_user     = c_me ).

  ENDMETHOD.

ENDCLASS.
