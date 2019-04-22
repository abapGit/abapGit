"! Change transport system API
CLASS zcl_abapgit_cts_api DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_cts_api.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_cts_api IMPLEMENTATION.
  METHOD zif_abapgit_cts_api~get_current_transport_for_obj.
    DATA: lv_object_lockable   TYPE abap_bool,
          lv_locked            TYPE abap_bool,
          lv_transport_request TYPE trkorr,
          lv_task              TYPE trkorr,
          lv_tr_object_name    TYPE trobj_name.

    lv_tr_object_name = iv_object_name.

    CALL FUNCTION 'TR_CHECK_OBJECT_LOCK'
      EXPORTING
        wi_pgmid             = iv_program_id
        wi_object            = iv_object_type
        wi_objname           = lv_tr_object_name
      IMPORTING
        we_lockable_object   = lv_object_lockable
        we_locked            = lv_locked
        we_lock_order        = lv_transport_request
        we_lock_task         = lv_task
      EXCEPTIONS
        empty_key            = 1
        no_systemname        = 2
        no_systemtype        = 3
        unallowed_lock_order = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF lv_locked = abap_false.
      zcx_abapgit_exception=>raise( |Object { iv_program_id }-{ iv_object_type }-{ iv_object_name } is not locked| ).
    ENDIF.

    IF lv_object_lockable = abap_false.
      zcx_abapgit_exception=>raise( |Object type { iv_program_id }-{ iv_object_type } not lockable| ).
    ENDIF.

    IF lv_task IS NOT INITIAL AND lv_task <> lv_transport_request AND iv_resolve_task_to_request = abap_false.
      rv_transport = lv_task.
    ELSE.
      rv_transport = lv_transport_request.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~is_object_locked_in_transport.
    DATA: ls_object_key        TYPE e071,
          lv_type_check_result TYPE c LENGTH 1,
          ls_lock_key          TYPE tlock_int,
          lv_lock_flag         TYPE c LENGTH 1.

    ls_object_key-pgmid = iv_program_id.
    ls_object_key-object = iv_object_type.
    ls_object_key-obj_name = iv_object_name.

    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING
        wi_e071     = ls_object_key
      IMPORTING
        pe_result   = lv_type_check_result
        we_lock_key = ls_lock_key.

    IF lv_type_check_result <> 'L'.
      zcx_abapgit_exception=>raise( |Object type { iv_program_id }-{ iv_object_type } not lockable| ).
    ENDIF.

    CALL FUNCTION 'TRINT_CHECK_LOCKS'
      EXPORTING
        wi_lock_key = ls_lock_key
      IMPORTING
        we_lockflag = lv_lock_flag
      EXCEPTIONS
        empty_key   = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |TRINT_CHECK_LOCKS: { sy-subrc }| ).
    ENDIF.

    rv_locked = boolc( lv_lock_flag <> space ).
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~is_object_type_lockable.
    DATA: ls_object_key        TYPE e071,
          lv_type_check_result TYPE c LENGTH 1.

    ls_object_key-pgmid = iv_program_id.
    ls_object_key-object = iv_object_type.
    ls_object_key-obj_name = '_'. " Dummy value #2071

    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING
        wi_e071   = ls_object_key
      IMPORTING
        pe_result = lv_type_check_result.

    rv_lockable = boolc( lv_type_check_result = 'L' ).
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~is_chrec_possible_for_package.
    rv_possible = zcl_abapgit_factory=>get_sap_package( iv_package )->are_changes_recorded_in_tr_req( ).
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~get_current_trs_for_objs.
    FIELD-SYMBOLS: <ls_object> LIKE LINE OF it_objects,
                   <ls_new>    LIKE LINE OF rt_objects.
    DATA: ls_new_line LIKE LINE OF rt_objects.

    LOOP AT it_objects ASSIGNING <ls_object>.
      TRY.
          CLEAR ls_new_line.
          MOVE-CORRESPONDING <ls_object> TO ls_new_line.
          INSERT ls_new_line INTO TABLE rt_objects ASSIGNING <ls_new>.
          <ls_new>-transport = zif_abapgit_cts_api~get_current_transport_for_obj(
            iv_program_id  = ls_new_line-program_id
            iv_object_type = ls_new_line-object_type
            iv_object_name = ls_new_line-object_name
          ).
        CATCH zcx_abapgit_exception ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_abapgit_cts_api~get_request_header.
    DATA: ls_request TYPE trwbo_request.

    CALL FUNCTION 'TR_READ_REQUEST'
      EXPORTING
        iv_read_e07t     = abap_true
        iv_read_e070     = abap_true
        iv_trkorr        = iv_transport
      CHANGING
        cs_request       = ls_request
      EXCEPTIONS
        error_occured    = 1
        no_authorization = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    rs_header-transport = ls_request-h-trkorr.
    rs_header-owner = ls_request-h-as4user.
    rs_header-changed_date = ls_request-h-as4date.
    rs_header-changed_time = ls_request-h-as4time.
    rs_header-text = ls_request-h-as4text.
    rs_header-source_client = ls_request-h-client.
    rs_header-target_client = ls_request-h-tarclient.
    rs_header-layer = ls_request-h-tarlayer.
    rs_header-target_system = ls_request-h-tarsystem.
  ENDMETHOD.
ENDCLASS.
