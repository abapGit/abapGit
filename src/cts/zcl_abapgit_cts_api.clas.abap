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

    DATA mv_confirm_transp_msgs_called TYPE abap_bool.

    "! Returns the transport request / task the object is currently locked in
    "! @parameter iv_program_id | Program ID
    "! @parameter iv_object_type | Object type
    "! @parameter iv_object_name | Object name
    "! @parameter rv_transport | Transport request / task
    "! @raising zcx_abapgit_exception | Object is not locked in a transport
    METHODS get_current_transport_for_obj
      IMPORTING
        !iv_program_id      TYPE tadir-pgmid DEFAULT 'R3TR'
        !iv_object_type     TYPE trobjtype
        !iv_object_name     TYPE sobj_name
      RETURNING
        VALUE(rv_transport) TYPE trkorr
      RAISING
        zcx_abapgit_exception .
    "! Returns the transport request / task that includes the object (even if not locked)
    "! @parameter iv_program_id | Program ID
    "! @parameter iv_object_type | Object type
    "! @parameter iv_object_name | Object name
    "! @parameter rv_transport | Transport request / task
    "! @raising zcx_abapgit_exception | Object is not locked in a transport
    METHODS get_current_transport_from_db
      IMPORTING
        !iv_program_id      TYPE tadir-pgmid DEFAULT 'R3TR'
        !iv_object_type     TYPE trobjtype
        !iv_object_name     TYPE sobj_name
      RETURNING
        VALUE(rv_transport) TYPE trkorr
      RAISING
        zcx_abapgit_exception .
    "! Check if the object is currently locked in a transport
    "! @parameter iv_program_id | Program ID
    "! @parameter iv_object_type | Object type
    "! @parameter iv_object_name | Object name
    "! @parameter rv_locked | Object is locked
    "! @raising zcx_abapgit_exception | Object type is not lockable
    METHODS is_object_locked_in_transport
      IMPORTING
        !iv_program_id   TYPE tadir-pgmid DEFAULT 'R3TR'
        !iv_object_type  TYPE trobjtype
        !iv_object_name  TYPE sobj_name
      RETURNING
        VALUE(rv_locked) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    "! Check if the object type is lockable
    "! @parameter iv_program_id | Program ID
    "! @parameter iv_object_type | Object type
    "! @parameter rv_lockable | Lockable
    METHODS is_object_type_lockable
      IMPORTING
        !iv_program_id     TYPE tadir-pgmid DEFAULT 'R3TR'
        !iv_object_type    TYPE trobjtype
      RETURNING
        VALUE(rv_lockable) TYPE abap_bool .
    "! Check if the object type can be transported
    "! @parameter iv_program_id | Program ID
    "! @parameter iv_object_type | Object type
    "! @parameter rv_transportable | Transportable
    METHODS is_object_type_transportable
      IMPORTING
        !iv_program_id          TYPE tadir-pgmid DEFAULT 'R3TR'
        !iv_object_type         TYPE trobjtype
      RETURNING
        VALUE(rv_transportable) TYPE abap_bool .
ENDCLASS.



CLASS zcl_abapgit_cts_api IMPLEMENTATION.


  METHOD get_current_transport_for_obj.
    DATA: lv_object_lockable   TYPE abap_bool,
          lv_locked            TYPE abap_bool,
          lv_transport_request TYPE trkorr,
          ls_tlock             TYPE tlock,
          lt_tlock             TYPE STANDARD TABLE OF tlock WITH DEFAULT KEY,
          lt_transports        TYPE STANDARD TABLE OF trkorr WITH DEFAULT KEY,
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
      TABLES
        wt_tlock             = lt_tlock
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

    LOOP AT lt_tlock INTO ls_tlock.
      COLLECT ls_tlock-trkorr INTO lt_transports.
    ENDLOOP.

    IF lines( lt_transports ) = 1.
      rv_transport = lv_transport_request.
    ELSE.
      rv_transport = zif_abapgit_definitions=>c_multiple_transports.
    ENDIF.

  ENDMETHOD.


  METHOD get_current_transport_from_db.

    " This method is used for objects that are included in transports but not locked
    " for example, namespaces (NSPC) or table entries (TABU)
    " Ignore unreleased SAP piece lists
    SELECT SINGLE a~trkorr FROM e070 AS a JOIN e071 AS b ON a~trkorr = b~trkorr
      INTO rv_transport
      WHERE ( a~trstatus = 'D' OR a~trstatus = 'L' )
        AND a~trfunction <> 'G'
        AND NOT ( a~trfunction = 'F' AND ( a~tarsystem = '' OR a~tarsystem = 'SAP' ) )
        AND b~pgmid = iv_program_id AND b~object = iv_object_type AND b~obj_name = iv_object_name.

  ENDMETHOD.


  METHOD is_object_locked_in_transport.
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
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    rv_locked = boolc( lv_lock_flag <> space ).
  ENDMETHOD.


  METHOD is_object_type_lockable.
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


  METHOD is_object_type_transportable.
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

    rv_transportable = boolc( lv_type_check_result CA 'RTL' OR iv_object_type = 'TABU' ).
  ENDMETHOD.


  METHOD zif_abapgit_cts_api~change_transport_type.

    DATA:
      ls_request_header  TYPE trwbo_request_header,
      lt_request_headers TYPE trwbo_request_headers.

    CALL FUNCTION 'ENQUEUE_E_TRKORR'
      EXPORTING
        trkorr         = iv_transport_request
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
      EXPORTING
        iv_trkorr          = iv_transport_request
      IMPORTING
        et_request_headers = lt_request_headers
      EXCEPTIONS
        invalid_input      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    LOOP AT lt_request_headers INTO ls_request_header WHERE trfunction = iv_transport_type_from.

      CALL FUNCTION 'TRINT_READ_REQUEST_HEADER'
        EXPORTING
          iv_read_e070   = abap_true
          iv_read_e070c  = abap_true
        CHANGING
          cs_request     = ls_request_header
        EXCEPTIONS
          empty_trkorr   = 1
          not_exist_e070 = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      CALL FUNCTION 'TRINT_CHANGE_TRFUNCTION'
        EXPORTING
          iv_new_trfunction      = iv_transport_type_to
        CHANGING
          cs_request_header      = ls_request_header
        EXCEPTIONS
          action_aborted_by_user = 1
          change_not_allowed     = 2
          db_access_error        = 3
          OTHERS                 = 4.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDLOOP.

    CALL FUNCTION 'DEQUEUE_E_TRKORR'
      EXPORTING
        trkorr = iv_transport_request.

  ENDMETHOD.


  METHOD zif_abapgit_cts_api~confirm_transport_messages.

    TYPES: BEGIN OF ty_s_message,
             id TYPE symsgid,
             ty TYPE symsgty,
             no TYPE symsgno,
             v1 TYPE symsgv,
             v2 TYPE symsgv,
             v3 TYPE symsgv,
             v4 TYPE symsgv,
           END OF ty_s_message.

    DATA ls_message TYPE ty_s_message.

    FIELD-SYMBOLS: <lt_confirmed_messages> TYPE STANDARD TABLE.

    IF mv_confirm_transp_msgs_called = abap_true.
      RETURN.
    ENDIF.

    " remember the call to avoid duplicates in GT_CONFIRMED_MESSAGES
    mv_confirm_transp_msgs_called = abap_true.


    " Auto-confirm certain messages (requires SAP Note 1609940)
    PERFORM dummy IN PROGRAM saplstrd IF FOUND.  "load function group STRD once into memory

    ASSIGN ('(SAPLSTRD)GT_CONFIRMED_MESSAGES') TO <lt_confirmed_messages>.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Object can only be created in package of namespace
    ls_message-id = 'TR'.
    ls_message-no = '007'.
    INSERT ls_message INTO TABLE <lt_confirmed_messages>.

    " Original system set to "SAP"
    ls_message-id = 'TR'.
    ls_message-no = '013'.
    INSERT ls_message INTO TABLE <lt_confirmed_messages>.

    " Make repairs in foreign namespaces only if they are urgent
    ls_message-id = 'TR'.
    ls_message-no = '852'.
    INSERT ls_message INTO TABLE <lt_confirmed_messages>.

    " Make repairs in foreign namespaces only if they are urgent
    ls_message-id = 'TK'.
    ls_message-no = '016'.
    INSERT ls_message INTO TABLE <lt_confirmed_messages>.

    rv_messages_confirmed = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_cts_api~create_transport_entries.

    DATA lt_tables      TYPE tredt_objects.
    DATA lt_table_keys  TYPE STANDARD TABLE OF e071k.
    DATA lv_with_dialog TYPE abap_bool.

    FIELD-SYMBOLS <ls_table> LIKE LINE OF lt_tables.
    FIELD-SYMBOLS <ls_table_key> LIKE LINE OF lt_table_keys.

    cl_table_utilities_brf=>create_transport_entries(
      EXPORTING
        it_table_ins = it_table_ins
        it_table_upd = it_table_upd
        it_table_del = it_table_del
        iv_tabname   = iv_tabname
      CHANGING
        ct_e071      = lt_tables
        ct_e071k     = lt_table_keys ).

    " cl_table_utilities_brf=>write_transport_entries does not allow passing a request

    CALL FUNCTION 'TR_OBJECTS_CHECK'
      TABLES
        wt_ko200                = lt_tables
        wt_e071k                = lt_table_keys
      EXCEPTIONS
        cancel_edit_other_error = 1
        show_only_other_error   = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF iv_transport IS INITIAL.
      lv_with_dialog = abap_true.
    ENDIF.

    READ TABLE lt_tables ASSIGNING <ls_table> INDEX 1.
    ASSERT sy-subrc = 0.

    LOOP AT lt_table_keys ASSIGNING <ls_table_key>.
      <ls_table_key>-objfunc = <ls_table>-objfunc.
    ENDLOOP.

    CALL FUNCTION 'TR_OBJECT_INSERT'
      EXPORTING
        wi_order                = iv_transport
        wi_ko200                = <ls_table>
        iv_no_show_option       = abap_true
      TABLES
        wt_e071k                = lt_table_keys
      EXCEPTIONS
        cancel_edit_other_error = 1
        show_only_other_error   = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_cts_api~get_r3tr_obj_for_limu_obj.

    CLEAR ev_object.
    CLEAR ev_obj_name.

    CASE iv_object.
      WHEN 'MESS'.
        ev_object = 'MSAG'.
        ev_obj_name = substring( val = iv_obj_name
                                 len = strlen( iv_obj_name ) - 3 ).
      WHEN 'TABT'.
* Technical Attributes of a Table
        ev_object = 'TABL'.
        ev_obj_name = iv_obj_name.
      WHEN 'DTED'.
* Data Element Definition
        ev_object = 'DTEL'.
        ev_obj_name = iv_obj_name.
      WHEN 'DOMD'.
* Domain Definition
        ev_object = 'DOMA'.
        ev_obj_name = iv_obj_name.
      WHEN OTHERS.
        CALL FUNCTION 'GET_R3TR_OBJECT_FROM_LIMU_OBJ'
          EXPORTING
            p_limu_objtype = iv_object
            p_limu_objname = iv_obj_name
          IMPORTING
            p_r3tr_objtype = ev_object
            p_r3tr_objname = ev_obj_name
          EXCEPTIONS
            no_mapping     = 1
            OTHERS         = 2.
        IF sy-subrc <> 0 OR ev_obj_name IS INITIAL.
          zcx_abapgit_exception=>raise( |No R3TR Object found for { iv_object } { iv_obj_name }| ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_cts_api~get_transports_for_list.

    DATA lv_request TYPE trkorr.
    DATA lt_tlock TYPE SORTED TABLE OF tlock WITH NON-UNIQUE KEY object hikey.
    DATA ls_object_key TYPE e071.
    DATA lv_type_check_result TYPE c LENGTH 1.
    DATA ls_lock_key TYPE tlock_int.
    DATA ls_transport LIKE LINE OF rt_transports.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF it_items.
    FIELD-SYMBOLS <ls_tlock> LIKE LINE OF lt_tlock.

* Workarounds to improve performance, note that IT_ITEMS might
* contain 1000s of rows, see standard logic in function module
* TR_CHECK_OBJECT_LOCK

* avoid database lookups in TLOCK for each item,
    SELECT * FROM tlock INTO TABLE lt_tlock.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT it_items ASSIGNING <ls_item>.
      CLEAR lv_request.

      ls_object_key-pgmid = 'R3TR'.
      ls_object_key-object = <ls_item>-obj_type.
      ls_object_key-obj_name = <ls_item>-obj_name.

      CALL FUNCTION 'TR_CHECK_TYPE'
        EXPORTING
          wi_e071     = ls_object_key
        IMPORTING
          we_lock_key = ls_lock_key
          pe_result   = lv_type_check_result.

      IF lv_type_check_result = 'L'.
        LOOP AT lt_tlock ASSIGNING <ls_tlock>
            WHERE object = ls_lock_key-obj
            AND hikey >= ls_lock_key-low
            AND lokey <= ls_lock_key-hi.                  "#EC PORTABLE
          IF lv_request IS INITIAL.
            lv_request = <ls_tlock>-trkorr.
          ELSE.
            lv_request = zif_abapgit_definitions=>c_multiple_transports.
            EXIT.
          ENDIF.
        ENDLOOP.
      ELSEIF is_object_type_transportable( <ls_item>-obj_type ) = abap_true.
        lv_request = get_current_transport_from_db(
          iv_object_type = <ls_item>-obj_type
          iv_object_name = <ls_item>-obj_name ).
      ENDIF.

      IF lv_request IS NOT INITIAL.
        ls_transport-obj_type = <ls_item>-obj_type.
        ls_transport-obj_name = <ls_item>-obj_name.
        ls_transport-trkorr = lv_request.
        INSERT ls_transport INTO TABLE rt_transports.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_cts_api~get_transport_for_object.

    IF is_item-obj_type IS NOT INITIAL AND is_item-obj_name IS NOT INITIAL.

      IF is_object_type_lockable( is_item-obj_type ) = abap_true AND
         is_object_locked_in_transport(
           iv_object_type = is_item-obj_type
           iv_object_name = is_item-obj_name ) = abap_true.

        rv_transport = get_current_transport_for_obj(
          iv_object_type = is_item-obj_type
          iv_object_name = is_item-obj_name ).

      ELSEIF is_object_type_transportable( is_item-obj_type ) = abap_true.

        rv_transport = get_current_transport_from_db(
          iv_object_type = is_item-obj_type
          iv_object_name = is_item-obj_name ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_cts_api~insert_transport_object.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = iv_obj_name
        object_class        = iv_object
        devclass            = iv_package
        master_language     = iv_language
        korrnum             = iv_transport
        mode                = iv_mode
        global_lock         = abap_true
        suppress_dialog     = abap_true
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_cts_api~is_chrec_possible_for_package.
    IF iv_package IS NOT INITIAL.
      rv_possible = zcl_abapgit_factory=>get_sap_package( iv_package )->are_changes_recorded_in_tr_req( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_cts_api~list_open_requests.

    TYPES: BEGIN OF ty_e070,
             trkorr     TYPE e070-trkorr,
             trfunction TYPE e070-trfunction,
             strkorr    TYPE e070-strkorr,
           END OF ty_e070.
    DATA lt_e070 TYPE STANDARD TABLE OF ty_e070 WITH DEFAULT KEY.

* find all tasks first
    SELECT trkorr trfunction strkorr
      FROM e070 INTO TABLE lt_e070
      WHERE trstatus = zif_abapgit_cts_api=>c_transport_status-modifiable
      AND as4user IN it_user
      AND as4date IN it_date
      AND strkorr <> ''
      ORDER BY PRIMARY KEY.

    IF lines( lt_e070 ) > 0.
      SELECT trkorr FROM e070
        INTO TABLE rt_trkorr
        FOR ALL ENTRIES IN lt_e070
        WHERE trkorr = lt_e070-strkorr
        AND trfunction = zif_abapgit_cts_api=>c_transport_type-wb_request.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_cts_api~list_r3tr_by_request.

    TYPES: BEGIN OF ty_contents,
             trkorr   TYPE e071-trkorr,
             as4pos   TYPE e071-as4pos,
             pgmid    TYPE e071-pgmid,
             object   TYPE e071-object,
             obj_name TYPE e071-obj_name,
           END OF ty_contents.

    DATA lt_tasks    TYPE STANDARD TABLE OF trkorr WITH DEFAULT KEY.
    DATA lt_contents TYPE STANDARD TABLE OF ty_contents WITH DEFAULT KEY.
    DATA ls_contents LIKE LINE OF lt_contents.
    DATA ls_list     LIKE LINE OF rt_list.


    SELECT trkorr FROM e070 INTO TABLE lt_tasks
      WHERE strkorr = iv_request
      ORDER BY PRIMARY KEY.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT trkorr as4pos pgmid object obj_name FROM e071
      INTO TABLE lt_contents
      FOR ALL ENTRIES IN lt_tasks
      WHERE trkorr = lt_tasks-table_line
      ORDER BY PRIMARY KEY.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_contents INTO ls_contents.
      CASE ls_contents-pgmid.
        WHEN 'R3TR'.
          ls_list-object = ls_contents-object.
          ls_list-obj_name = ls_contents-obj_name.
          INSERT ls_list INTO TABLE rt_list.
        WHEN 'LIMU'.
          TRY.
              zif_abapgit_cts_api~get_r3tr_obj_for_limu_obj(
                EXPORTING
                  iv_object   = ls_contents-object
                  iv_obj_name = ls_contents-obj_name
                IMPORTING
                  ev_object   = ls_list-object
                  ev_obj_name = ls_list-obj_name ).
              INSERT ls_list INTO TABLE rt_list.
            CATCH zcx_abapgit_exception ##NO_HANDLER.
          ENDTRY.
      ENDCASE.
    ENDLOOP.

    SORT rt_list BY object obj_name.
    DELETE ADJACENT DUPLICATES FROM rt_list COMPARING object obj_name.

  ENDMETHOD.


  METHOD zif_abapgit_cts_api~read.

    DATA ls_request TYPE trwbo_request.
    DATA ls_key     LIKE LINE OF ls_request-keys.

    FIELD-SYMBOLS <ls_key> LIKE LINE OF rs_request-keys.


    ls_request-h-trkorr = iv_trkorr.

    CALL FUNCTION 'TRINT_READ_REQUEST'
      EXPORTING
        iv_read_e070       = abap_true
        iv_read_e07t       = abap_true
        iv_read_e070c      = abap_true
        iv_read_e070m      = abap_true
        iv_read_objs_keys  = abap_true
        iv_read_objs       = abap_true
        iv_read_attributes = abap_true
      CHANGING
        cs_request         = ls_request
      EXCEPTIONS
        error_occured      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

* move to output structure
    rs_request-trstatus = ls_request-h-trstatus.
    rs_request-as4date  = ls_request-h-as4date.
    LOOP AT ls_request-keys INTO ls_key.
      APPEND INITIAL LINE TO rs_request-keys ASSIGNING <ls_key>.
      MOVE-CORRESPONDING ls_key TO <ls_key>.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_cts_api~read_description.

    SELECT SINGLE as4text FROM e07t
      INTO rv_description
      WHERE trkorr = iv_trkorr
      AND langu = sy-langu.
    IF sy-subrc <> 0.
* fallback to any language
      SELECT SINGLE as4text FROM e07t
        INTO rv_description
        WHERE trkorr = iv_trkorr ##SUBRC_OK. "#EC CI_NOORDER
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_cts_api~read_user.

    SELECT SINGLE as4user FROM e070 INTO rv_uname
      WHERE trkorr = iv_trkorr ##SUBRC_OK.

  ENDMETHOD.


  METHOD zif_abapgit_cts_api~validate_transport_request.

    CONSTANTS:
      BEGIN OF c_tr_status,
        modifiable           TYPE trstatus VALUE 'D',
        modifiable_protected TYPE trstatus VALUE 'L',
      END OF c_tr_status.

    DATA ls_request TYPE zif_abapgit_cts_api=>ty_transport_data.

    ls_request = zif_abapgit_cts_api~read( iv_transport_request ).

    IF ls_request-trstatus <> c_tr_status-modifiable
        AND ls_request-trstatus <> c_tr_status-modifiable_protected.
      " Task/request &1 has already been released
      MESSAGE e064(tk) WITH iv_transport_request INTO zcx_abapgit_exception=>null.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
