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

    "! Returns the transport request / task the object is currently locked in
    "! @parameter iv_program_id | Program ID
    "! @parameter iv_object_type | Object type
    "! @parameter iv_object_name | Object name
    "! @parameter rv_transport | Transport request / task
    "! @raising zcx_abapgit_exception | Object is not locked in a transport
    METHODS get_current_transport_for_obj
      IMPORTING
        !iv_program_id      TYPE pgmid DEFAULT 'R3TR'
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
        !iv_program_id              TYPE pgmid DEFAULT 'R3TR'
        !iv_object_type             TYPE trobjtype
        !iv_object_name             TYPE sobj_name
      RETURNING
        VALUE(rv_transport)         TYPE trkorr
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
        !iv_program_id   TYPE pgmid DEFAULT 'R3TR'
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
        !iv_program_id     TYPE pgmid DEFAULT 'R3TR'
        !iv_object_type    TYPE trobjtype
      RETURNING
        VALUE(rv_lockable) TYPE abap_bool .
    "! Check if the object type can be transported
    "! @parameter iv_program_id | Program ID
    "! @parameter iv_object_type | Object type
    "! @parameter rv_transportable | Transportable
    METHODS is_object_type_transportable
      IMPORTING
        !iv_program_id          TYPE pgmid DEFAULT 'R3TR'
        !iv_object_type         TYPE trobjtype
      RETURNING
        VALUE(rv_transportable) TYPE abap_bool .
ENDCLASS.



CLASS ZCL_ABAPGIT_CTS_API IMPLEMENTATION.


  METHOD get_current_transport_for_obj.
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

    rv_transport = lv_transport_request.

  ENDMETHOD.


  METHOD get_current_transport_from_db.

    " This method is used for objects that are included in transports but not locked
    " for example, namespaces (NSPC)
    SELECT SINGLE a~trkorr FROM e070 AS a JOIN e071 AS b ON a~trkorr = b~trkorr
      INTO rv_transport
      WHERE ( a~trstatus = 'D' OR a~trstatus = 'L' )
      AND b~pgmid = iv_program_id AND b~object = iv_object_type AND b~obj_name = iv_object_name.

  ENDMETHOD.


  METHOD is_object_locked_in_transport.
    DATA: ls_object_key        TYPE e071,
          lv_type_check_result TYPE c LENGTH 1,
          lv_name_too_long     TYPE c LENGTH 1,
          lv_objlen            TYPE tlock-len,
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

    rv_transportable = boolc( lv_type_check_result CA 'RTL' ).
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
                         iv_object_name = is_item-obj_name  ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_cts_api~is_chrec_possible_for_package.
    IF iv_package IS NOT INITIAL.
      rv_possible = zcl_abapgit_factory=>get_sap_package( iv_package )->are_changes_recorded_in_tr_req( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
