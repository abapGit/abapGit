"! Change transport system API
INTERFACE zif_abapgit_cts_api PUBLIC.
  TYPES:
    BEGIN OF gty_object,
      program_id  TYPE pgmid,
      object_type TYPE trobjtype,
      object_name TYPE sobj_name,
    END OF gty_object,
    BEGIN OF gty_object_transport.
      INCLUDE TYPE gty_object.
  TYPES:
    transport TYPE trkorr,
    END OF gty_object_transport,
    BEGIN OF gty_request_header,
      transport     TYPE trkorr,
      text          TYPE trordertxt,
      owner         TYPE syuname,
      type          TYPE string,
      changed_date  TYPE as4date,
      changed_time  TYPE as4time,
      layer         TYPE tarlayer,
      source_client TYPE trclient,
      target_client TYPE trclient,
      target_system TYPE tr_target,
    END OF gty_request_header,
    gty_object_tab           TYPE SORTED TABLE OF gty_object WITH UNIQUE KEY table_line,
    gty_object_transport_tab TYPE SORTED TABLE OF gty_object_transport
                                  WITH UNIQUE KEY program_id object_type object_name.
  METHODS:
    "! Returns the transport request / task the object is currently locked in
    "! @parameter iv_program_id | Program ID
    "! @parameter iv_object_type | Object type
    "! @parameter iv_object_name | Object name
    "! @parameter iv_resolve_task_to_request | Return the transport request number if the object is locked in a task
    "! @parameter rv_transport | Transport request / task
    "! @raising zcx_abapgit_exception | Object is not locked in a transport
    get_current_transport_for_obj IMPORTING iv_program_id              TYPE pgmid DEFAULT 'R3TR'
                                            iv_object_type             TYPE trobjtype
                                            iv_object_name             TYPE sobj_name
                                            iv_resolve_task_to_request TYPE abap_bool DEFAULT abap_true
                                  RETURNING VALUE(rv_transport)        TYPE trkorr
                                  RAISING   zcx_abapgit_exception,
    "! Check if the object is currently locked in a transport
    "! @parameter iv_program_id | Program ID
    "! @parameter iv_object_type | Object type
    "! @parameter iv_object_name | Object name
    "! @parameter rv_locked | Object is locked
    "! @raising zcx_abapgit_exception | Object type is not lockable
    is_object_locked_in_transport IMPORTING iv_program_id    TYPE pgmid DEFAULT 'R3TR'
                                            iv_object_type   TYPE trobjtype
                                            iv_object_name   TYPE sobj_name
                                  RETURNING VALUE(rv_locked) TYPE abap_bool
                                  RAISING   zcx_abapgit_exception,
    "! Check if the object type is lockable
    "! @parameter iv_program_id | Program ID
    "! @parameter iv_object_type | Object type
    "! @parameter rv_lockable | Lockable
    is_object_type_lockable IMPORTING iv_program_id      TYPE pgmid DEFAULT 'R3TR'
                                      iv_object_type     TYPE trobjtype
                            RETURNING VALUE(rv_lockable) TYPE abap_bool,
    "! Check if change recording is possible for the given package
    "! @parameter iv_package | Package
    "! @parameter rv_possible | Change recording is possible
    "! @raising zcx_abapgit_exception | Package could not be loaded
    is_chrec_possible_for_package IMPORTING iv_package         TYPE devclass
                                  RETURNING VALUE(rv_possible) TYPE abap_bool
                                  RAISING   zcx_abapgit_exception,
    "! Enriches a list of objects with their current transports
    "! @parameter it_objects | Objects
    "! @parameter iv_resolve_task_to_request | Return the transport request number if the object is locked in a task
    "! @parameter rt_objects | Objects with their transports
    get_current_trs_for_objs IMPORTING it_objects                 TYPE gty_object_tab
                                       iv_resolve_task_to_request TYPE abap_bool DEFAULT abap_true
                             RETURNING VALUE(rt_objects)          TYPE gty_object_transport_tab,
    "! Get header information for a transport request
    "! @parameter iv_transport | Transport request
    "! @parameter rs_header | Header
    "! @raising zcx_abapgit_exception | Error retrieving request information
    get_request_header IMPORTING iv_transport     TYPE trkorr
                       RETURNING VALUE(rs_header) TYPE gty_request_header
                       RAISING   zcx_abapgit_exception.
ENDINTERFACE.
