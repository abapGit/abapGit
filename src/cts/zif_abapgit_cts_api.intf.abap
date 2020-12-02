"! Change transport system API
INTERFACE zif_abapgit_cts_api PUBLIC.
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
                                  RAISING   zcx_abapgit_exception.
ENDINTERFACE.
