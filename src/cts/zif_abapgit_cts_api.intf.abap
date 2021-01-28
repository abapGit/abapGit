INTERFACE zif_abapgit_cts_api
  PUBLIC .


  "! Returns the transport request / task the object is currently in
  "! @parameter is_item | Object
  "! @parameter iv_resolve_task_to_request | Return the transport request number
  "! @parameter rv_transport | Transport request / task
  "! @raising zcx_abapgit_exception | Object is not in a transport
  METHODS get_transport_for_object
    IMPORTING
      !is_item                    TYPE zif_abapgit_definitions=>ty_item
      !iv_resolve_task_to_request TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(rv_transport)         TYPE trkorr
    RAISING
      zcx_abapgit_exception .
  "! Check if change recording is possible for the given package
  "! @parameter iv_package | Package
  "! @parameter rv_possible | Change recording is possible
  "! @raising zcx_abapgit_exception | Package could not be loaded
  METHODS is_chrec_possible_for_package
    IMPORTING
      !iv_package        TYPE devclass
    RETURNING
      VALUE(rv_possible) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
