INTERFACE zif_abapgit_cts_api
  PUBLIC .

  TYPES: BEGIN OF ty_transport,
           obj_type TYPE tadir-object,
           obj_name TYPE tadir-obj_name,
           trkorr   TYPE trkorr,
         END OF ty_transport.

  TYPES ty_transport_list TYPE SORTED TABLE OF ty_transport WITH NON-UNIQUE KEY obj_type obj_name.

  "! Returns the transport request / task the object is currently in
  "! @parameter is_item | Object
  "! @parameter rv_transport | Transport request / task
  "! @raising zcx_abapgit_exception | Object is not in a transport
  METHODS get_transport_for_object
    IMPORTING
      !is_item            TYPE zif_abapgit_definitions=>ty_item
    RETURNING
      VALUE(rv_transport) TYPE trkorr
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
  METHODS get_transports_for_list
    IMPORTING
      !it_items            TYPE zif_abapgit_definitions=>ty_items_tt
    RETURNING
      VALUE(rt_transports) TYPE ty_transport_list
    RAISING
      zcx_abapgit_exception .
  METHODS get_r3tr_obj_for_limu_obj
    IMPORTING
      iv_object   TYPE tadir-object
      iv_obj_name TYPE trobj_name
    EXPORTING
      ev_object   TYPE tadir-object
      ev_obj_name TYPE trobj_name
    RAISING
      zcx_abapgit_exception .

  METHODS read_description
    IMPORTING
      iv_trkorr             TYPE trkorr
    RETURNING
      VALUE(rv_description) TYPE string.

  METHODS read_user
    IMPORTING
      iv_trkorr       TYPE trkorr
    RETURNING
      VALUE(rv_uname) TYPE uname.

ENDINTERFACE.
