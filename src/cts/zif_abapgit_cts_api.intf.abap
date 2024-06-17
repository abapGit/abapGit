INTERFACE zif_abapgit_cts_api
  PUBLIC .


  TYPES:
    BEGIN OF ty_transport,
      obj_type TYPE tadir-object,
      obj_name TYPE tadir-obj_name,
      trkorr   TYPE trkorr,
    END OF ty_transport .
  TYPES:
    ty_transport_list TYPE SORTED TABLE OF ty_transport WITH NON-UNIQUE KEY obj_type obj_name .
  TYPES:
    ty_trkorr_tt TYPE STANDARD TABLE OF trkorr WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_transport_key,
      object  TYPE e071k-object,
      objname TYPE e071k-objname,
      tabkey  TYPE e071k-tabkey,
    END OF ty_transport_key .
  TYPES:
    BEGIN OF ty_transport_data,
      trstatus TYPE e070-trstatus,
      keys     TYPE STANDARD TABLE OF ty_transport_key WITH DEFAULT KEY,
    END OF ty_transport_data .
  TYPES:
    BEGIN OF ty_transport_obj,
      object   TYPE e071-object,
      obj_name TYPE e071-obj_name,
    END OF ty_transport_obj .
  TYPES:
    ty_transport_obj_tt TYPE STANDARD TABLE OF ty_transport_obj WITH DEFAULT KEY .

  CONSTANTS:
    BEGIN OF c_transport_type,
      wb_request   TYPE c LENGTH 1 VALUE 'K', "workbench request
      wb_repair    TYPE c LENGTH 1 VALUE 'R', "workbench repair
      wb_task      TYPE c LENGTH 1 VALUE 'S', "workbench task
      cust_request TYPE c LENGTH 1 VALUE 'W', "customizing request
      cust_task    TYPE c LENGTH 1 VALUE 'Q', "customizing task
    END OF c_transport_type .
  CONSTANTS:
    BEGIN OF c_transport_category,
      workbench   TYPE c LENGTH 4 VALUE 'SYST',
      customizing TYPE c LENGTH 4 VALUE 'CUST',
    END OF c_transport_category .
  CONSTANTS:
    BEGIN OF c_transport_mode,
      insert TYPE c LENGTH 1 VALUE 'I',
      delete TYPE c LENGTH 1 VALUE 'D',
    END OF c_transport_mode .
  CONSTANTS:
    BEGIN OF c_transport_status,
      modifiable TYPE c LENGTH 1 VALUE 'D',
    END OF c_transport_status .

  METHODS confirm_transport_messages
    RETURNING
      VALUE(rv_messages_confirmed) TYPE abap_bool .
  METHODS create_transport_entries
    IMPORTING
      !iv_transport TYPE trkorr
      !it_table_ins TYPE ANY TABLE
      !it_table_upd TYPE ANY TABLE
      !it_table_del TYPE ANY TABLE
      !iv_tabname   TYPE tabname
    RAISING
      zcx_abapgit_exception .
  METHODS get_r3tr_obj_for_limu_obj
    IMPORTING
      !iv_object   TYPE tadir-object
      !iv_obj_name TYPE trobj_name
    EXPORTING
      !ev_object   TYPE tadir-object
      !ev_obj_name TYPE trobj_name
    RAISING
      zcx_abapgit_exception .
  METHODS get_transports_for_list
    IMPORTING
      !it_items            TYPE zif_abapgit_definitions=>ty_items_tt
    RETURNING
      VALUE(rt_transports) TYPE ty_transport_list
    RAISING
      zcx_abapgit_exception .
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
  METHODS insert_transport_object
    IMPORTING
      !iv_pgmid    TYPE tadir-pgmid DEFAULT 'R3TR'
      !iv_object   TYPE tadir-object
      !iv_obj_name TYPE csequence
      !iv_package  TYPE devclass
      !iv_language TYPE sy-langu DEFAULT sy-langu
      !iv_mode     TYPE c DEFAULT 'I'
    EXPORTING
      !ev_object   TYPE tadir-object
      !ev_obj_name TYPE trobj_name
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
  METHODS list_open_requests_by_user
    IMPORTING
      !iv_user         TYPE sy-uname DEFAULT sy-uname
    RETURNING
      VALUE(rt_trkorr) TYPE ty_trkorr_tt
    RAISING
      zcx_abapgit_exception .
  METHODS list_r3tr_by_request
    IMPORTING
      !iv_request    TYPE trkorr
    RETURNING
      VALUE(rt_list) TYPE ty_transport_obj_tt
    RAISING
      zcx_abapgit_exception .
  METHODS read
    IMPORTING
      !iv_trkorr        TYPE trkorr
    RETURNING
      VALUE(rs_request) TYPE ty_transport_data
    RAISING
      zcx_abapgit_exception .
  METHODS read_description
    IMPORTING
      !iv_trkorr            TYPE trkorr
    RETURNING
      VALUE(rv_description) TYPE string .
  METHODS read_user
    IMPORTING
      !iv_trkorr      TYPE trkorr
    RETURNING
      VALUE(rv_uname) TYPE uname .
  METHODS validate_transport_request
    IMPORTING
      !iv_transport_request TYPE trkorr
    RAISING
      zcx_abapgit_exception .

  METHODS change_transport_type
    IMPORTING
      !iv_transport_request   TYPE trkorr
      !iv_transport_type_from TYPE trfunction
      !iv_transport_type_to   TYPE trfunction
    RAISING
      zcx_abapgit_exception.
ENDINTERFACE.
