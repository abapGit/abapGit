INTERFACE zif_abapgit_sap_package
  PUBLIC .


  TYPES:
    ty_devclass_tt TYPE STANDARD TABLE OF devclass WITH DEFAULT KEY .

  METHODS create
    IMPORTING
      !is_package TYPE scompkdtln
    RAISING
      zcx_abapgit_exception .
  METHODS create_local
    RAISING
      zcx_abapgit_exception .
  METHODS list_subpackages
    IMPORTING
      !it_devc_info  TYPE zif_abapgit_definitions=>tt_devc_buffer OPTIONAL
    RETURNING
      VALUE(rt_list) TYPE ty_devclass_tt .
  METHODS list_superpackages
    IMPORTING
      !it_devc_info  TYPE zif_abapgit_definitions=>tt_devc_buffer OPTIONAL
    RETURNING
      VALUE(rt_list) TYPE ty_devclass_tt .
  METHODS read_parent
    IMPORTING
      !it_devc_info      TYPE zif_abapgit_definitions=>tt_devc_buffer OPTIONAL
    RETURNING
      VALUE(rv_parentcl) TYPE tdevc-parentcl .
  METHODS create_child
    IMPORTING
      !iv_child TYPE devclass
    RAISING
      zcx_abapgit_exception .
  METHODS exists
    RETURNING
      VALUE(rv_bool) TYPE abap_bool .
  METHODS are_changes_recorded_in_tr_req
    RETURNING
      VALUE(rv_are_changes_rec_in_tr_req) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS get_transport_type
    RETURNING
      VALUE(rv_transport_type) TYPE zif_abapgit_definitions=>ty_transport_type
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
