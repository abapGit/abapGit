INTERFACE zif_abapgit_sap_package PUBLIC.

  TYPES: ty_devclass_tt TYPE STANDARD TABLE OF devclass WITH DEFAULT KEY.
  METHODS:
    create
      IMPORTING is_package TYPE scompkdtln
      RAISING   zcx_abapgit_exception,
    create_local
      RAISING zcx_abapgit_exception,
    list_subpackages
      RETURNING VALUE(rt_list) TYPE ty_devclass_tt,
    list_superpackages
      RETURNING VALUE(rt_list) TYPE ty_devclass_tt
      RAISING   zcx_abapgit_exception,
    read_parent
      RETURNING VALUE(rv_parentcl) TYPE tdevc-parentcl
      RAISING   zcx_abapgit_exception,
    create_child
      IMPORTING iv_child TYPE devclass
      RAISING   zcx_abapgit_exception,
    exists
      RETURNING VALUE(rv_bool) TYPE abap_bool,
    are_changes_recorded_in_tr_req
      RETURNING VALUE(rv_are_changes_rec_in_tr_req) TYPE abap_bool
      RAISING   zcx_abapgit_exception,
    get_transport_type
      RETURNING VALUE(rs_transport_type) TYPE zif_abapgit_definitions=>ty_transport_type
      RAISING   zcx_abapgit_exception.

ENDINTERFACE.
