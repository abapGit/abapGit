INTERFACE zif_abapgit_sap_package PUBLIC.

  TYPES: ty_devclass_tt TYPE STANDARD TABLE OF devclass WITH DEFAULT KEY.

  METHODS:
    list_subpackages
      RETURNING VALUE(rt_list) TYPE ty_devclass_tt,
    list_superpackages
      RETURNING VALUE(rt_list) TYPE ty_devclass_tt,
    read_parent
      RETURNING VALUE(rv_parentcl) TYPE tdevc-parentcl,
    create_child
      IMPORTING iv_child TYPE devclass
      RAISING   zcx_abapgit_exception,
    exists
      RETURNING VALUE(rv_bool) TYPE abap_bool,
    are_changes_recorded_in_tr_req
      RETURNING VALUE(rv_are_changes_rec_in_tr_req) TYPE abap_bool
      RAISING zcx_abapgit_exception.

ENDINTERFACE.
