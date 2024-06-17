INTERFACE zif_abapgit_default_transport PUBLIC.

  TYPES: BEGIN OF ty_get,
           trfunction TYPE c LENGTH 1,
           ordernum   TYPE trkorr,
         END OF ty_get.

  METHODS set
    IMPORTING
      iv_transport TYPE trkorr
    RAISING
      zcx_abapgit_exception.

  METHODS reset
    RAISING
      zcx_abapgit_exception.

  METHODS get
    RETURNING
      VALUE(rs_default_task) TYPE ty_get
    RAISING
      zcx_abapgit_exception .

ENDINTERFACE.
