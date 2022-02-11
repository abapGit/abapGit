INTERFACE zif_abapgit_object_filter
  PUBLIC .

  METHODS get_filter
    RETURNING
      VALUE(rt_filter) TYPE zif_abapgit_definitions=>ty_tadir_tt
    RAISING
      zcx_abapgit_exception.
ENDINTERFACE.
