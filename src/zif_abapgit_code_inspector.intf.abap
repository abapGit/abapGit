INTERFACE zif_abapgit_code_inspector
  PUBLIC .


  METHODS run
    IMPORTING
      !iv_variant    TYPE sci_chkv
    RETURNING
      VALUE(rt_list) TYPE scit_alvlist
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
