INTERFACE zif_abapgit_code_inspector
  PUBLIC .


  METHODS run
    IMPORTING
      !iv_variant    TYPE sci_chkv
      !iv_save       TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(rt_list) TYPE scit_alvlist
    RAISING
      zcx_abapgit_exception .
  METHODS is_successful
    RETURNING
      VALUE(rv_success) TYPE abap_bool .
ENDINTERFACE.
