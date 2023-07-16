INTERFACE zif_abapgit_code_inspector
  PUBLIC .

  TYPES: BEGIN OF ty_result,
           objtype  TYPE tadir-object,
           objname  TYPE tadir-obj_name,
           sobjtype TYPE c LENGTH 4,
           sobjname TYPE c LENGTH 40,
           kind     TYPE c LENGTH 1,
           line     TYPE n LENGTH 6,
           col      TYPE n LENGTH 4,
           code     TYPE c LENGTH 10,
           test     TYPE c LENGTH 30,
           text     TYPE string,
           param1   TYPE c LENGTH 80,
         END OF ty_result.

  TYPES ty_results TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_variant,
           name        TYPE sci_chkv,
           description TYPE string,
         END OF ty_variant.

  TYPES ty_variants TYPE STANDARD TABLE OF ty_variant WITH DEFAULT KEY.

  METHODS run
    IMPORTING
      !iv_variant    TYPE sci_chkv
      !iv_save       TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(rt_list) TYPE ty_results
    RAISING
      zcx_abapgit_exception .

  METHODS is_successful
    RETURNING
      VALUE(rv_success) TYPE abap_bool .

  METHODS get_summary
    RETURNING
      VALUE(rv_summary) TYPE string.

  METHODS validate_check_variant
    IMPORTING
      !iv_check_variant_name TYPE sci_chkv
    RAISING
      zcx_abapgit_exception.

  METHODS list_global_variants
    RETURNING
      VALUE(rt_list) TYPE ty_variants.
ENDINTERFACE.
