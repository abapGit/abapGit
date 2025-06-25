INTERFACE zif_abapgit_tadir
  PUBLIC .


  METHODS get_object_package
    IMPORTING
      !iv_pgmid          TYPE tadir-pgmid DEFAULT 'R3TR'
      !iv_object         TYPE tadir-object
      !iv_obj_name       TYPE tadir-obj_name
    RETURNING
      VALUE(rv_devclass) TYPE tadir-devclass
    RAISING
      zcx_abapgit_exception .

  METHODS read
    IMPORTING
      !iv_package            TYPE tadir-devclass
      !iv_ignore_subpackages TYPE abap_bool DEFAULT abap_false
      !iv_only_local_objects TYPE abap_bool DEFAULT abap_false
      !io_dot                TYPE REF TO zcl_abapgit_dot_abapgit OPTIONAL
      !ii_log                TYPE REF TO zif_abapgit_log OPTIONAL
      !it_filter             TYPE zif_abapgit_definitions=>ty_tadir_tt OPTIONAL
      !iv_check_exists       TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(rt_tadir)        TYPE zif_abapgit_definitions=>ty_tadir_tt
    RAISING
      zcx_abapgit_exception .

  METHODS read_single
    IMPORTING
      !iv_pgmid       TYPE tadir-pgmid DEFAULT 'R3TR'
      !iv_object      TYPE tadir-object
      !iv_obj_name    TYPE tadir-obj_name
    RETURNING
      VALUE(rs_tadir) TYPE zif_abapgit_definitions=>ty_tadir.

  METHODS insert_single
    IMPORTING
      !iv_pgmid       TYPE csequence DEFAULT 'R3TR'
      !iv_object      TYPE csequence
      !iv_obj_name    TYPE csequence
      !iv_package     TYPE csequence OPTIONAL
      !iv_language    TYPE tadir-masterlang OPTIONAL
      !iv_srcsystem   TYPE tadir-srcsystem OPTIONAL
      !iv_set_genflag TYPE abap_bool DEFAULT abap_false
      !iv_set_edtflag TYPE abap_bool DEFAULT abap_false
    RAISING
      zcx_abapgit_exception.

  METHODS delete_single
    IMPORTING
      !iv_pgmid    TYPE csequence DEFAULT 'R3TR'
      !iv_object   TYPE csequence
      !iv_obj_name TYPE csequence
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
