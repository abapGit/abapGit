class ZCL_ABAPGIT_OBJECT_TABL_COMPAR definition
  public
  create public .

public section.

  interfaces ZIF_ABAPGIT_COMPARATOR .

  methods CONSTRUCTOR
    importing
      !IO_LOCAL type ref to ZCL_ABAPGIT_XML_INPUT .
protected section.
private section.

  data MO_LOCAL type ref to ZCL_ABAPGIT_XML_INPUT .
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_TABL_COMPAR IMPLEMENTATION.


  METHOD constructor.

    mo_local = io_local.

  ENDMETHOD.


  METHOD zif_abapgit_comparator~compare.

    DATA: lo_table_validation TYPE REF TO zcl_abapgit_object_tabl_valid.


    CREATE OBJECT lo_table_validation.

    rs_result-text = lo_table_validation->validate(
      io_remote_version = io_remote
      io_local_version  = mo_local ).

  ENDMETHOD.
ENDCLASS.
