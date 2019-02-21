CLASS zcl_abapgit_object_tabl_compar DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_comparator .

    METHODS constructor
      IMPORTING
        !io_local TYPE REF TO zcl_abapgit_xml_input .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_local TYPE REF TO zcl_abapgit_xml_input .
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
