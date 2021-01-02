CLASS zcl_abapgit_data_utils DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS build_table_itab
      IMPORTING
        !iv_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rr_data) TYPE REF TO data .

    CLASS-METHODS build_filename
      IMPORTING
        is_config          TYPE zif_abapgit_data_config=>ty_config
      RETURNING
        VALUE(rv_filename) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_DATA_UTILS IMPLEMENTATION.


  METHOD build_filename.

    rv_filename = to_lower( |{ is_config-name }.{ is_config-type }.json| ).

  ENDMETHOD.


  METHOD build_table_itab.

    DATA lo_structure TYPE REF TO cl_abap_structdescr.
    DATA lo_table TYPE REF TO cl_abap_tabledescr.

    lo_structure ?= cl_abap_structdescr=>describe_by_name( iv_name ).
* todo, also add unique key corresponding to the db table, so duplicates cannot be returned
    lo_table = cl_abap_tabledescr=>create( lo_structure ).
    CREATE DATA rr_data TYPE HANDLE lo_table.

  ENDMETHOD.
ENDCLASS.
