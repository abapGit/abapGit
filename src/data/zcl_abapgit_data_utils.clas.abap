CLASS zcl_abapgit_data_utils DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS build_table_itab
      IMPORTING
        !iv_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rr_data) TYPE REF TO data
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS build_filename
      IMPORTING
        !is_config         TYPE zif_abapgit_data_config=>ty_config
      RETURNING
        VALUE(rv_filename) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_data_utils IMPLEMENTATION.


  METHOD build_filename.

    rv_filename = to_lower( |{ is_config-name }.{ is_config-type }.{ zif_abapgit_data_config=>c_default_format }| ).

    REPLACE ALL OCCURRENCES OF '/' IN rv_filename WITH '#'.

  ENDMETHOD.


  METHOD build_table_itab.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
    DATA lo_data TYPE REF TO cl_abap_datadescr.
    DATA lo_table TYPE REF TO cl_abap_tabledescr.

    cl_abap_structdescr=>describe_by_name(
      EXPORTING
        p_name         = iv_name
      RECEIVING
        p_descr_ref    = lo_type
      EXCEPTIONS
        type_not_found = 1 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Table { iv_name } not found for data serialization| ).
    ENDIF.

    TRY.
        lo_data ?= lo_type.
* todo, also add unique key corresponding to the db table, so duplicates cannot be returned
        lo_table = cl_abap_tabledescr=>create( lo_data ).
        CREATE DATA rr_data TYPE HANDLE lo_table.
      CATCH cx_root.
        zcx_abapgit_exception=>raise( |Error creating internal table for data serialization| ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
