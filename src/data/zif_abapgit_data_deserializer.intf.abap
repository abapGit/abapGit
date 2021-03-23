INTERFACE zif_abapgit_data_deserializer
  PUBLIC .


  TYPES: BEGIN OF ty_result,
           table   TYPE tadir-obj_name,
           deletes TYPE REF TO data,
           updates TYPE REF TO data,
           inserts TYPE REF TO data,
         END OF ty_result.
  TYPES: ty_results TYPE STANDARD TABLE OF ty_result WITH KEY table.

  METHODS deserialize
    IMPORTING
      !ii_config       TYPE REF TO zif_abapgit_data_config
      !it_files        TYPE zif_abapgit_definitions=>ty_files_tt
    RETURNING
      VALUE(rt_result) TYPE ty_results
    RAISING
      zcx_abapgit_exception .

  METHODS actualize
    IMPORTING
      it_result TYPE ty_results
    RAISING
      zcx_abapgit_exception .

ENDINTERFACE.
