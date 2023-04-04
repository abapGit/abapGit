INTERFACE zif_abapgit_data_deserializer
  PUBLIC .


  TYPES: BEGIN OF ty_result,
           type    TYPE zif_abapgit_data_config=>ty_config-type,
           name    TYPE zif_abapgit_data_config=>ty_config-name,
           deletes TYPE REF TO data,
           updates TYPE REF TO data,
           inserts TYPE REF TO data,
           file    TYPE zif_abapgit_git_definitions=>ty_file_signature,
           config  TYPE zif_abapgit_git_definitions=>ty_file_signature,
         END OF ty_result.
  TYPES: ty_results TYPE STANDARD TABLE OF ty_result WITH KEY type name.

  METHODS deserialize_check
    IMPORTING
      !io_repo         TYPE REF TO zcl_abapgit_repo
      !ii_config       TYPE REF TO zif_abapgit_data_config
    RETURNING
      VALUE(rs_checks) TYPE zif_abapgit_definitions=>ty_deserialize_checks-customizing
    RAISING
      zcx_abapgit_exception .

  METHODS deserialize
    IMPORTING
      !ii_config       TYPE REF TO zif_abapgit_data_config
      !it_files        TYPE zif_abapgit_git_definitions=>ty_files_tt
    RETURNING
      VALUE(rt_result) TYPE ty_results
    RAISING
      zcx_abapgit_exception .

  METHODS actualize
    IMPORTING
      !is_checks               TYPE zif_abapgit_definitions=>ty_deserialize_checks
      !it_result               TYPE ty_results
    RETURNING
      VALUE(rt_accessed_files) TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
