INTERFACE zif_abapgit_object
  PUBLIC .

  TYPES:
    ty_deserialization_step TYPE string.
  TYPES:
    ty_deserialization_step_tt TYPE STANDARD TABLE OF ty_deserialization_step
                                          WITH DEFAULT KEY .

  DATA mo_files TYPE REF TO zcl_abapgit_objects_files .

  CONSTANTS:
    BEGIN OF gc_step_id,
      abap TYPE zif_abapgit_object=>ty_deserialization_step VALUE `ABAP`,
      ddic TYPE zif_abapgit_object=>ty_deserialization_step VALUE `DDIC`,
      late TYPE zif_abapgit_object=>ty_deserialization_step VALUE `LATE`,
    END OF gc_step_id.

  CONSTANTS c_abap_version_sap_cp TYPE progdir-uccheck VALUE '5' ##NO_TEXT.
  CONSTANTS c_abap_version_default TYPE progdir-uccheck VALUE 'X' ##NO_TEXT.

  METHODS serialize
    IMPORTING
      !io_xml TYPE REF TO zcl_abapgit_xml_output
    RAISING
      zcx_abapgit_exception .
  METHODS deserialize
    IMPORTING
      !iv_package TYPE devclass
      !io_xml     TYPE REF TO zcl_abapgit_xml_input
      !iv_step    TYPE ty_deserialization_step
      !ii_log     TYPE REF TO zif_abapgit_log
    RAISING
      zcx_abapgit_exception .
  METHODS delete
    IMPORTING
      iv_package TYPE devclass
    RAISING
      zcx_abapgit_exception .
  METHODS exists
    RETURNING
      VALUE(rv_bool) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS is_locked
    RETURNING
      VALUE(rv_is_locked) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS is_active
    RETURNING
      VALUE(rv_active) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS changed_by
    RETURNING
      VALUE(rv_user) TYPE xubname
    RAISING
      zcx_abapgit_exception .
  METHODS jump
    RAISING
      zcx_abapgit_exception .
  METHODS get_metadata
    RETURNING
      VALUE(rs_metadata) TYPE zif_abapgit_definitions=>ty_metadata .
  METHODS get_comparator
    RETURNING
      VALUE(ri_comparator) TYPE REF TO zif_abapgit_comparator
    RAISING
      zcx_abapgit_exception .
  METHODS get_deserialize_steps
    RETURNING
      VALUE(rt_steps) TYPE ty_deserialization_step_tt .
ENDINTERFACE.
