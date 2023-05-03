INTERFACE zif_abapgit_object
  PUBLIC .

  DATA mo_files TYPE REF TO zcl_abapgit_objects_files .

  CONSTANTS:
    BEGIN OF gc_step_id,
      early TYPE zif_abapgit_definitions=>ty_deserialization_step VALUE `EARLY`,
      abap  TYPE zif_abapgit_definitions=>ty_deserialization_step VALUE `ABAP`,
      ddic  TYPE zif_abapgit_definitions=>ty_deserialization_step VALUE `DDIC`,
      late  TYPE zif_abapgit_definitions=>ty_deserialization_step VALUE `LATE`,
    END OF gc_step_id.

  METHODS serialize
    IMPORTING
      !io_xml TYPE REF TO zif_abapgit_xml_output
    RAISING
      zcx_abapgit_exception .

  METHODS deserialize
    IMPORTING
      !iv_package   TYPE devclass
      !io_xml       TYPE REF TO zif_abapgit_xml_input
      !iv_step      TYPE zif_abapgit_definitions=>ty_deserialization_step
      !ii_log       TYPE REF TO zif_abapgit_log
      !iv_transport TYPE trkorr
    RAISING
      zcx_abapgit_exception .

  METHODS delete
    IMPORTING
      !iv_package   TYPE devclass
      !iv_transport TYPE trkorr
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
    IMPORTING
      !iv_extra      TYPE string OPTIONAL
    RETURNING
      VALUE(rv_user) TYPE syuname
    RAISING
      zcx_abapgit_exception .

  METHODS jump
    IMPORTING
      !iv_extra      TYPE string OPTIONAL
    RETURNING
      VALUE(rv_exit) TYPE abap_bool
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
      VALUE(rt_steps) TYPE zif_abapgit_definitions=>ty_deserialization_step_tt .

  METHODS get_deserialize_order
    IMPORTING
      !it_all_objects          TYPE zif_abapgit_definitions=>ty_items_tt
    RETURNING
      VALUE(rt_objects_before) TYPE zif_abapgit_definitions=>ty_items_tt.

  CLASS-METHODS map_filename_to_object
    IMPORTING
      !iv_filename TYPE string
      !iv_path     TYPE string OPTIONAL
      !io_dot      TYPE REF TO zcl_abapgit_dot_abapgit OPTIONAL
      !iv_package  TYPE devclass OPTIONAL
    CHANGING
      cs_item      TYPE zif_abapgit_definitions=>ty_item
    RAISING
      zcx_abapgit_exception.

  CLASS-METHODS map_object_to_filename
    IMPORTING
      !is_item    TYPE zif_abapgit_definitions=>ty_item
    CHANGING
      cv_filename TYPE string
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
