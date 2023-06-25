INTERFACE zif_abapgit_lxe_texts
  PUBLIC .

  TYPES:
    ty_text_pairs TYPE STANDARD TABLE OF lxe_pcx_s1 WITH DEFAULT KEY.

  METHODS serialize
    IMPORTING
      !iv_object_type   TYPE tadir-object
      !iv_object_name   TYPE tadir-obj_name
      !io_i18n_params   TYPE REF TO zcl_abapgit_i18n_params
      !ii_xml           TYPE REF TO zif_abapgit_xml_output
      !io_files         TYPE REF TO zcl_abapgit_objects_files
    RAISING
      zcx_abapgit_exception .
  METHODS deserialize
    IMPORTING
      !iv_object_type   TYPE tadir-object OPTIONAL
      !iv_object_name   TYPE tadir-obj_name OPTIONAL
      !io_i18n_params   TYPE REF TO zcl_abapgit_i18n_params
      !ii_xml           TYPE REF TO zif_abapgit_xml_input
      !io_files         TYPE REF TO zcl_abapgit_objects_files
    RAISING
      zcx_abapgit_exception .

ENDINTERFACE.
