INTERFACE zif_abapgit_lxe_texts
  PUBLIC .

  TYPES:
    ty_text_pairs TYPE STANDARD TABLE OF lxe_pcx_s1 WITH DEFAULT KEY.

  METHODS serialize
    IMPORTING
      !iv_lxe_text_name TYPE string DEFAULT 'LXE_TEXTS'
      !iv_object_type   TYPE tadir-object
      !iv_object_name   TYPE tadir-obj_name
      !ii_xml           TYPE REF TO zif_abapgit_xml_output
    RAISING
      zcx_abapgit_exception .
  METHODS deserialize
    IMPORTING
      !iv_lxe_text_name TYPE string DEFAULT 'LXE_TEXTS'
      !iv_object_type   TYPE tadir-object OPTIONAL
      !iv_object_name   TYPE tadir-obj_name OPTIONAL
      !ii_xml           TYPE REF TO zif_abapgit_xml_input
    RAISING
      zcx_abapgit_exception .

  METHODS serialize_as_po
    IMPORTING
      !iv_object_type   TYPE tadir-object
      !iv_object_name   TYPE tadir-obj_name
      !is_i18n_params      TYPE zif_abapgit_definitions=>ty_i18n_params
    RETURNING
      VALUE(rt_po_files) TYPE zif_abapgit_i18n_file=>ty_table_of
    RAISING
      zcx_abapgit_exception .

ENDINTERFACE.
