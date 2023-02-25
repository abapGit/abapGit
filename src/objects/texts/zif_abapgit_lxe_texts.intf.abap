INTERFACE zif_abapgit_lxe_texts
  PUBLIC .

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

ENDINTERFACE.
