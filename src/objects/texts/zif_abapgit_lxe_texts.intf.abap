INTERFACE zif_abapgit_lxe_texts
  PUBLIC .

  TYPES:
    BEGIN OF ty_lxe_i18n,
      source_lang TYPE lxeisolang,
      target_lang TYPE lxeisolang,
      custmnr     TYPE lxecustmnr,
      objtype     TYPE trobjtype,
      objname     TYPE lxeobjname,
      text_pairs  TYPE STANDARD TABLE OF lxe_pcx_s1 WITH DEFAULT KEY,
    END OF ty_lxe_i18n .
  TYPES:
    ty_tlxe_i18n TYPE STANDARD TABLE OF ty_lxe_i18n WITH DEFAULT KEY .

  METHODS serialize
    IMPORTING
      !iv_lxe_text_name TYPE string DEFAULT 'LXE_TEXTS'
      !iv_object_type   TYPE trobjtype
      !iv_object_name   TYPE sobj_name
      !ii_xml           TYPE REF TO zif_abapgit_xml_output
    RAISING
      zcx_abapgit_exception .
  METHODS deserialize
    IMPORTING
      !iv_lxe_text_name TYPE string DEFAULT 'LXE_TEXTS'
      !iv_object_type   TYPE trobjtype OPTIONAL
      !iv_object_name   TYPE sobj_name OPTIONAL
      !ii_xml           TYPE REF TO zif_abapgit_xml_input
    RAISING
      zcx_abapgit_exception .

ENDINTERFACE.
