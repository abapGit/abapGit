INTERFACE zif_abapgit_longtexts
  PUBLIC .

  TYPES:
    BEGIN OF ty_longtext,
      dokil TYPE dokil,
      head  TYPE thead,
      lines TYPE tline_tab,
    END OF ty_longtext .
  TYPES:
    ty_longtexts TYPE STANDARD TABLE OF ty_longtext WITH NON-UNIQUE DEFAULT KEY .

  METHODS changed_by
    IMPORTING
      !iv_object_name TYPE tadir-obj_name
      !iv_longtext_id TYPE dokil-id
      !it_dokil       TYPE zif_abapgit_definitions=>ty_dokil_tt OPTIONAL
    RETURNING
      VALUE(rv_user)  TYPE syuname
    RAISING
      zcx_abapgit_exception .
  METHODS serialize
    IMPORTING
      !iv_longtext_name   TYPE string DEFAULT 'LONGTEXTS'
      !iv_object_name     TYPE clike
      !iv_longtext_id     TYPE dokil-id
      !it_dokil           TYPE zif_abapgit_definitions=>ty_dokil_tt OPTIONAL
      !ii_xml             TYPE REF TO zif_abapgit_xml_output
    RETURNING
      VALUE(rt_longtexts) TYPE ty_longtexts
    RAISING
      zcx_abapgit_exception .
  METHODS deserialize
    IMPORTING
      !iv_longtext_name TYPE string DEFAULT 'LONGTEXTS'
      !iv_object_name   TYPE clike
      !iv_longtext_id   TYPE dokil-id
      !ii_xml           TYPE REF TO zif_abapgit_xml_input
      !iv_main_language TYPE sy-langu
    RAISING
      zcx_abapgit_exception .
  METHODS delete
    IMPORTING
      !iv_object_name TYPE tadir-obj_name
      !iv_longtext_id TYPE dokil-id
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
