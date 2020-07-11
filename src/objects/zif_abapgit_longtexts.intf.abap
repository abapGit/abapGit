INTERFACE zif_abapgit_longtexts
  PUBLIC .

  METHODS:
    changed_by
      IMPORTING
        iv_object_name TYPE sobj_name
        iv_longtext_id TYPE dokil-id
        it_dokil       TYPE zif_abapgit_definitions=>tty_dokil OPTIONAL
      RETURNING
        VALUE(rv_user) TYPE xubname
      RAISING
        zcx_abapgit_exception,

    serialize
      IMPORTING
        iv_object_name TYPE sobj_name
        iv_longtext_id TYPE dokil-id
        it_dokil       TYPE zif_abapgit_definitions=>tty_dokil OPTIONAL
        io_xml         TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception,

    deserialize
      IMPORTING
        io_xml             TYPE REF TO zcl_abapgit_xml_input
        iv_master_language TYPE langu
      RAISING
        zcx_abapgit_exception,

    delete
      IMPORTING
        iv_object_name TYPE sobj_name
        iv_longtext_id TYPE dokil-id
      RAISING
        zcx_abapgit_exception.

ENDINTERFACE.
