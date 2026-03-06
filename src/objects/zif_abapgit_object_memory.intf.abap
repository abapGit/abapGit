INTERFACE zif_abapgit_object_memory PUBLIC.

  METHODS serialize_from_memory
    IMPORTING
      !io_xml TYPE REF TO zif_abapgit_xml_output
    RAISING
      zcx_abapgit_exception .

  METHODS deserialize_to_memory
    IMPORTING
      !io_xml TYPE REF TO zif_abapgit_xml_input
    RAISING
      zcx_abapgit_exception .

ENDINTERFACE.
