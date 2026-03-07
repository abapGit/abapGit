INTERFACE zif_abapgit_object_memory PUBLIC.

  METHODS deserialize_to_memory
    IMPORTING
      !io_xml TYPE REF TO zif_abapgit_xml_input
    RAISING
      zcx_abapgit_exception .

ENDINTERFACE.
