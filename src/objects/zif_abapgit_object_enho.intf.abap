INTERFACE zif_abapgit_object_enho PUBLIC.

  METHODS:
    deserialize
      IMPORTING ii_xml     TYPE REF TO zif_abapgit_xml_input
                iv_package TYPE devclass
      RAISING   zcx_abapgit_exception,
    serialize
      IMPORTING ii_xml      TYPE REF TO zif_abapgit_xml_output
                ii_enh_tool TYPE REF TO if_enh_tool
      RAISING   zcx_abapgit_exception.

ENDINTERFACE.
