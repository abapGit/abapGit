INTERFACE zif_abapgit_object_enhs PUBLIC.

  METHODS:
    deserialize
      IMPORTING io_xml           TYPE REF TO zcl_abapgit_xml_input
                iv_package       TYPE devclass
                ii_enh_spot_tool TYPE REF TO if_enh_spot_tool
      RAISING   zcx_abapgit_exception,

    serialize
      IMPORTING io_xml           TYPE REF TO zcl_abapgit_xml_output
                ii_enh_spot_tool TYPE REF TO if_enh_spot_tool
      RAISING   zcx_abapgit_exception.

ENDINTERFACE.
