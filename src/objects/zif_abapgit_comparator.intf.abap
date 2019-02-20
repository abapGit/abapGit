INTERFACE zif_abapgit_comparator
  PUBLIC .


  TYPES:
    BEGIN OF ty_result,
      text TYPE string,
    END OF ty_result .

  METHODS compare
    IMPORTING
      !io_remote       TYPE REF TO zcl_abapgit_xml_input
    RETURNING
      VALUE(rs_result) TYPE ty_result
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
