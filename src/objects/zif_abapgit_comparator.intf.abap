INTERFACE zif_abapgit_comparator
  PUBLIC .


  TYPES:
    BEGIN OF ty_result,
      text TYPE string,
    END OF ty_result .

  METHODS compare
    IMPORTING
      !ii_remote       TYPE REF TO zif_abapgit_xml_input
      !ii_log          TYPE REF TO zif_abapgit_log
    RETURNING
      VALUE(rs_result) TYPE ty_result
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
