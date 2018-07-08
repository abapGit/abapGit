CLASS zcl_abapgit_objects_generic DEFINITION PUBLIC CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !is_item TYPE zif_abapgit_definitions=>ty_item .
    METHODS: serialize
      IMPORTING io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING   zcx_abapgit_exception,
      deserialize
        IMPORTING iv_package TYPE devclass
                  io_xml     TYPE REF TO zcl_abapgit_xml_input
        RAISING   zcx_abapgit_exception,
      delete
        RAISING zcx_abapgit_exception,
      exists
        RETURNING VALUE(rv_bool) TYPE abap_bool
        RAISING   zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_objh TYPE objh .
    DATA ms_item TYPE zif_abapgit_definitions=>ty_item .
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECTS_GENERIC IMPLEMENTATION.


  METHOD constructor.

    SELECT SINGLE * FROM objh INTO ms_objh
      WHERE objectname = is_item-obj_type
      AND objecttype = 'L'.
    IF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.

  ENDMETHOD.


  METHOD delete.
  ENDMETHOD.


  METHOD deserialize.
  ENDMETHOD.


  METHOD exists.

    rv_bool = abap_true.

  ENDMETHOD.


  METHOD serialize.
  ENDMETHOD.
ENDCLASS.
