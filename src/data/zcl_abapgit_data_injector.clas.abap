CLASS zcl_abapgit_data_injector DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS set_serializer
      IMPORTING
        !ii_serializer TYPE REF TO zif_abapgit_data_serializer .
    METHODS set_deserializer
      IMPORTING
        !ii_deserializer TYPE REF TO zif_abapgit_data_deserializer .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_DATA_INJECTOR IMPLEMENTATION.


  METHOD set_deserializer.
    zcl_abapgit_data_factory=>gi_deserializer = ii_deserializer.
  ENDMETHOD.


  METHOD set_serializer.
    zcl_abapgit_data_factory=>gi_serializer = ii_serializer.
  ENDMETHOD.
ENDCLASS.
