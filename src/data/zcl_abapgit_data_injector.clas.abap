CLASS zcl_abapgit_data_injector DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS set_supporter
      IMPORTING
        !ii_supporter TYPE REF TO zif_abapgit_data_supporter .
    METHODS set_serializer
      IMPORTING
        !ii_serializer TYPE REF TO zif_abapgit_data_serializer .
    METHODS set_deserializer
      IMPORTING
        !ii_deserializer TYPE REF TO zif_abapgit_data_deserializer .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_data_injector IMPLEMENTATION.


  METHOD set_deserializer.
    zcl_abapgit_data_factory=>gi_deserializer = ii_deserializer.
  ENDMETHOD.


  METHOD set_serializer.
    zcl_abapgit_data_factory=>gi_serializer = ii_serializer.
  ENDMETHOD.


  METHOD set_supporter.
    zcl_abapgit_data_factory=>gi_supporter = ii_supporter.
  ENDMETHOD.
ENDCLASS.
