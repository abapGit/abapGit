CLASS zcl_abapgit_data_factory DEFINITION
  PUBLIC
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_abapgit_data_injector .

  PUBLIC SECTION.

    CLASS-METHODS get_serializer
      RETURNING
        VALUE(ri_serializer) TYPE REF TO zif_abapgit_data_serializer .
    CLASS-METHODS get_deserializer
      RETURNING
        VALUE(ri_deserializer) TYPE REF TO zif_abapgit_data_deserializer .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gi_serializer TYPE REF TO zif_abapgit_data_serializer .
    CLASS-DATA gi_deserializer TYPE REF TO zif_abapgit_data_deserializer .
ENDCLASS.



CLASS ZCL_ABAPGIT_DATA_FACTORY IMPLEMENTATION.


  METHOD get_deserializer.

    IF gi_deserializer IS INITIAL.
      CREATE OBJECT gi_deserializer TYPE zcl_abapgit_data_deserializer.
    ENDIF.

    ri_deserializer = gi_deserializer.

  ENDMETHOD.


  METHOD get_serializer.

    IF gi_serializer IS INITIAL.
      CREATE OBJECT gi_serializer TYPE zcl_abapgit_data_serializer.
    ENDIF.

    ri_serializer = gi_serializer.

  ENDMETHOD.
ENDCLASS.
