CLASS zcl_abapgit_data_factory DEFINITION
  PUBLIC
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_abapgit_data_injector .

  PUBLIC SECTION.

    METHODS get_serializer
      RETURNING
        VALUE(ri_serializer) TYPE REF TO zif_abapgit_data_serializer .
    METHODS get_deserializer
      RETURNING
        VALUE(ri_deserializer) TYPE REF TO zif_abapgit_data_deserializer .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA mi_serializer TYPE REF TO zif_abapgit_data_serializer .
    CLASS-DATA mi_deserializer TYPE REF TO zif_abapgit_data_deserializer .
ENDCLASS.



CLASS ZCL_ABAPGIT_DATA_FACTORY IMPLEMENTATION.


  METHOD get_deserializer.

    IF mi_deserializer IS INITIAL.
      CREATE OBJECT mi_deserializer TYPE zcl_abapgit_data_deserializer.
    ENDIF.

    ri_deserializer = mi_deserializer.

  ENDMETHOD.


  METHOD get_serializer.

    IF mi_serializer IS INITIAL.
      CREATE OBJECT mi_serializer TYPE zcl_abapgit_data_serializer.
    ENDIF.

    ri_serializer = mi_serializer.

  ENDMETHOD.
ENDCLASS.
