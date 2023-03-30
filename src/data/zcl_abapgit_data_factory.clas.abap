CLASS zcl_abapgit_data_factory DEFINITION
  PUBLIC
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_abapgit_data_injector .

  PUBLIC SECTION.

    CLASS-METHODS get_supporter
      RETURNING
        VALUE(ri_supporter) TYPE REF TO zif_abapgit_data_supporter .
    CLASS-METHODS get_serializer
      RETURNING
        VALUE(ri_serializer) TYPE REF TO zif_abapgit_data_serializer .
    CLASS-METHODS get_deserializer
      RETURNING
        VALUE(ri_deserializer) TYPE REF TO zif_abapgit_data_deserializer .
    CLASS-METHODS get_config
      RETURNING
        VALUE(ri_config) TYPE REF TO zif_abapgit_data_config .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gi_supporter TYPE REF TO zif_abapgit_data_supporter .
    CLASS-DATA gi_serializer TYPE REF TO zif_abapgit_data_serializer .
    CLASS-DATA gi_deserializer TYPE REF TO zif_abapgit_data_deserializer .
ENDCLASS.



CLASS zcl_abapgit_data_factory IMPLEMENTATION.


  METHOD get_config.
    CREATE OBJECT ri_config TYPE zcl_abapgit_data_config.
  ENDMETHOD.


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


  METHOD get_supporter.

    IF gi_supporter IS INITIAL.
      CREATE OBJECT gi_supporter TYPE zcl_abapgit_data_supporter.
    ENDIF.

    ri_supporter = gi_supporter.

  ENDMETHOD.
ENDCLASS.
