CLASS zcl_abapgit_data_factory DEFINITION
  PUBLIC
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_abapgit_data_injector .

  PUBLIC SECTION.

    CLASS-METHODS get_supporter
      RETURNING
        VALUE(ri_supporter) TYPE REF TO zif_abapgit_data_supporter .
    CLASS-METHODS get_supporter_for_repo
      IMPORTING
        !io_repo            TYPE REF TO zif_abapgit_repo
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
    CLASS-METHODS set_current_repo
      IMPORTING
        !io_repo TYPE REF TO zif_abapgit_repo .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gi_supporter TYPE REF TO zif_abapgit_data_supporter .
    CLASS-DATA gi_serializer TYPE REF TO zif_abapgit_data_serializer .
    CLASS-DATA gi_deserializer TYPE REF TO zif_abapgit_data_deserializer .
    CLASS-DATA go_current_repo TYPE REF TO zif_abapgit_repo.
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

    " Use repository-specific supporter if repository context is available
    IF go_current_repo IS BOUND.
      ri_supporter = get_supporter_for_repo( go_current_repo ).
      RETURN.
    ENDIF.

    IF gi_supporter IS INITIAL.
      CREATE OBJECT gi_supporter TYPE zcl_abapgit_data_supporter.
    ENDIF.

    ri_supporter = gi_supporter.

  ENDMETHOD.


  METHOD get_supporter_for_repo.

    " Always create new instance for repository-specific supporter
    " to avoid caching issues with different repositories
    CREATE OBJECT ri_supporter TYPE zcl_abapgit_data_supporter
      EXPORTING
        io_repo = io_repo.

  ENDMETHOD.


  METHOD set_current_repo.
    go_current_repo = io_repo.
  ENDMETHOD.
ENDCLASS.
