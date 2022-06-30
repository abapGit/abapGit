*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_aff_type_mapping DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_aff_type_mapping.
ENDCLASS.

CLASS lcl_aff_type_mapping IMPLEMENTATION.

  METHOD zif_abapgit_aff_type_mapping~to_aff.
    DATA:
      ls_data_abapgit TYPE zcl_abapgit_object_intf=>ty_intf,
      ls_data_aff     TYPE zif_abapgit_aff_intf_v1=>ty_main.

    ls_data_abapgit = iv_data.

    " todo: to convert the data
    ls_data_aff-format_version = '42'.

    es_data = ls_data_aff.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_aff_serialize_metadata DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS serialize
      IMPORTING is_intf          TYPE data
      RETURNING VALUE(rv_result) TYPE xstring
      RAISING   zcx_abapgit_exception.
ENDCLASS.

CLASS lcl_aff_serialize_metadata IMPLEMENTATION.

  METHOD serialize.
    DATA:
      ls_data_abapgit TYPE zcl_abapgit_object_intf=>ty_intf,
      ls_data_aff     TYPE zif_abapgit_aff_intf_v1=>ty_main,
      lx_exception    TYPE REF TO cx_root,
      lo_ajson        TYPE REF TO zcl_abapgit_json_handler,
      lo_aff_mapper   TYPE REF TO zif_abapgit_aff_type_mapping.

    ls_data_abapgit = is_intf.

    CREATE OBJECT lo_aff_mapper TYPE lcl_aff_type_mapping.
    lo_aff_mapper->to_aff( EXPORTING iv_data = ls_data_abapgit
                           IMPORTING es_data = ls_data_aff ).
    CREATE OBJECT lo_ajson.
    TRY.
        rv_result = lo_ajson->serialize( ls_data_aff ).
      CATCH cx_root INTO lx_exception.
        zcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
