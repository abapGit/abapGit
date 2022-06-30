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
