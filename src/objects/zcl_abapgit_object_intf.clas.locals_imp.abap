*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_aff_type_mapping DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abapit_aff_type_mapping.
ENDCLASS.

CLASS lcl_aff_type_mapping IMPLEMENTATION.

  METHOD zif_abapit_aff_type_mapping~to_aff.
    DATA:
      data_abapgit TYPE zcl_abapgit_object_intf=>ty_intf,
      data_aff     TYPE zif_abapgit_aff_intf_v1=>ty_main.

    data_abapgit = iv_data.

    " todo: to convert the data
    data_aff-format_version = '42'.

    rs_data = data_aff.
  ENDMETHOD.

ENDCLASS.
