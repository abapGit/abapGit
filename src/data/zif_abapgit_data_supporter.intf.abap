INTERFACE zif_abapgit_data_supporter
  PUBLIC.

  TYPES:
    BEGIN OF ty_object,
      type TYPE zif_abapgit_data_config=>ty_config-type,
      name TYPE zif_abapgit_data_config=>ty_config-name,
    END OF ty_object.
  TYPES:
    ty_objects TYPE SORTED TABLE OF ty_object WITH UNIQUE KEY type name.

  METHODS is_object_supported
    IMPORTING
      !iv_type            TYPE ty_object-type
      !iv_name            TYPE ty_object-name
    RETURNING
      VALUE(rv_supported) TYPE abap_bool.

ENDINTERFACE.
