INTERFACE zif_abapgit_aff_intf_v1 PUBLIC.

  TYPES ty_category TYPE n LENGTH 2.

  CONSTANTS:
    BEGIN OF co_category,
      general                      TYPE ty_category VALUE '00',
      classic_badi                 TYPE ty_category VALUE '01',
      business_static_components   TYPE ty_category VALUE '51',
      business_instance_components TYPE ty_category VALUE '52',
      db_procedure_proxy           TYPE ty_category VALUE '65',
      web_dynpro_runtime           TYPE ty_category VALUE '80',
      enterprise_service           TYPE ty_category VALUE '90',
    END OF co_category.

  TYPES:
    BEGIN OF ty_main,
      format_version TYPE zif_abapgit_aff_types_v1=>ty_format_version,
      header         TYPE zif_abapgit_aff_types_v1=>ty_header_60_src,
      category       TYPE ty_category,
      proxy          TYPE abap_bool,
      descriptions   TYPE zif_abapgit_aff_oo_types_v1=>ty_descriptions,
    END OF ty_main.

ENDINTERFACE.
