INTERFACE zif_abapgit_aff_clas_v1 PUBLIC.

  TYPES ty_category TYPE n LENGTH 2.

  CONSTANTS:
    BEGIN OF co_category,
      general_object_type            TYPE ty_category VALUE '00',
      exit_class                     TYPE ty_category VALUE '01',
      testclass_abap_unit            TYPE ty_category VALUE '05',
      behavior_class                 TYPE ty_category VALUE '06',
      entity_event_handler           TYPE ty_category VALUE '07',
      persistent_class               TYPE ty_category VALUE '10',
      factory_for_persistent_class   TYPE ty_category VALUE '11',
      status_class_for_persist_class TYPE ty_category VALUE '12',
      rfc_proxy_class                TYPE ty_category VALUE '35',
      communication_connection_class TYPE ty_category VALUE '36',
      exception_class                TYPE ty_category VALUE '40',
      area_class_shared_objects      TYPE ty_category VALUE '45',
      business_class                 TYPE ty_category VALUE '50',
      bsp_application_class          TYPE ty_category VALUE '60',
      basis_class_bsp_element_hdlr   TYPE ty_category VALUE '70',
      web_dynpro_runtime_object      TYPE ty_category VALUE '80',
    END OF co_category.

  TYPES:
    BEGIN OF ty_main,
      format_version       TYPE zif_abapgit_aff_types_v1=>ty_format_version,
      header               TYPE zif_abapgit_aff_types_v1=>ty_header_60_src,
      category             TYPE ty_category,
      fix_point_arithmetic TYPE abap_bool,
      message_class        TYPE c LENGTH 20,
      descriptions         TYPE zif_abapgit_aff_oo_types_v1=>ty_descriptions,
    END OF ty_main.

ENDINTERFACE.
