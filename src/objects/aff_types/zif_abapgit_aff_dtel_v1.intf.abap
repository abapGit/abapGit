INTERFACE zif_abapgit_aff_dtel_v1 PUBLIC.

  TYPES:
    BEGIN OF ty_predefined_type,
      data_type TYPE zif_abapgit_aff_ddic_types_v1=>ty_data_type,
      length    TYPE zif_abapgit_aff_ddic_types_v1=>ty_length,
      decimals  TYPE zif_abapgit_aff_ddic_types_v1=>ty_decimals,
    END OF ty_predefined_type.

  TYPES:
    BEGIN OF ty_field_labels,
      short          TYPE c LENGTH 10,
      short_length   TYPE i,
      medium         TYPE c LENGTH 20,
      medium_length  TYPE i,
      long           TYPE c LENGTH 40,
      long_length    TYPE i,
      heading        TYPE c LENGTH 55,
      heading_length TYPE i,
    END OF ty_field_labels.

  TYPES ty_category TYPE c LENGTH 30.


  CONSTANTS:
    BEGIN OF co_category,
      domain                       TYPE ty_category VALUE 'domain',
      predefined_type              TYPE ty_category VALUE 'predefinedAbapType',
      reference_to_predefined_type TYPE ty_category VALUE 'refToPredefinedAbapType',
      reference_dictionary_type    TYPE ty_category VALUE 'refToDictionaryType',
      reference_clas_int_type      TYPE ty_category VALUE 'refToClifType',
    END OF co_category.

  TYPES:
    BEGIN OF ty_data_type_information,
      category        TYPE ty_category,
      type_name       TYPE zif_abapgit_aff_types_v1=>ty_object_name_30,
      predefined_type TYPE ty_predefined_type,
    END OF ty_data_type_information.

  TYPES:
    BEGIN OF ty_search_help,
      name      TYPE zif_abapgit_aff_types_v1=>ty_object_name_30,
      parameter TYPE c LENGTH 30,
    END OF ty_search_help.

  TYPES ty_basic_direction TYPE c LENGTH 1.

  CONSTANTS:
    BEGIN OF co_bidi_basic_direction,
      left_to_right TYPE c LENGTH 1 VALUE ' ',
      right_to_left TYPE c LENGTH 1 VALUE 'X',
    END OF co_bidi_basic_direction.

  TYPES:
    BEGIN OF ty_bidirectional_options,
      basic_direction TYPE ty_basic_direction,
      no_filtering    TYPE abap_bool,
    END OF ty_bidirectional_options.

  TYPES:
    BEGIN OF ty_additional_properties,
      search_help              TYPE ty_search_help,
      bidirectional_options    TYPE ty_bidirectional_options,
      parameter_id             TYPE c LENGTH 20,
      default_component_name   TYPE c LENGTH 30,
      change_document_relevant TYPE abap_bool,
      no_input_history         TYPE abap_bool,
    END OF ty_additional_properties.

  TYPES:
    BEGIN OF ty_main,
      format_version        TYPE zif_abapgit_aff_types_v1=>ty_format_version,
      header                TYPE zif_abapgit_aff_types_v1=>ty_header_60,
      data_type_information TYPE ty_data_type_information,
      field_labels          TYPE ty_field_labels,
      additional_properties TYPE ty_additional_properties,
    END OF ty_main.

ENDINTERFACE.
