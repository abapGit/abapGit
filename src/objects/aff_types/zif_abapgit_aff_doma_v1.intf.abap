INTERFACE zif_abapgit_aff_doma_v1 PUBLIC.

  TYPES ty_output_style TYPE n LENGTH 2.

  CONSTANTS:
    BEGIN OF co_output_style,
               normal                       TYPE ty_output_style VALUE '00',
               sign_right                   TYPE ty_output_style VALUE '01',
               scale_preserving             TYPE ty_output_style VALUE '02',
               scientific                   TYPE ty_output_style VALUE '03',
               scientific_with_leading_zero TYPE ty_output_style VALUE '04',
               scale_preserving_scientific  TYPE ty_output_style VALUE '05',
               engineering                  TYPE ty_output_style VALUE '06',
             END OF co_output_style.

  TYPES ty_output_length        TYPE i.

  TYPES ty_conv_routine         TYPE c LENGTH 5.

  TYPES:
    BEGIN OF ty_format,
      data_type TYPE zif_abapgit_aff_ddic_types_v1=>ty_data_type,

      length    TYPE zif_abapgit_aff_ddic_types_v1=>ty_length,

      decimals  TYPE zif_abapgit_aff_ddic_types_v1=>ty_decimals,

    END OF ty_format.

  TYPES:
    BEGIN OF ty_output_characteristics,
      style              TYPE ty_output_style,

      length             TYPE ty_output_length,

      conversion_routine TYPE ty_conv_routine,

      case_sensitive     TYPE abap_bool,

      negative_values    TYPE abap_bool,

      am_pm_time_format  TYPE abap_bool,

    END OF ty_output_characteristics.


  TYPES:
    BEGIN OF ty_value_table,
      name TYPE zif_abapgit_aff_types_v1=>ty_object_name_30,
    END OF ty_value_table.

  TYPES:
    BEGIN OF ty_single_value,
      fixed_value TYPE c LENGTH 10,
      description TYPE zif_abapgit_aff_types_v1=>ty_description_60,
    END OF ty_single_value,

    ty_single_values TYPE STANDARD TABLE OF ty_single_value WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_intervals_value,
      low_limit   TYPE c LENGTH 10,
      high_limit  TYPE c LENGTH 10,
      description TYPE zif_abapgit_aff_types_v1=>ty_description_60,
    END OF ty_intervals_value,

    ty_intervals_values TYPE STANDARD TABLE OF ty_intervals_value WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_fixed_value_append,
      name TYPE zif_abapgit_aff_types_v1=>ty_object_name_30,
    END OF ty_fixed_value_append,

    ty_fixed_value_appends TYPE STANDARD TABLE OF ty_fixed_value_append WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_main,
      format_version         TYPE zif_abapgit_aff_types_v1=>ty_format_version,
      header                 TYPE zif_abapgit_aff_types_v1=>ty_header_60,

      format                 TYPE ty_format,

      output_characteristics TYPE ty_output_characteristics,

      fixed_values           TYPE ty_single_values,

      fixed_value_intervals  TYPE ty_intervals_values,

      value_table            TYPE ty_value_table,

      fixed_value_appends    TYPE ty_fixed_value_appends,
    END OF ty_main.

ENDINTERFACE.
