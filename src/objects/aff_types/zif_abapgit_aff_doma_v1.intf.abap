INTERFACE zif_abapgit_aff_doma_v1 PUBLIC.

  "! $values {@link zif_aff_doma_v1.data:co_output_style}
  "! $default {@link zif_aff_doma_v1.data:co_output_style.normal}
  TYPES ty_output_style TYPE n LENGTH 2.

  CONSTANTS:
    BEGIN OF co_output_style,
               "! <p class="shorttext">Normal</p>
               "! Normal
               normal                       TYPE ty_output_style VALUE '00',
               "! <p class="shorttext">Sign right</p>
               "! Sign right
               sign_right                   TYPE ty_output_style VALUE '01',
               "! <p class="shorttext">Scale-preserving</p>
               "! Scale-preserving
               scale_preserving             TYPE ty_output_style VALUE '02',
               "! <p class="shorttext">Scientific</p>
               "! Scientific
               scientific                   TYPE ty_output_style VALUE '03',
               "! <p class="shorttext">Scientific with leading zero</p>
               "! Scientific with leading zero
               scientific_with_leading_zero TYPE ty_output_style VALUE '04',
               "! <p class="shorttext">Scale-preserving scientific</p>
               "! Scale-preserving scientific
               scale_preserving_scientific  TYPE ty_output_style VALUE '05',
               "! <p class="shorttext">Engineering</p>
               "! Engineering
               engineering                  TYPE ty_output_style VALUE '06',
             END OF co_output_style.

  "! <p class="shorttext">Output Length</p>
  "! Output length
  "! $minimum 0
  "! $maximum 999999
  TYPES ty_output_length        TYPE i.

  "! <p class="shorttext">Conversion Routine</p>
  "! Conversion routine
  TYPES ty_conv_routine         TYPE c LENGTH 5.

  TYPES:
    BEGIN OF ty_format,
      "! <p class="shorttext">Data Type</p>
      "! Data type
      "! $required
      data_type TYPE zif_abapgit_aff_ddic_types_v1=>ty_data_type,

      "! <p class="shorttext">Length</p>
      "! Length
      "! $required
      length    TYPE zif_abapgit_aff_ddic_types_v1=>ty_length,

      "! Set if decimal
      decimals  TYPE zif_abapgit_aff_ddic_types_v1=>ty_decimals,

    END OF ty_format.

  TYPES:
    BEGIN OF ty_output_characteristics,
      "! <p class="shorttext">Style</p>
      "! Output style (for Releases < 71*)
      style              TYPE ty_output_style,

      "! <p class="shorttext">Length</p>
      "! Set the output length
      length             TYPE ty_output_length,

      "! Conversion Routine
      conversion_routine TYPE ty_conv_routine,

      "! <p class="shorttext">Case Sensitive</p>
      "! Values are case sensitive
      case_sensitive     TYPE abap_bool,

      "! <p class="shorttext">Negative Values</p>
      "! Supports negative values
      negative_values    TYPE abap_bool,

      "! <p class="shorttext">AM/PM Time Format</p>
      "! AM/PM time format supported
      am_pm_time_format  TYPE abap_bool,

    END OF ty_output_characteristics.


  TYPES:
    "! <p class="shorttext">Value Table</p>
    "! Value table
    BEGIN OF ty_value_table,
      "! <p class="shorttext">Name</p>
      "! Name
      name TYPE zif_abapgit_aff_types_v1=>ty_object_name_30,
    END OF ty_value_table.

  TYPES:
    "! <p class="shorttext">Single Values</p>
    "! Single values
    BEGIN OF ty_single_value,
      "! <p class="shorttext">Fixed Value</p>
      "! Value
      fixed_value TYPE c LENGTH 10,
      "! <p class="shorttext">Description</p>
      "! Description
      description TYPE zif_abapgit_aff_types_v1=>ty_description_60,
    END OF ty_single_value,

    ty_single_values TYPE STANDARD TABLE OF ty_single_value WITH DEFAULT KEY.

  TYPES:
    "! <p class="shorttext">Interval Values</p>
    "! Interval values
    BEGIN OF ty_intervals_value,
      "! <p class="shorttext">Low Limit of the Interval</p>
      "! Low value for the interval
      low_limit   TYPE c LENGTH 10,
      "! <p class="shorttext">High Limit of the Interval</p>
      "! High value for the interval
      "! $required
      high_limit  TYPE c LENGTH 10,
      "! <p class="shorttext">Description</p>
      "! Description
      description TYPE zif_abapgit_aff_types_v1=>ty_description_60,
    END OF ty_intervals_value,

    ty_intervals_values TYPE STANDARD TABLE OF ty_intervals_value WITH DEFAULT KEY.

  TYPES:
    "! <p class="shorttext">Fixed Value Appends</p>
    "! Fixed value appends
    BEGIN OF ty_fixed_value_append,
      "! <p class="shorttext">Name</p>
      "! Name
      name TYPE zif_abapgit_aff_types_v1=>ty_object_name_30,
    END OF ty_fixed_value_append,

    ty_fixed_value_appends TYPE STANDARD TABLE OF ty_fixed_value_append WITH DEFAULT KEY.

  TYPES:
    "! <p class="shorttext">Domain Properties</p>
    "! Domain properties
    BEGIN OF ty_main,
      "! $required
      format_version         TYPE zif_abapgit_aff_types_v1=>ty_format_version,
      "! <p class="shorttext">Header</p>
      "! Header
      "! $required
      header                 TYPE zif_abapgit_aff_types_v1=>ty_header_60,

      "! <p class="shorttext">Format</p>
      "! Format
      "! $required
      format                 TYPE ty_format,

      "! <p class="shorttext">Output Characteristics</p>
      "! Output characteristics
      output_characteristics TYPE ty_output_characteristics,

      "! <p class="shorttext">Fixed Values</p>
      "! Fixed values
      fixed_values           TYPE ty_single_values,

      "! <p class="shorttext">Fixed Value Intervals</p>
      "! Fixed value intervals
      fixed_value_intervals  TYPE ty_intervals_values,

      "! <p class="shorttext">Value Table</p>
      "! Value table
      value_table            TYPE ty_value_table,

      "! <p class="shorttext">Fixed Value Appends</p>
      "! Fixed value appends
      fixed_value_appends    TYPE ty_fixed_value_appends,
    END OF ty_main.

ENDINTERFACE.
