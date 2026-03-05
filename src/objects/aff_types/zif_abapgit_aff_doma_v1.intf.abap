INTERFACE zif_abapgit_aff_doma_v1 PUBLIC.

  TYPES ty_data_type TYPE c LENGTH 4.

  CONSTANTS:
    BEGIN OF co_data_type,
      accp       TYPE ty_data_type VALUE 'ACCP',
      char       TYPE ty_data_type VALUE 'CHAR',
      clnt       TYPE ty_data_type VALUE 'CLNT',
      cuky       TYPE ty_data_type VALUE 'CUKY',
      curr       TYPE ty_data_type VALUE 'CURR',
      df16_dec   TYPE ty_data_type VALUE 'D16D',
      df16_raw   TYPE ty_data_type VALUE 'D16R',
      df16_scl   TYPE ty_data_type VALUE 'D16S',
      decfloat16 TYPE ty_data_type VALUE 'D16N',
      df34_dec   TYPE ty_data_type VALUE 'D34D',
      df34_raw   TYPE ty_data_type VALUE 'D34R',
      df34_scl   TYPE ty_data_type VALUE 'D34S',
      decfloat34 TYPE ty_data_type VALUE 'D34N',
      dats       TYPE ty_data_type VALUE 'DATS',
      datn       TYPE ty_data_type VALUE 'DATN',
      dec        TYPE ty_data_type VALUE 'DEC',
      fltp       TYPE ty_data_type VALUE 'FLTP',
      geom_ewkb  TYPE ty_data_type VALUE 'GGM1',
      int1       TYPE ty_data_type VALUE 'INT1',
      int2       TYPE ty_data_type VALUE 'INT2',
      int4       TYPE ty_data_type VALUE 'INT4',
      int8       TYPE ty_data_type VALUE 'INT8',
      lang       TYPE ty_data_type VALUE 'LANG',
      lchr       TYPE ty_data_type VALUE 'LCHR',
      lraw       TYPE ty_data_type VALUE 'LRAW',
      numc       TYPE ty_data_type VALUE 'NUMC',
      prec       TYPE ty_data_type VALUE 'PREC',
      quan       TYPE ty_data_type VALUE 'QUAN',
      raw        TYPE ty_data_type VALUE 'RAW',
      rawstring  TYPE ty_data_type VALUE 'RSTR',
      sstring    TYPE ty_data_type VALUE 'SSTR',
      string     TYPE ty_data_type VALUE 'STRG',
      tims       TYPE ty_data_type VALUE 'TIMS',
      timn       TYPE ty_data_type VALUE 'TIMN',
      unit       TYPE ty_data_type VALUE 'UNIT',
      utclong    TYPE ty_data_type VALUE 'UTCL',
      varc       TYPE ty_data_type VALUE 'VARC',
    END OF co_data_type.

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

  TYPES ty_length               TYPE i.

  TYPES ty_decimals             TYPE i.

  TYPES ty_output_length        TYPE i.

  TYPES ty_conv_routine         TYPE c LENGTH 5.

  TYPES:
    BEGIN OF ty_format,
      data_type TYPE ty_data_type,

      length    TYPE ty_length,

      decimals  TYPE ty_decimals,

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
