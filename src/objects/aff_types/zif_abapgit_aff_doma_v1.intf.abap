INTERFACE zif_abapgit_aff_doma_v1 PUBLIC.

  TYPES ty_data_type TYPE c LENGTH 12.

  CONSTANTS:
    BEGIN OF co_data_type,
      accp         TYPE ty_data_type VALUE 'ACCP',
      char         TYPE ty_data_type VALUE 'CHAR',
      clnt         TYPE ty_data_type VALUE 'CLNT',
      cuky         TYPE ty_data_type VALUE 'CUKY',
      curr         TYPE ty_data_type VALUE 'CURR',
      df16_dec     TYPE ty_data_type VALUE 'DF16_DEC',
      df16_raw     TYPE ty_data_type VALUE 'DF16_RAW',
      df16_scl     TYPE ty_data_type VALUE 'DF16_SCL',
      decfloat16   TYPE ty_data_type VALUE 'DECFLOAT16',
      df34_dec     TYPE ty_data_type VALUE 'DF34_DEC',
      df34_raw     TYPE ty_data_type VALUE 'DF34_RAW',
      df34_scl     TYPE ty_data_type VALUE 'DF34_SCL',
      decfloat34   TYPE ty_data_type VALUE 'DECFLOAT34',
      dats         TYPE ty_data_type VALUE 'DATS',
      datn         TYPE ty_data_type VALUE 'DATN',
      dec          TYPE ty_data_type VALUE 'DEC',
      fltp         TYPE ty_data_type VALUE 'FLTP',
      geom_ewkb    TYPE ty_data_type VALUE 'GEOM_EWKB',
      int1         TYPE ty_data_type VALUE 'INT1',
      int2         TYPE ty_data_type VALUE 'INT2',
      int4         TYPE ty_data_type VALUE 'INT4',
      int8         TYPE ty_data_type VALUE 'INT8',
      lang         TYPE ty_data_type VALUE 'LANG',
      lchr         TYPE ty_data_type VALUE 'LCHR',
      lraw         TYPE ty_data_type VALUE 'LRAW',
      numc         TYPE ty_data_type VALUE 'NUMC',
      prec         TYPE ty_data_type VALUE 'PREC',
      quan         TYPE ty_data_type VALUE 'QUAN',
      raw          TYPE ty_data_type VALUE 'RAW',
      rawstring    TYPE ty_data_type VALUE 'RAWSTRING',
      sstring      TYPE ty_data_type VALUE 'SSTRING',
      string       TYPE ty_data_type VALUE 'STRING',
      tims         TYPE ty_data_type VALUE 'TIMS',
      timn         TYPE ty_data_type VALUE 'TIMN',
      unit         TYPE ty_data_type VALUE 'UNIT',
      utclong      TYPE ty_data_type VALUE 'UTCLONG',
      varc         TYPE ty_data_type VALUE 'VARC',
    END OF co_data_type.

  TYPES ty_output_style TYPE string.

  CONSTANTS:
    BEGIN OF co_output_style,
      normal                           TYPE ty_output_style VALUE 'normal',
      sign_right                       TYPE ty_output_style VALUE 'signRight',
      scale_preserving                 TYPE ty_output_style VALUE 'scalePreserving',
      scientific                       TYPE ty_output_style VALUE 'scientific',
      scientific_with_leading_zero     TYPE ty_output_style VALUE 'scientificWithLeadingZero',
      scale_preserving_scientific      TYPE ty_output_style VALUE 'scalePreservingScientific',
      engineering                      TYPE ty_output_style VALUE 'engineering',
    END OF co_output_style.

  TYPES:
    BEGIN OF ty_format,
      data_type TYPE ty_data_type,
      length    TYPE i,
      decimals  TYPE i,
    END OF ty_format.

  TYPES:
    BEGIN OF ty_output_characteristics,
      style              TYPE ty_output_style,
      length             TYPE i,
      conversion_routine TYPE c LENGTH 5,
      case_sensitive     TYPE abap_bool,
      negative_values    TYPE abap_bool,
      am_pm_time_format  TYPE abap_bool,
    END OF ty_output_characteristics.

  TYPES:
    BEGIN OF ty_single_value,
      fixed_value TYPE string,
      description TYPE string,
    END OF ty_single_value.

  TYPES:
    BEGIN OF ty_interval_value,
      low_limit   TYPE string,
      high_limit  TYPE string,
      description TYPE string,
    END OF ty_interval_value.

  TYPES:
    BEGIN OF ty_value_table,
      name TYPE c LENGTH 30,
    END OF ty_value_table.

  TYPES:
    BEGIN OF ty_fixed_value_append,
      name TYPE c LENGTH 30,
    END OF ty_fixed_value_append.

  TYPES ty_single_values TYPE STANDARD TABLE OF ty_single_value WITH DEFAULT KEY.
  TYPES ty_interval_values TYPE STANDARD TABLE OF ty_interval_value WITH DEFAULT KEY.
  TYPES ty_fixed_value_appends TYPE STANDARD TABLE OF ty_fixed_value_append WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_main,
      format_version         TYPE zif_abapgit_aff_types_v1=>ty_format_version,
      header                 TYPE zif_abapgit_aff_types_v1=>ty_header_60,
      format                 TYPE ty_format,
      output_characteristics TYPE ty_output_characteristics,
      fixed_values           TYPE ty_single_values,
      fixed_value_intervals  TYPE ty_interval_values,
      value_table            TYPE ty_value_table,
      fixed_value_appends    TYPE ty_fixed_value_appends,
    END OF ty_main.

ENDINTERFACE.
