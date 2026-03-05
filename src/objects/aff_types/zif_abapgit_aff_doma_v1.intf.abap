INTERFACE zif_abapgit_aff_doma_v1 PUBLIC.

  "! $values {@link zif_aff_doma_v1.data:co_data_type}
  "! $default {@link zif_aff_doma_v1.data:co_data_type.char}
  TYPES ty_data_type TYPE c LENGTH 4.

  CONSTANTS:
    BEGIN OF co_data_type,
      "! <p class="shorttext">Posting period YYYYMM</p>
      "! Posting period YYYYMM
      "! $enumValue 'ACCP'
      accp       TYPE ty_data_type VALUE 'ACCP',
      "! <p class="shorttext">Character String</p>
      "! Character string
      "! $enumValue 'CHAR'
      char       TYPE ty_data_type VALUE 'CHAR',
      "! <p class="shorttext">Client</p>
      "! Client
      "! $enumValue 'CLNT'
      clnt       TYPE ty_data_type VALUE 'CLNT',
      "! <p class="shorttext">Currency Key</p>
      "! Currency key, referenced by CURR fields
      "! $enumValue 'CUKY'
      cuky       TYPE ty_data_type VALUE 'CUKY',
      "! <p class="shorttext">Currency Field</p>
      "! Currency field, stored as DEC
      "! $enumValue 'CURR'
      curr       TYPE ty_data_type VALUE 'CURR',
      "! <p class="shorttext">Decimal Floating Point, 16 Digits, DEC on Database</p>
      "! Decimal floating point, 16 digits, DEC on database
      "! $enumValue 'DF16_DEC'
      df16_dec   TYPE ty_data_type VALUE 'D16D',
      "! <p class="shorttext">Decimal Floating Point, 16 Digits, RAW on Database</p>
      "! Decimal floating point, 16 digits, RAW on database
      "! $enumValue 'DF16_RAW'
      df16_raw   TYPE ty_data_type VALUE 'D16R',
      "! <p class="shorttext">Decimal Floating Point. 16 Digits, with Scale Field</p>
      "! Decimal floating point. 16 digits, with scale field
      "! $enumValue 'DF16_SCL'
      df16_scl   TYPE ty_data_type VALUE 'D16S',
      "! <p class="shorttext">Floating Point Number, HANA Type, Small Decimal</p>
      "! Floating point number, HANA type, small decimal
      "! $enumValue 'DECFLOAT16'
      decfloat16 TYPE ty_data_type VALUE 'D16N',
      "! <p class="shorttext">Decimal Floating Point, 34 Digits, DEC on Database</p>
      "! Decimal floating point, 34 Digits, DEC on database
      "! $enumValue 'DF34_DEC'
      df34_dec   TYPE ty_data_type VALUE 'D34D',
      "! <p class="shorttext">Decimal Floating Point, 34 Digits, RAW on Database</p>
      "! Decimal floating point, 34 Digits, RAW on database
      "! $enumValue 'DF34_RAW'
      df34_raw   TYPE ty_data_type VALUE 'D34R',
      "! <p class="shorttext">Decimal Floating Point, 34 Digits, with Scale Field</p>
      "! Decimal floating point, 34 digits, with scale field
      "! $enumValue 'DF34_SCL'
      df34_scl   TYPE ty_data_type VALUE 'D34S',
      "! <p class="shorttext">Floating Point Number, HANA Type, Large Decimal</p>
      "! Floating point number, HANA type, large decimal
      "! $enumValue 'DECFLOAT34'
      decfloat34 TYPE ty_data_type VALUE 'D34N',
      "! <p class="shorttext">Date (YYYYMMDD)</p>
      "! Date field (YYYYMMDD), stored as char(8)
      "! $enumValue 'DATS'
      dats       TYPE ty_data_type VALUE 'DATS',
      "! <p class="shorttext">Date (YYYYMMDD), HANA Type</p>
      "! Date field (YYYYMMDD), HANA type
      "! $enumValue 'DATN'
      datn       TYPE ty_data_type VALUE 'DATN',
      "! <p class="shorttext">Counter or Amount</p>
      "! Counter or amount field with comma and sign
      "! $enumValue 'DEC'
      dec        TYPE ty_data_type VALUE 'DEC',
      "! <p class="shorttext">Floating Point Number</p>
      "! Floating point number, accurate to 8 bytes
      "! $enumValue 'FLTP'
      fltp       TYPE ty_data_type VALUE 'FLTP',
      "! <p class="shorttext">Geometry</p>
      "! Geometry, EWKB representation
      "! $enumValue 'GEOM_EWKB'
      geom_ewkb  TYPE ty_data_type VALUE 'GGM1',
      "! <p class="shorttext">1-Bypte Integer</p>
      "! 1-byte integer, integer number <= 255
      "! $enumValue 'INT1'
      int1       TYPE ty_data_type VALUE 'INT1',
      "! <p class="shorttext">2-Byte Integer</p>
      "! 2-byte integer, only for length field before LCHR or LRAW
      "! $enumValue 'INT2'
      int2       TYPE ty_data_type VALUE 'INT2',
      "! <p class="shorttext">4-Byte Integer</p>
      "! 4-byte integer, integer number with sign
      "! $enumValue 'INT4'
      int4       TYPE ty_data_type VALUE 'INT4',
      "! <p class="shorttext">8-Byte Integer</p>
      "! 8-byte integer, integer number with sign
      "! $enumValue 'INT8'
      int8       TYPE ty_data_type VALUE 'INT8',
      "! <p class="shorttext">Language Key</p>
      "! Language key
      "! $enumValue 'LANG'
      lang       TYPE ty_data_type VALUE 'LANG',
      "! <p class="shorttext">Long Character String</p>
      "! Long character string, requires preceding INT2 field
      "! $enumValue 'LCHR'
      lchr       TYPE ty_data_type VALUE 'LCHR',
      "! <p class="shorttext">Long Byte String</p>
      "! Long byte string, requires preceding INT2 field
      "! $enumValue 'LRAW'
      lraw       TYPE ty_data_type VALUE 'LRAW',
      "! <p class="shorttext">Numeric Character String</p>
      "! Character string with only digits
      "! $enumValue 'NUMC'
      numc       TYPE ty_data_type VALUE 'NUMC',
      "! <p class="shorttext">Obsolete Data Type PREC (Do Not Use)</p>
      "! Obsolete data type PREC, do not use
      "! $enumValue 'PREC'
      prec       TYPE ty_data_type VALUE 'PREC',
      "! <p class="shorttext">Quantity</p>
      "! Quantity field, points to a unit field with format UNIT
      "! $enumValue 'QUAN'
      quan       TYPE ty_data_type VALUE 'QUAN',
      "! <p class="shorttext">Uninterpreted sequence of bytes</p>
      "! Uninterpreted sequence of bytes
      "! $enumValue 'RAW'
      raw        TYPE ty_data_type VALUE 'RAW',
      "! <p class="shorttext">Byte String of Variable Length</p>
      "! Byte String of Variable Length
      "! $enumValue 'RAWSTRING'
      rawstring  TYPE ty_data_type VALUE 'RSTR',
      "! <p class="shorttext">Short Character String of Variable Length</p>
      "! Short Character String of Variable Length
      "! $enumValue 'SSTRING'
      sstring    TYPE ty_data_type VALUE 'SSTR',
      "! <p class="shorttext">Character String of Variable Length</p>
      "! Character String of Variable Length
      "! $enumValue 'STRING'
      string     TYPE ty_data_type VALUE 'STRG',
      "! <p class="shorttext">Time (hhmmss)</p>
      "! Time field (hhmmss), stored as char(6)
      "! $enumValue 'TIMS'
      tims       TYPE ty_data_type VALUE 'TIMS',
      "! <p class="shorttext">Time (hhmmss; Native HANA Type)</p>
      "! Time field (hhmmss), native HANA type
      "! $enumValue 'TIMN'
      timn       TYPE ty_data_type VALUE 'TIMN',
      "! <p class="shorttext">Unit</p>
      "! Unit key for QUAN fields
      "! $enumValue 'UNIT'
      unit       TYPE ty_data_type VALUE 'UNIT',
      "! <p class="shorttext">Timestamp, HANA Type</p>
      "! Timestamp, HANA type
      "! $enumValue 'UTCLONG'
      utclong    TYPE ty_data_type VALUE 'UTCL',
      "! <p class="shorttext">Long character string, no longer supported from Rel. 3.0</p>
      "! Long character string, no longer supported from Rel. 3.0
      "! $enumValue 'VARC'
      varc       TYPE ty_data_type VALUE 'VARC',
    END OF co_data_type.

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

  "! <p class="shorttext">No Characters</p>
  "! Number of characters
  "! $minimum 0
  "! $maximum 999999
  TYPES ty_length               TYPE i.

  "! <p class="shorttext">Decimals</p>
  "! Decimals
  "! $minimum 0
  "! $maximum 999999
  TYPES ty_decimals             TYPE i.

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
      data_type TYPE ty_data_type,

      "! <p class="shorttext">Length</p>
      "! Length
      "! $required
      length    TYPE ty_length,

      "! Set if decimal
      decimals  TYPE ty_decimals,

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
      name TYPE zif_aff_types_v1=>ty_object_name_30,
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
      description TYPE zif_aff_types_v1=>ty_description_60,
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
      description TYPE zif_aff_types_v1=>ty_description_60,
    END OF ty_intervals_value,

    ty_intervals_values TYPE STANDARD TABLE OF ty_intervals_value WITH DEFAULT KEY.

  TYPES:
    "! <p class="shorttext">Fixed Value Appends</p>
    "! Fixed value appends
    BEGIN OF ty_fixed_value_append,
      "! <p class="shorttext">Name</p>
      "! Name
      name TYPE zif_aff_types_v1=>ty_object_name_30,
    END OF ty_fixed_value_append,

    ty_fixed_value_appends TYPE STANDARD TABLE OF ty_fixed_value_append WITH DEFAULT KEY.

  TYPES:
    "! <p class="shorttext">Domain Properties</p>
    "! Domain properties
    BEGIN OF ty_main,
      "! $required
      format_version         TYPE zif_aff_types_v1=>ty_format_version,
      "! <p class="shorttext">Header</p>
      "! Header
      "! $required
      header                 TYPE zif_aff_types_v1=>ty_header_60,

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
