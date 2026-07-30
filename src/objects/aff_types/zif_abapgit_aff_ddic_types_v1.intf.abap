INTERFACE zif_abapgit_aff_ddic_types_v1 PUBLIC.

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

  TYPES ty_length TYPE i.

  TYPES ty_decimals TYPE i.

ENDINTERFACE.
