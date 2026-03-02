CLASS lcl_doma_data DEFINITION.
  PUBLIC SECTION.
    DATA dd01v TYPE dd01v.
    DATA dd07v TYPE dd07v_tab.
ENDCLASS.

CLASS lcl_doma_data IMPLEMENTATION.
ENDCLASS.


CLASS lcl_aff_type_mapping DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_aff_type_mapping.
  PRIVATE SECTION.
    METHODS:
      map_data_type_to_aff
        IMPORTING
          iv_ddic_type       TYPE dd01v-datatype
        RETURNING
          VALUE(rv_aff_type) TYPE string,
      map_data_type_to_ddic
        IMPORTING
          iv_aff_type         TYPE string
        RETURNING
          VALUE(rv_ddic_type) TYPE dd01v-datatype.
ENDCLASS.

CLASS lcl_aff_type_mapping IMPLEMENTATION.

  METHOD zif_abapgit_aff_type_mapping~to_aff.
    DATA lo_doma_data TYPE REF TO lcl_doma_data.
    DATA ls_data_aff TYPE zif_abapgit_aff_doma_v1=>ty_main.

    " Convert input data to DOMA structure
    TRY.
        lo_doma_data ?= iv_data.
      CATCH cx_sy_move_cast_error.
        " Handle error
        RETURN.
    ENDTRY.

    " Set format version
    ls_data_aff-format_version = '1'.

    " Map header
    ls_data_aff-header-description = lo_doma_data->dd01v-ddtext.
    ls_data_aff-header-original_language = lo_doma_data->dd01v-ddlanguage.
    ls_data_aff-header-abap_language_version = zif_abapgit_aff_types_v1=>co_abap_language_version-standard.

    " Map format
    ls_data_aff-format-data_type = map_data_type_to_aff( lo_doma_data->dd01v-datatype ).
    ls_data_aff-format-length = lo_doma_data->dd01v-leng.
    IF lo_doma_data->dd01v-decimals IS NOT INITIAL.
      ls_data_aff-format-decimals = lo_doma_data->dd01v-decimals.
    ENDIF.

    " Map output characteristics
    IF lo_doma_data->dd01v-outputlen IS NOT INITIAL
        OR lo_doma_data->dd01v-convexit IS NOT INITIAL
        OR lo_doma_data->dd01v-lowercase IS NOT INITIAL
        OR lo_doma_data->dd01v-signflag IS NOT INITIAL.
      IF lo_doma_data->dd01v-outputlen IS NOT INITIAL.
        ls_data_aff-output_characteristics-length = lo_doma_data->dd01v-outputlen.
      ENDIF.
      IF lo_doma_data->dd01v-convexit IS NOT INITIAL.
        ls_data_aff-output_characteristics-conversion_routine = lo_doma_data->dd01v-convexit.
      ENDIF.
      IF lo_doma_data->dd01v-lowercase IS NOT INITIAL.
        ls_data_aff-output_characteristics-case_sensitive = abap_true.
      ENDIF.
      IF lo_doma_data->dd01v-signflag IS NOT INITIAL.
        ls_data_aff-output_characteristics-negative_values = abap_true.
      ENDIF.
    ENDIF.

    " Map fixed values and intervals
    FIELD-SYMBOLS <ls_dd07v> TYPE dd07v.
    DATA ls_single_value TYPE zif_abapgit_aff_doma_v1=>ty_single_value.
    DATA ls_interval_value TYPE zif_abapgit_aff_doma_v1=>ty_interval_value.

    LOOP AT lo_doma_data->dd07v ASSIGNING <ls_dd07v>.
      IF <ls_dd07v>-domvalue_l = <ls_dd07v>-domvalue_h.
        " Single value
        ls_single_value-fixed_value = <ls_dd07v>-domvalue_l.
        ls_single_value-description = <ls_dd07v>-ddtext.
        APPEND ls_single_value TO ls_data_aff-fixed_values.
      ELSE.
        " Interval
        ls_interval_value-low_limit = <ls_dd07v>-domvalue_l.
        ls_interval_value-high_limit = <ls_dd07v>-domvalue_h.
        ls_interval_value-description = <ls_dd07v>-ddtext.
        APPEND ls_interval_value TO ls_data_aff-fixed_value_intervals.
      ENDIF.
    ENDLOOP.

    " Map value table
    IF lo_doma_data->dd01v-entitytab IS NOT INITIAL.
      ls_data_aff-value_table-name = lo_doma_data->dd01v-entitytab.
    ENDIF.

    es_data = ls_data_aff.
  ENDMETHOD.

  METHOD zif_abapgit_aff_type_mapping~to_abapgit.
    DATA ls_data_aff TYPE zif_abapgit_aff_doma_v1=>ty_main.
    DATA lo_doma_data TYPE REF TO lcl_doma_data.
    DATA ls_dd07v TYPE dd07v.
    DATA lv_valpos TYPE i.

    ls_data_aff = iv_data.

    CREATE OBJECT lo_doma_data.

    " Map header
    lo_doma_data->dd01v-domname = to_upper( iv_object_name ).
    lo_doma_data->dd01v-ddtext = ls_data_aff-header-description.
    lo_doma_data->dd01v-ddlanguage = ls_data_aff-header-original_language.

    " Map format
    lo_doma_data->dd01v-datatype = map_data_type_to_ddic( ls_data_aff-format-data_type ).
    lo_doma_data->dd01v-leng = ls_data_aff-format-length.
    lo_doma_data->dd01v-decimals = ls_data_aff-format-decimals.

    " Map output characteristics
    IF ls_data_aff-output_characteristics-length IS NOT INITIAL.
      lo_doma_data->dd01v-outputlen = ls_data_aff-output_characteristics-length.
    ENDIF.
    IF ls_data_aff-output_characteristics-conversion_routine IS NOT INITIAL.
      lo_doma_data->dd01v-convexit = ls_data_aff-output_characteristics-conversion_routine.
    ENDIF.
    IF ls_data_aff-output_characteristics-case_sensitive = abap_true.
      lo_doma_data->dd01v-lowercase = abap_true.
    ENDIF.
    IF ls_data_aff-output_characteristics-negative_values = abap_true.
      lo_doma_data->dd01v-signflag = abap_true.
    ENDIF.

    " Map fixed values
    lv_valpos = 1.
    FIELD-SYMBOLS <ls_single_value> TYPE zif_abapgit_aff_doma_v1=>ty_single_value.
    LOOP AT ls_data_aff-fixed_values ASSIGNING <ls_single_value>.
      CLEAR ls_dd07v.
      ls_dd07v-domname = lo_doma_data->dd01v-domname.
      ls_dd07v-valpos = lv_valpos.
      ls_dd07v-domvalue_l = <ls_single_value>-fixed_value.
      ls_dd07v-domvalue_h = <ls_single_value>-fixed_value.
      ls_dd07v-ddtext = <ls_single_value>-description.
      ls_dd07v-ddlanguage = lo_doma_data->dd01v-ddlanguage.
      APPEND ls_dd07v TO lo_doma_data->dd07v.
      lv_valpos = lv_valpos + 1.
    ENDLOOP.

    " Map fixed value intervals
    FIELD-SYMBOLS <ls_interval_value> TYPE zif_abapgit_aff_doma_v1=>ty_interval_value.
    LOOP AT ls_data_aff-fixed_value_intervals ASSIGNING <ls_interval_value>.
      CLEAR ls_dd07v.
      ls_dd07v-domname = lo_doma_data->dd01v-domname.
      ls_dd07v-valpos = lv_valpos.
      ls_dd07v-domvalue_l = <ls_interval_value>-low_limit.
      ls_dd07v-domvalue_h = <ls_interval_value>-high_limit.
      ls_dd07v-ddtext = <ls_interval_value>-description.
      ls_dd07v-ddlanguage = lo_doma_data->dd01v-ddlanguage.
      APPEND ls_dd07v TO lo_doma_data->dd07v.
      lv_valpos = lv_valpos + 1.
    ENDLOOP.

    " Map value table
    IF ls_data_aff-value_table-name IS NOT INITIAL.
      lo_doma_data->dd01v-entitytab = ls_data_aff-value_table-name.
    ENDIF.

    es_data = lo_doma_data.
  ENDMETHOD.

  METHOD map_data_type_to_aff.
    CASE iv_ddic_type.
      WHEN 'ACCP'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-accp.
      WHEN 'CHAR'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-char.
      WHEN 'CLNT'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-clnt.
      WHEN 'CUKY'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-cuky.
      WHEN 'CURR'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-curr.
      WHEN 'DF16_DEC'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-df16_dec.
      WHEN 'DF16_RAW'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-df16_raw.
      WHEN 'DF16_SCL'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-df16_scl.
      WHEN 'DECFLOAT16'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-decfloat16.
      WHEN 'DF34_DEC'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-df34_dec.
      WHEN 'DF34_RAW'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-df34_raw.
      WHEN 'DF34_SCL'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-df34_scl.
      WHEN 'DECFLOAT34'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-decfloat34.
      WHEN 'DATS'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-dats.
      WHEN 'DATN'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-datn.
      WHEN 'DEC'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-dec.
      WHEN 'FLTP'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-fltp.
      WHEN 'GEOM_EWKB'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-geom_ewkb.
      WHEN 'INT1'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-int1.
      WHEN 'INT2'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-int2.
      WHEN 'INT4'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-int4.
      WHEN 'INT8'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-int8.
      WHEN 'LANG'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-lang.
      WHEN 'LCHR'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-lchr.
      WHEN 'LRAW'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-lraw.
      WHEN 'NUMC'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-numc.
      WHEN 'PREC'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-prec.
      WHEN 'QUAN'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-quan.
      WHEN 'RAW'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-raw.
      WHEN 'RAWSTRING'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-rawstring.
      WHEN 'SSTRING'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-sstring.
      WHEN 'STRING' OR 'STRG'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-string.
      WHEN 'TIMS'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-tims.
      WHEN 'TIMN'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-timn.
      WHEN 'UNIT'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-unit.
      WHEN 'UTCLONG'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-utclong.
      WHEN 'VARC'.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-varc.
      WHEN OTHERS.
        rv_aff_type = zif_abapgit_aff_doma_v1=>co_data_type-char.
    ENDCASE.
  ENDMETHOD.

  METHOD map_data_type_to_ddic.
    CASE to_upper( iv_aff_type ).
      WHEN 'ACCP'.
        rv_ddic_type = 'ACCP'.
      WHEN 'CHAR'.
        rv_ddic_type = 'CHAR'.
      WHEN 'CLNT'.
        rv_ddic_type = 'CLNT'.
      WHEN 'CUKY'.
        rv_ddic_type = 'CUKY'.
      WHEN 'CURR'.
        rv_ddic_type = 'CURR'.
      WHEN 'DF16_DEC'.
        rv_ddic_type = 'DF16_DEC'.
      WHEN 'DF16_RAW'.
        rv_ddic_type = 'DF16_RAW'.
      WHEN 'DF16_SCL'.
        rv_ddic_type = 'DF16_SCL'.
      WHEN 'DECFLOAT16'.
        rv_ddic_type = 'DECFLOAT16'.
      WHEN 'DF34_DEC'.
        rv_ddic_type = 'DF34_DEC'.
      WHEN 'DF34_RAW'.
        rv_ddic_type = 'DF34_RAW'.
      WHEN 'DF34_SCL'.
        rv_ddic_type = 'DF34_SCL'.
      WHEN 'DECFLOAT34'.
        rv_ddic_type = 'DECFLOAT34'.
      WHEN 'DATS'.
        rv_ddic_type = 'DATS'.
      WHEN 'DATN'.
        rv_ddic_type = 'DATN'.
      WHEN 'DEC'.
        rv_ddic_type = 'DEC'.
      WHEN 'FLTP'.
        rv_ddic_type = 'FLTP'.
      WHEN 'GEOM_EWKB'.
        rv_ddic_type = 'GEOM_EWKB'.
      WHEN 'INT1'.
        rv_ddic_type = 'INT1'.
      WHEN 'INT2'.
        rv_ddic_type = 'INT2'.
      WHEN 'INT4'.
        rv_ddic_type = 'INT4'.
      WHEN 'INT8'.
        rv_ddic_type = 'INT8'.
      WHEN 'LANG'.
        rv_ddic_type = 'LANG'.
      WHEN 'LCHR'.
        rv_ddic_type = 'LCHR'.
      WHEN 'LRAW'.
        rv_ddic_type = 'LRAW'.
      WHEN 'NUMC'.
        rv_ddic_type = 'NUMC'.
      WHEN 'PREC'.
        rv_ddic_type = 'PREC'.
      WHEN 'QUAN'.
        rv_ddic_type = 'QUAN'.
      WHEN 'RAW'.
        rv_ddic_type = 'RAW'.
      WHEN 'RAWSTRING'.
        rv_ddic_type = 'RAWSTRING'.
      WHEN 'SSTRING'.
        rv_ddic_type = 'SSTRING'.
      WHEN 'STRING'.
        rv_ddic_type = 'STRG'.
      WHEN 'TIMS'.
        rv_ddic_type = 'TIMS'.
      WHEN 'TIMN'.
        rv_ddic_type = 'TIMN'.
      WHEN 'UNIT'.
        rv_ddic_type = 'UNIT'.
      WHEN 'UTCLONG'.
        rv_ddic_type = 'UTCLONG'.
      WHEN 'VARC'.
        rv_ddic_type = 'VARC'.
      WHEN OTHERS.
        rv_ddic_type = 'CHAR'.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_aff_metadata_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      serialize
        IMPORTING
          is_dd01v       TYPE dd01v
          it_dd07v       TYPE dd07v_tab
        RETURNING
          VALUE(rv_json) TYPE xstring
        RAISING
          zcx_abapgit_exception,
      deserialize
        IMPORTING
          iv_json          TYPE xstring
          iv_object_name   TYPE string
        EXPORTING
          es_dd01v         TYPE dd01v
          et_dd07v         TYPE dd07v_tab
        RAISING
          zcx_abapgit_exception.
  PRIVATE SECTION.
    CLASS-METHODS:
      get_enum_mappings
        RETURNING
          VALUE(rt_result) TYPE zcl_abapgit_json_handler=>ty_enum_mappings.
ENDCLASS.

CLASS lcl_aff_metadata_handler IMPLEMENTATION.

  METHOD serialize.
    DATA lo_aff_handler TYPE REF TO zcl_abapgit_json_handler.
    DATA lo_aff_mapper TYPE REF TO zif_abapgit_aff_type_mapping.
    DATA ls_data_aff TYPE zif_abapgit_aff_doma_v1=>ty_main.
    DATA lo_doma_data TYPE REF TO lcl_doma_data.
    DATA lt_enum_mappings TYPE zcl_abapgit_json_handler=>ty_enum_mappings.

    CREATE OBJECT lo_doma_data.
    lo_doma_data->dd01v = is_dd01v.
    lo_doma_data->dd07v = it_dd07v.

    CREATE OBJECT lo_aff_mapper TYPE lcl_aff_type_mapping.
    lo_aff_mapper->to_aff(
      EXPORTING
        iv_data = lo_doma_data
      IMPORTING
        es_data = ls_data_aff ).

    lt_enum_mappings = get_enum_mappings( ).

    CREATE OBJECT lo_aff_handler.
    rv_json = lo_aff_handler->serialize(
      iv_data          = ls_data_aff
      iv_enum_mappings = lt_enum_mappings ).

  ENDMETHOD.

  METHOD deserialize.
    DATA lo_json_handler TYPE REF TO zcl_abapgit_json_handler.
    DATA lo_aff_mapper TYPE REF TO zif_abapgit_aff_type_mapping.
    DATA ls_data_aff TYPE zif_abapgit_aff_doma_v1=>ty_main.
    DATA lo_doma_data TYPE REF TO lcl_doma_data.
    DATA lt_enum_mappings TYPE zcl_abapgit_json_handler=>ty_enum_mappings.
    DATA lv_json_string TYPE string.

    lt_enum_mappings = get_enum_mappings( ).

    lv_json_string = zcl_abapgit_convert=>xstring_to_string_utf8( iv_json ).

    CREATE OBJECT lo_json_handler.
    lo_json_handler->deserialize(
      EXPORTING
        iv_content       = lv_json_string
        iv_enum_mappings = lt_enum_mappings
      IMPORTING
        ev_data          = ls_data_aff ).

    CREATE OBJECT lo_aff_mapper TYPE lcl_aff_type_mapping.
    lo_aff_mapper->to_abapgit(
      EXPORTING
        iv_data        = ls_data_aff
        iv_object_name = iv_object_name
      IMPORTING
        es_data        = lo_doma_data ).

    es_dd01v = lo_doma_data->dd01v.
    et_dd07v = lo_doma_data->dd07v.

  ENDMETHOD.

  METHOD get_enum_mappings.
    DATA ls_mapping TYPE zcl_abapgit_json_handler=>ty_enum_mapping.
    DATA ls_json_abap_mapping TYPE zcl_abapgit_json_handler=>ty_json_abap_mapping.

    " Map data types - all types map to themselves in uppercase
    ls_mapping-path = '/format/dataType'.
    CLEAR ls_mapping-mappings.

    ls_json_abap_mapping-abap = 'ACCP'.
    ls_json_abap_mapping-json = 'ACCP'.
    APPEND ls_json_abap_mapping TO ls_mapping-mappings.

    ls_json_abap_mapping-abap = 'CHAR'.
    ls_json_abap_mapping-json = 'CHAR'.
    APPEND ls_json_abap_mapping TO ls_mapping-mappings.

    ls_json_abap_mapping-abap = 'CLNT'.
    ls_json_abap_mapping-json = 'CLNT'.
    APPEND ls_json_abap_mapping TO ls_mapping-mappings.

    ls_json_abap_mapping-abap = 'CUKY'.
    ls_json_abap_mapping-json = 'CUKY'.
    APPEND ls_json_abap_mapping TO ls_mapping-mappings.

    ls_json_abap_mapping-abap = 'CURR'.
    ls_json_abap_mapping-json = 'CURR'.
    APPEND ls_json_abap_mapping TO ls_mapping-mappings.

    ls_json_abap_mapping-abap = 'DATS'.
    ls_json_abap_mapping-json = 'DATS'.
    APPEND ls_json_abap_mapping TO ls_mapping-mappings.

    ls_json_abap_mapping-abap = 'DEC'.
    ls_json_abap_mapping-json = 'DEC'.
    APPEND ls_json_abap_mapping TO ls_mapping-mappings.

    ls_json_abap_mapping-abap = 'FLTP'.
    ls_json_abap_mapping-json = 'FLTP'.
    APPEND ls_json_abap_mapping TO ls_mapping-mappings.

    ls_json_abap_mapping-abap = 'INT1'.
    ls_json_abap_mapping-json = 'INT1'.
    APPEND ls_json_abap_mapping TO ls_mapping-mappings.

    ls_json_abap_mapping-abap = 'INT2'.
    ls_json_abap_mapping-json = 'INT2'.
    APPEND ls_json_abap_mapping TO ls_mapping-mappings.

    ls_json_abap_mapping-abap = 'INT4'.
    ls_json_abap_mapping-json = 'INT4'.
    APPEND ls_json_abap_mapping TO ls_mapping-mappings.

    ls_json_abap_mapping-abap = 'INT8'.
    ls_json_abap_mapping-json = 'INT8'.
    APPEND ls_json_abap_mapping TO ls_mapping-mappings.

    ls_json_abap_mapping-abap = 'LANG'.
    ls_json_abap_mapping-json = 'LANG'.
    APPEND ls_json_abap_mapping TO ls_mapping-mappings.

    ls_json_abap_mapping-abap = 'NUMC'.
    ls_json_abap_mapping-json = 'NUMC'.
    APPEND ls_json_abap_mapping TO ls_mapping-mappings.

    ls_json_abap_mapping-abap = 'QUAN'.
    ls_json_abap_mapping-json = 'QUAN'.
    APPEND ls_json_abap_mapping TO ls_mapping-mappings.

    ls_json_abap_mapping-abap = 'RAW'.
    ls_json_abap_mapping-json = 'RAW'.
    APPEND ls_json_abap_mapping TO ls_mapping-mappings.

    ls_json_abap_mapping-abap = 'RAWSTRING'.
    ls_json_abap_mapping-json = 'RAWSTRING'.
    APPEND ls_json_abap_mapping TO ls_mapping-mappings.

    ls_json_abap_mapping-abap = 'STRING'.
    ls_json_abap_mapping-json = 'STRING'.
    APPEND ls_json_abap_mapping TO ls_mapping-mappings.

    ls_json_abap_mapping-abap = 'TIMS'.
    ls_json_abap_mapping-json = 'TIMS'.
    APPEND ls_json_abap_mapping TO ls_mapping-mappings.

    ls_json_abap_mapping-abap = 'UNIT'.
    ls_json_abap_mapping-json = 'UNIT'.
    APPEND ls_json_abap_mapping TO ls_mapping-mappings.

    APPEND ls_mapping TO rt_result.

  ENDMETHOD.

ENDCLASS.
