
CLASS lcl_doma_data DEFINITION.
  PUBLIC SECTION.
    DATA ms_dd01v TYPE dd01v.
    DATA ms_dd07v TYPE dd07v_tab.
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
          iv_length          TYPE dd01v-leng
        RETURNING
          VALUE(rv_aff_type) TYPE zif_abapgit_aff_ddic_types_v1=>ty_data_type,
      map_data_type_to_ddic
        IMPORTING
          iv_aff_type         TYPE zif_abapgit_aff_ddic_types_v1=>ty_data_type
        RETURNING
          VALUE(rv_ddic_type) TYPE dd01v-datatype,
      is_supported_data_type
        IMPORTING
          iv_data_type        TYPE zif_abapgit_aff_ddic_types_v1=>ty_data_type
        RETURNING
          VALUE(rv_supported) TYPE abap_bool.
ENDCLASS.

CLASS lcl_aff_type_mapping IMPLEMENTATION.

  METHOD zif_abapgit_aff_type_mapping~to_aff.
    DATA lo_doma_data TYPE REF TO lcl_doma_data.
    DATA ls_data_aff TYPE zif_abapgit_aff_doma_v1=>ty_main.

    " Map fixed values and intervals
    FIELD-SYMBOLS <ls_dd07v> TYPE dd07v.
    DATA ls_single_value TYPE zif_abapgit_aff_doma_v1=>ty_single_value.
    DATA ls_interval_value TYPE zif_abapgit_aff_doma_v1=>ty_intervals_value.

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
    ls_data_aff-header-description = lo_doma_data->ms_dd01v-ddtext.
    ls_data_aff-header-original_language = lo_doma_data->ms_dd01v-ddlanguage.
    ls_data_aff-header-abap_language_version = zif_abapgit_aff_types_v1=>co_abap_language_version-standard.

    " Map format
    ls_data_aff-format-data_type = map_data_type_to_aff(
      iv_ddic_type = lo_doma_data->ms_dd01v-datatype
      iv_length    = lo_doma_data->ms_dd01v-leng ).
    ls_data_aff-format-length = lo_doma_data->ms_dd01v-leng.
    IF lo_doma_data->ms_dd01v-decimals IS NOT INITIAL.
      ls_data_aff-format-decimals = lo_doma_data->ms_dd01v-decimals.
    ENDIF.

    " Map output characteristics
    IF lo_doma_data->ms_dd01v-outputlen IS NOT INITIAL
        OR lo_doma_data->ms_dd01v-convexit IS NOT INITIAL
        OR lo_doma_data->ms_dd01v-lowercase IS NOT INITIAL
        OR lo_doma_data->ms_dd01v-signflag IS NOT INITIAL.
      IF lo_doma_data->ms_dd01v-outputlen IS NOT INITIAL.
        ls_data_aff-output_characteristics-length = lo_doma_data->ms_dd01v-outputlen.
      ENDIF.
      IF lo_doma_data->ms_dd01v-convexit IS NOT INITIAL.
        ls_data_aff-output_characteristics-conversion_routine = lo_doma_data->ms_dd01v-convexit.
      ENDIF.
      IF lo_doma_data->ms_dd01v-lowercase IS NOT INITIAL.
        ls_data_aff-output_characteristics-case_sensitive = abap_true.
      ENDIF.
      IF lo_doma_data->ms_dd01v-signflag IS NOT INITIAL.
        ls_data_aff-output_characteristics-negative_values = abap_true.
      ENDIF.
    ENDIF.

    LOOP AT lo_doma_data->ms_dd07v ASSIGNING <ls_dd07v>.
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
    IF lo_doma_data->ms_dd01v-entitytab IS NOT INITIAL.
      ls_data_aff-value_table-name = lo_doma_data->ms_dd01v-entitytab.
    ENDIF.

    es_data = ls_data_aff.
  ENDMETHOD.

  METHOD zif_abapgit_aff_type_mapping~to_abapgit.
    DATA ls_data_aff TYPE zif_abapgit_aff_doma_v1=>ty_main.
    DATA lo_doma_data TYPE REF TO lcl_doma_data.
    DATA ls_dd07v TYPE dd07v.
    DATA lv_valpos TYPE i.
    FIELD-SYMBOLS <ls_single_value> TYPE zif_abapgit_aff_doma_v1=>ty_single_value.
    FIELD-SYMBOLS <ls_interval_value> TYPE zif_abapgit_aff_doma_v1=>ty_intervals_value.

    ls_data_aff = iv_data.

    CREATE OBJECT lo_doma_data.

    " Map header
    lo_doma_data->ms_dd01v-domname = to_upper( iv_object_name ).
    lo_doma_data->ms_dd01v-ddtext = ls_data_aff-header-description.
    lo_doma_data->ms_dd01v-ddlanguage = ls_data_aff-header-original_language.

    " Map format
    lo_doma_data->ms_dd01v-datatype = map_data_type_to_ddic( ls_data_aff-format-data_type ).
    lo_doma_data->ms_dd01v-leng = ls_data_aff-format-length.
    lo_doma_data->ms_dd01v-decimals = ls_data_aff-format-decimals.

    " Map output characteristics
    IF ls_data_aff-output_characteristics-length IS NOT INITIAL.
      lo_doma_data->ms_dd01v-outputlen = ls_data_aff-output_characteristics-length.
    ENDIF.
    IF ls_data_aff-output_characteristics-conversion_routine IS NOT INITIAL.
      lo_doma_data->ms_dd01v-convexit = ls_data_aff-output_characteristics-conversion_routine.
    ENDIF.
    IF ls_data_aff-output_characteristics-case_sensitive = abap_true.
      lo_doma_data->ms_dd01v-lowercase = abap_true.
    ENDIF.
    IF ls_data_aff-output_characteristics-negative_values = abap_true.
      lo_doma_data->ms_dd01v-signflag = abap_true.
    ENDIF.

    " Map fixed values
    lv_valpos = 1.

    LOOP AT ls_data_aff-fixed_values ASSIGNING <ls_single_value>.
      CLEAR ls_dd07v.
      ls_dd07v-domname = lo_doma_data->ms_dd01v-domname.
      ls_dd07v-valpos = lv_valpos.
      ls_dd07v-domvalue_l = <ls_single_value>-fixed_value.
      ls_dd07v-domvalue_h = <ls_single_value>-fixed_value.
      ls_dd07v-ddtext = <ls_single_value>-description.
      ls_dd07v-ddlanguage = lo_doma_data->ms_dd01v-ddlanguage.
      APPEND ls_dd07v TO lo_doma_data->ms_dd07v.
      lv_valpos = lv_valpos + 1.
    ENDLOOP.

    " Map fixed value intervals
    LOOP AT ls_data_aff-fixed_value_intervals ASSIGNING <ls_interval_value>.
      CLEAR ls_dd07v.
      ls_dd07v-domname = lo_doma_data->ms_dd01v-domname.
      ls_dd07v-valpos = lv_valpos.
      ls_dd07v-domvalue_l = <ls_interval_value>-low_limit.
      ls_dd07v-domvalue_h = <ls_interval_value>-high_limit.
      ls_dd07v-ddtext = <ls_interval_value>-description.
      ls_dd07v-ddlanguage = lo_doma_data->ms_dd01v-ddlanguage.
      APPEND ls_dd07v TO lo_doma_data->ms_dd07v.
      lv_valpos = lv_valpos + 1.
    ENDLOOP.

    " Map value table
    IF ls_data_aff-value_table-name IS NOT INITIAL.
      lo_doma_data->ms_dd01v-entitytab = ls_data_aff-value_table-name.
    ENDIF.

    es_data = lo_doma_data.
  ENDMETHOD.

  METHOD map_data_type_to_aff.
    CASE iv_ddic_type.
      WHEN 'DF16'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-decfloat16.
      WHEN 'DF34'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-decfloat34.
      WHEN 'DECF'.
        IF iv_length <= 16.
          rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-decfloat16.
        ELSE.
          rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-decfloat34.
        ENDIF.
      WHEN 'GEOM'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-geom_ewkb.
      WHEN 'RAWS'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-rawstring.
      WHEN OTHERS.
        rv_aff_type = iv_ddic_type.
    ENDCASE.

    IF is_supported_data_type( rv_aff_type ) = abap_false.
      rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-char.
    ENDIF.
  ENDMETHOD.

  METHOD map_data_type_to_ddic.
    IF is_supported_data_type( iv_aff_type ) = abap_true.
      rv_ddic_type = iv_aff_type.
    ELSE.
      rv_ddic_type = 'CHAR'.
    ENDIF.
  ENDMETHOD.

  METHOD is_supported_data_type.
    DATA ls_data_types LIKE zif_abapgit_aff_ddic_types_v1=>co_data_type.

    FIELD-SYMBOLS <lv_data_type> TYPE any.

    ls_data_types = zif_abapgit_aff_ddic_types_v1=>co_data_type.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE ls_data_types TO <lv_data_type>.
      IF sy-subrc <> 0.
        RETURN.
      ELSEIF <lv_data_type> = iv_data_type.
        rv_supported = abap_true.
        RETURN.
      ENDIF.
    ENDDO.
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
          iv_json        TYPE xstring
          iv_object_name TYPE sobj_name
        EXPORTING
          es_dd01v       TYPE dd01v
          et_dd07v       TYPE dd07v_tab
        RAISING
          zcx_abapgit_exception.
  PRIVATE SECTION.
    CLASS-METHODS:
      get_enum_mappings
        IMPORTING
          iv_snake_case    TYPE abap_bool DEFAULT abap_false
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
    DATA lx_exception TYPE REF TO cx_root.
    DATA lt_skip_paths TYPE zcl_abapgit_json_handler=>ty_skip_paths.
    DATA ls_skip_path  TYPE zcl_abapgit_json_handler=>ty_path_value_pair.

    CREATE OBJECT lo_doma_data.
    lo_doma_data->ms_dd01v = is_dd01v.
    lo_doma_data->ms_dd07v = it_dd07v.

    CREATE OBJECT lo_aff_mapper TYPE lcl_aff_type_mapping.
    lo_aff_mapper->to_aff(
      EXPORTING
        iv_data = lo_doma_data
      IMPORTING
        es_data = ls_data_aff ).

    lt_enum_mappings = get_enum_mappings( ).

    ls_skip_path-path  = '/format/decimals'.
    ls_skip_path-value = '0'.
    APPEND ls_skip_path TO lt_skip_paths.
    ls_skip_path-path  = '/outputCharacteristics/style'.
    ls_skip_path-value = '00'.
    APPEND ls_skip_path TO lt_skip_paths.

    CREATE OBJECT lo_aff_handler.

    TRY.
        rv_json = lo_aff_handler->serialize(
          iv_data          = ls_data_aff
          iv_enum_mappings = lt_enum_mappings
          iv_skip_paths    = lt_skip_paths ).
      CATCH cx_root INTO lx_exception.
        zcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDTRY.

  ENDMETHOD.

  METHOD deserialize.
    DATA lo_json_handler TYPE REF TO zcl_abapgit_json_handler.
    DATA lo_aff_mapper TYPE REF TO zif_abapgit_aff_type_mapping.
    DATA ls_data_aff TYPE zif_abapgit_aff_doma_v1=>ty_main.
    DATA lo_doma_data TYPE REF TO lcl_doma_data.
    DATA lt_enum_mappings TYPE zcl_abapgit_json_handler=>ty_enum_mappings.
    DATA lv_json_string TYPE string.
    DATA lx_exception TYPE REF TO cx_root.

    lt_enum_mappings = get_enum_mappings( abap_true ).

    lv_json_string = zcl_abapgit_convert=>xstring_to_string_utf8( iv_json ).

    CREATE OBJECT lo_json_handler.

    TRY.
        lo_json_handler->deserialize(
          EXPORTING
            iv_content       = lv_json_string
            iv_enum_mappings = lt_enum_mappings
          IMPORTING
            ev_data          = ls_data_aff ).
      CATCH cx_root INTO lx_exception.
        zcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDTRY.

    CREATE OBJECT lo_aff_mapper TYPE lcl_aff_type_mapping.
    lo_aff_mapper->to_abapgit(
      EXPORTING
        iv_data        = ls_data_aff
        iv_object_name = iv_object_name
      IMPORTING
        es_data        = lo_doma_data ).

    es_dd01v = lo_doma_data->ms_dd01v.
    et_dd07v = lo_doma_data->ms_dd07v.

  ENDMETHOD.

  METHOD get_enum_mappings.
    DATA ls_mapping TYPE zcl_abapgit_json_handler=>ty_enum_mapping.
    DATA ls_json_abap_mapping TYPE zcl_abapgit_json_handler=>ty_json_abap_mapping.
    DATA lt_identity_values TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA lt_enum_pairs TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA lv_identity_value TYPE string.
    DATA lv_enum_pair TYPE string.

    IF iv_snake_case = abap_true.
      ls_mapping-path = '/format/data_type'.
    ELSE.
      ls_mapping-path = '/format/dataType'.
    ENDIF.

    SPLIT `ACCP CHAR CLNT CUKY CURR DATS DATN DEC FLTP INT1 INT2 INT4 INT8 LANG ` &&
      `LCHR LRAW NUMC PREC QUAN RAW TIMS TIMN UNIT VARC`
      AT space INTO TABLE lt_identity_values.
    LOOP AT lt_identity_values INTO lv_identity_value.
      ls_json_abap_mapping-abap = lv_identity_value.
      ls_json_abap_mapping-json = lv_identity_value.
      APPEND ls_json_abap_mapping TO ls_mapping-mappings.
    ENDLOOP.

    SPLIT `D16D=DF16_DEC D16R=DF16_RAW D16S=DF16_SCL D16N=DECFLOAT16 ` &&
      `D34D=DF34_DEC D34R=DF34_RAW D34S=DF34_SCL D34N=DECFLOAT34 ` &&
      `GGM1=GEOM_EWKB RSTR=RAWSTRING SSTR=SSTRING STRG=STRING UTCL=UTCLONG`
      AT space INTO TABLE lt_enum_pairs.
    LOOP AT lt_enum_pairs INTO lv_enum_pair.
      SPLIT lv_enum_pair AT '=' INTO ls_json_abap_mapping-abap ls_json_abap_mapping-json.
      APPEND ls_json_abap_mapping TO ls_mapping-mappings.
    ENDLOOP.

    APPEND ls_mapping TO rt_result.

  ENDMETHOD.

ENDCLASS.
