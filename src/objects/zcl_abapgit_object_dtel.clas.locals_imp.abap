
CLASS lcl_dtel_data DEFINITION.
  PUBLIC SECTION.
    DATA ms_dd04v TYPE dd04v.
    DATA mv_abap_language_version TYPE uccheck.
ENDCLASS.

CLASS lcl_dtel_data IMPLEMENTATION.
ENDCLASS.


CLASS lcl_aff_type_mapping DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_aff_type_mapping.
  PRIVATE SECTION.
    METHODS map_data_type_to_aff
      IMPORTING
        iv_ddic_type       TYPE dd04v-datatype
        iv_length          TYPE dd04v-leng
      RETURNING
        VALUE(rv_aff_type) TYPE zif_abapgit_aff_ddic_types_v1=>ty_data_type.
    METHODS map_data_type_to_ddic
      IMPORTING
        iv_aff_type         TYPE zif_abapgit_aff_ddic_types_v1=>ty_data_type
      RETURNING
        VALUE(rv_ddic_type) TYPE dd04v-datatype.
    METHODS map_reference_category_to_aff
      IMPORTING
        is_dd04v           TYPE dd04v
      RETURNING
        VALUE(rv_category) TYPE zif_abapgit_aff_dtel_v1=>ty_category.
    METHODS map_reference_category_to_ddic
      IMPORTING
        iv_category  TYPE zif_abapgit_aff_dtel_v1=>ty_category
        iv_type_name TYPE zif_abapgit_aff_types_v1=>ty_object_name_30
      CHANGING
        cs_dd04v     TYPE dd04v.
ENDCLASS.

CLASS lcl_aff_type_mapping IMPLEMENTATION.

  METHOD zif_abapgit_aff_type_mapping~to_aff.
    DATA lo_dtel_data TYPE REF TO lcl_dtel_data.
    DATA ls_data_aff TYPE zif_abapgit_aff_dtel_v1=>ty_main.

    TRY.
        lo_dtel_data ?= iv_data.
      CATCH cx_sy_move_cast_error.
        RETURN.
    ENDTRY.

    ls_data_aff-format_version = '1'.
    ls_data_aff-header-description = lo_dtel_data->ms_dd04v-ddtext.
    ls_data_aff-header-original_language = lo_dtel_data->ms_dd04v-ddlanguage.
    ls_data_aff-header-abap_language_version = lo_dtel_data->mv_abap_language_version.

    ls_data_aff-data_type_information-category = map_reference_category_to_aff( lo_dtel_data->ms_dd04v ).
    IF ls_data_aff-data_type_information-category = zif_abapgit_aff_dtel_v1=>co_category-predefined_type.
      ls_data_aff-data_type_information-predefined_type-data_type = map_data_type_to_aff(
        iv_ddic_type = lo_dtel_data->ms_dd04v-datatype
        iv_length    = lo_dtel_data->ms_dd04v-leng ).
      ls_data_aff-data_type_information-predefined_type-length = lo_dtel_data->ms_dd04v-leng.
      ls_data_aff-data_type_information-predefined_type-decimals = lo_dtel_data->ms_dd04v-decimals.
    ELSE.
      ls_data_aff-data_type_information-type_name = lo_dtel_data->ms_dd04v-domname.
    ENDIF.

    ls_data_aff-field_labels-short = lo_dtel_data->ms_dd04v-scrtext_s.
    ls_data_aff-field_labels-short_length = lo_dtel_data->ms_dd04v-scrlen1.
    ls_data_aff-field_labels-medium = lo_dtel_data->ms_dd04v-scrtext_m.
    ls_data_aff-field_labels-medium_length = lo_dtel_data->ms_dd04v-scrlen2.
    ls_data_aff-field_labels-long = lo_dtel_data->ms_dd04v-scrtext_l.
    ls_data_aff-field_labels-long_length = lo_dtel_data->ms_dd04v-scrlen3.
    ls_data_aff-field_labels-heading = lo_dtel_data->ms_dd04v-reptext.
    ls_data_aff-field_labels-heading_length = lo_dtel_data->ms_dd04v-headlen.

    ls_data_aff-additional_properties-search_help-name = lo_dtel_data->ms_dd04v-shlpname.
    ls_data_aff-additional_properties-search_help-parameter = lo_dtel_data->ms_dd04v-shlpfield.
    ls_data_aff-additional_properties-bidirectional_options-basic_direction =
      lo_dtel_data->ms_dd04v-ltrflddis.
    ls_data_aff-additional_properties-bidirectional_options-no_filtering = lo_dtel_data->ms_dd04v-bidictrlc.
    ls_data_aff-additional_properties-parameter_id = lo_dtel_data->ms_dd04v-memoryid.
    ls_data_aff-additional_properties-default_component_name = lo_dtel_data->ms_dd04v-deffdname.
    ls_data_aff-additional_properties-change_document_relevant = lo_dtel_data->ms_dd04v-logflag.
    ls_data_aff-additional_properties-no_input_history = lo_dtel_data->ms_dd04v-nohistory.

    es_data = ls_data_aff.
  ENDMETHOD.

  METHOD zif_abapgit_aff_type_mapping~to_abapgit.
    DATA ls_data_aff TYPE zif_abapgit_aff_dtel_v1=>ty_main.
    DATA lo_dtel_data TYPE REF TO lcl_dtel_data.

    ls_data_aff = iv_data.
    CREATE OBJECT lo_dtel_data.

    lo_dtel_data->ms_dd04v-rollname = to_upper( iv_object_name ).
    lo_dtel_data->ms_dd04v-ddtext = ls_data_aff-header-description.
    lo_dtel_data->ms_dd04v-ddlanguage = ls_data_aff-header-original_language.
    lo_dtel_data->mv_abap_language_version = ls_data_aff-header-abap_language_version.

    IF ls_data_aff-data_type_information-category = zif_abapgit_aff_dtel_v1=>co_category-predefined_type.
      lo_dtel_data->ms_dd04v-refkind = 'T'.
      lo_dtel_data->ms_dd04v-datatype = map_data_type_to_ddic(
        ls_data_aff-data_type_information-predefined_type-data_type ).
      lo_dtel_data->ms_dd04v-leng = ls_data_aff-data_type_information-predefined_type-length.
      lo_dtel_data->ms_dd04v-decimals = ls_data_aff-data_type_information-predefined_type-decimals.
    ELSE.
      map_reference_category_to_ddic(
        EXPORTING
          iv_category  = ls_data_aff-data_type_information-category
          iv_type_name = ls_data_aff-data_type_information-type_name
        CHANGING
          cs_dd04v     = lo_dtel_data->ms_dd04v ).
    ENDIF.

    lo_dtel_data->ms_dd04v-scrtext_s = ls_data_aff-field_labels-short.
    lo_dtel_data->ms_dd04v-scrlen1 = ls_data_aff-field_labels-short_length.
    lo_dtel_data->ms_dd04v-scrtext_m = ls_data_aff-field_labels-medium.
    lo_dtel_data->ms_dd04v-scrlen2 = ls_data_aff-field_labels-medium_length.
    lo_dtel_data->ms_dd04v-scrtext_l = ls_data_aff-field_labels-long.
    lo_dtel_data->ms_dd04v-scrlen3 = ls_data_aff-field_labels-long_length.
    lo_dtel_data->ms_dd04v-reptext = ls_data_aff-field_labels-heading.
    lo_dtel_data->ms_dd04v-headlen = ls_data_aff-field_labels-heading_length.

    lo_dtel_data->ms_dd04v-shlpname = to_upper( ls_data_aff-additional_properties-search_help-name ).
    lo_dtel_data->ms_dd04v-shlpfield = to_upper( ls_data_aff-additional_properties-search_help-parameter ).
    lo_dtel_data->ms_dd04v-ltrflddis =
      ls_data_aff-additional_properties-bidirectional_options-basic_direction.
    lo_dtel_data->ms_dd04v-bidictrlc =
      ls_data_aff-additional_properties-bidirectional_options-no_filtering.
    lo_dtel_data->ms_dd04v-memoryid = to_upper( ls_data_aff-additional_properties-parameter_id ).
    lo_dtel_data->ms_dd04v-deffdname = to_upper( ls_data_aff-additional_properties-default_component_name ).
    lo_dtel_data->ms_dd04v-logflag = ls_data_aff-additional_properties-change_document_relevant.
    lo_dtel_data->ms_dd04v-nohistory = ls_data_aff-additional_properties-no_input_history.

    es_data = lo_dtel_data.
  ENDMETHOD.

  METHOD map_reference_category_to_aff.
    CASE is_dd04v-refkind.
      WHEN 'D'.
        rv_category = zif_abapgit_aff_dtel_v1=>co_category-domain.
      WHEN 'R'.
        CASE is_dd04v-reftype.
          WHEN 'A' OR 'D' OR 'O'.
            rv_category = zif_abapgit_aff_dtel_v1=>co_category-reference_to_predefined_type.
          WHEN 'B' OR 'E' OR 'L' OR 'S'.
            rv_category = zif_abapgit_aff_dtel_v1=>co_category-reference_dictionary_type.
          WHEN 'C' OR 'I'.
            rv_category = zif_abapgit_aff_dtel_v1=>co_category-reference_clas_int_type.
        ENDCASE.
      WHEN OTHERS.
        rv_category = zif_abapgit_aff_dtel_v1=>co_category-predefined_type.
    ENDCASE.
  ENDMETHOD.

  METHOD map_reference_category_to_ddic.
    cs_dd04v-domname = to_upper( iv_type_name ).
    CASE iv_category.
      WHEN zif_abapgit_aff_dtel_v1=>co_category-domain.
        cs_dd04v-refkind = 'D'.
      WHEN zif_abapgit_aff_dtel_v1=>co_category-reference_to_predefined_type.
        cs_dd04v-refkind = 'R'.
        cs_dd04v-datatype = 'REF'.
        CASE cs_dd04v-domname.
          WHEN 'ANY'.
            cs_dd04v-reftype = 'A'.
          WHEN 'OBJECT'.
            cs_dd04v-reftype = 'O'.
          WHEN OTHERS.
            cs_dd04v-reftype = 'D'.
        ENDCASE.
      WHEN zif_abapgit_aff_dtel_v1=>co_category-reference_dictionary_type.
        cs_dd04v-refkind = 'R'.
        cs_dd04v-datatype = 'REF'.
        cs_dd04v-reftype = 'B'.
      WHEN zif_abapgit_aff_dtel_v1=>co_category-reference_clas_int_type.
        cs_dd04v-refkind = 'R'.
        cs_dd04v-datatype = 'REF'.
        IF cs_dd04v-domname CP 'IF_*'
            OR cs_dd04v-domname CP 'ZIF_*'
            OR cs_dd04v-domname CP 'YIF_*'
            OR cs_dd04v-domname CP '/*/IF_*'
            OR cs_dd04v-domname CP '/*/ZIF_*'
            OR cs_dd04v-domname CP '/*/YIF_*'.
          cs_dd04v-reftype = 'I'.
        ELSE.
          cs_dd04v-reftype = 'C'.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD map_data_type_to_aff.
    CASE iv_ddic_type.
      WHEN 'ACCP' OR 'CHAR' OR 'CLNT' OR 'CUKY' OR 'CURR' OR 'DATS' OR 'DATN'
          OR 'DEC' OR 'FLTP' OR 'INT1' OR 'INT2' OR 'INT4' OR 'INT8' OR 'LANG'
          OR 'LCHR' OR 'LRAW' OR 'NUMC' OR 'PREC' OR 'QUAN' OR 'RAW' OR 'TIMS'
          OR 'TIMN' OR 'UNIT' OR 'VARC'.
        rv_aff_type = iv_ddic_type.
      WHEN 'D16D'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-df16_dec.
      WHEN 'D16R'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-df16_raw.
      WHEN 'D16S'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-df16_scl.
      WHEN 'D16N' OR 'DF16'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-decfloat16.
      WHEN 'D34D'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-df34_dec.
      WHEN 'D34R'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-df34_raw.
      WHEN 'D34S'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-df34_scl.
      WHEN 'D34N' OR 'DF34'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-decfloat34.
      WHEN 'DECF'.
        IF iv_length <= 16.
          rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-decfloat16.
        ELSE.
          rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-decfloat34.
        ENDIF.
      WHEN 'GGM1' OR 'GEOM'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-geom_ewkb.
      WHEN 'RSTR' OR 'RAWS'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-rawstring.
      WHEN 'SSTR'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-sstring.
      WHEN 'STRG'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-string.
      WHEN 'UTCL'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-utclong.
    ENDCASE.
  ENDMETHOD.

  METHOD map_data_type_to_ddic.
    " The internal AFF enum values are the corresponding DD04V data type codes.
    rv_ddic_type = iv_aff_type.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_aff_metadata_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      serialize
        IMPORTING
          is_dd04v                 TYPE dd04v
          iv_abap_language_version TYPE uccheck
        RETURNING
          VALUE(rv_json)           TYPE xstring
        RAISING
          zcx_abapgit_exception,
      deserialize
        IMPORTING
          iv_json                  TYPE xstring
          iv_object_name           TYPE sobj_name
        EXPORTING
          es_dd04v                 TYPE dd04v
          ev_abap_language_version TYPE uccheck
        RAISING
          zcx_abapgit_exception.
  PRIVATE SECTION.
    CLASS-METHODS:
      get_enum_mappings
        IMPORTING
          iv_snake_case    TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(rt_result) TYPE zcl_abapgit_json_handler=>ty_enum_mappings,
      add_enum_mapping
        IMPORTING
          iv_path   TYPE string
          iv_abap   TYPE clike
          iv_json   TYPE clike
        CHANGING
          ct_result TYPE zcl_abapgit_json_handler=>ty_enum_mappings,
      validate
        IMPORTING
          is_data_aff TYPE zif_abapgit_aff_dtel_v1=>ty_main
        RAISING
          zcx_abapgit_exception.
ENDCLASS.

CLASS lcl_aff_metadata_handler IMPLEMENTATION.

  METHOD serialize.
    DATA lo_json_handler TYPE REF TO zcl_abapgit_json_handler.
    DATA lo_mapper TYPE REF TO zif_abapgit_aff_type_mapping.
    DATA lo_dtel_data TYPE REF TO lcl_dtel_data.
    DATA ls_data_aff TYPE zif_abapgit_aff_dtel_v1=>ty_main.
    DATA lt_skip_paths TYPE zcl_abapgit_json_handler=>ty_skip_paths.
    DATA ls_skip_path TYPE zcl_abapgit_json_handler=>ty_path_value_pair.
    DATA lx_exception TYPE REF TO cx_root.

    CREATE OBJECT lo_dtel_data.
    lo_dtel_data->ms_dd04v = is_dd04v.
    lo_dtel_data->mv_abap_language_version = iv_abap_language_version.
    CREATE OBJECT lo_mapper TYPE lcl_aff_type_mapping.
    lo_mapper->to_aff( EXPORTING iv_data = lo_dtel_data IMPORTING es_data = ls_data_aff ).
    validate( ls_data_aff ).

    ls_skip_path-path = '/dataTypeInformation/predefinedType/decimals'.
    ls_skip_path-value = '0'.
    APPEND ls_skip_path TO lt_skip_paths.
    ls_skip_path-path = '/additionalProperties/bidirectionalOptions/basicDirection'.
    ls_skip_path-value = 'leftToRight'.
    APPEND ls_skip_path TO lt_skip_paths.
    ls_skip_path-path = '/fieldLabels/shortLength'.
    ls_skip_path-value = '0'.
    APPEND ls_skip_path TO lt_skip_paths.
    ls_skip_path-path = '/fieldLabels/mediumLength'.
    APPEND ls_skip_path TO lt_skip_paths.
    ls_skip_path-path = '/fieldLabels/longLength'.
    APPEND ls_skip_path TO lt_skip_paths.
    ls_skip_path-path = '/fieldLabels/headingLength'.
    APPEND ls_skip_path TO lt_skip_paths.

    CREATE OBJECT lo_json_handler.
    TRY.
        rv_json = lo_json_handler->serialize(
          iv_data          = ls_data_aff
          iv_enum_mappings = get_enum_mappings( )
          iv_skip_paths    = lt_skip_paths ).
      CATCH cx_root INTO lx_exception.
        zcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD deserialize.
    DATA lo_json_handler TYPE REF TO zcl_abapgit_json_handler.
    DATA lo_mapper TYPE REF TO zif_abapgit_aff_type_mapping.
    DATA lo_dtel_data TYPE REF TO lcl_dtel_data.
    DATA ls_data_aff TYPE zif_abapgit_aff_dtel_v1=>ty_main.
    DATA lv_json TYPE string.
    DATA lx_exception TYPE REF TO cx_root.

    lv_json = zcl_abapgit_convert=>xstring_to_string_utf8( iv_json ).
    CREATE OBJECT lo_json_handler.
    TRY.
        lo_json_handler->deserialize(
          EXPORTING
            iv_content       = lv_json
            iv_enum_mappings = get_enum_mappings( abap_true )
          IMPORTING
            ev_data          = ls_data_aff ).
      CATCH cx_root INTO lx_exception.
        zcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDTRY.
    validate( ls_data_aff ).

    CREATE OBJECT lo_mapper TYPE lcl_aff_type_mapping.
    lo_mapper->to_abapgit(
      EXPORTING
        iv_data        = ls_data_aff
        iv_object_name = iv_object_name
      IMPORTING
        es_data        = lo_dtel_data ).
    es_dd04v = lo_dtel_data->ms_dd04v.
    ev_abap_language_version = lo_dtel_data->mv_abap_language_version.
  ENDMETHOD.

  METHOD validate.
    IF is_data_aff-format_version <> '1'.
      zcx_abapgit_exception=>raise( 'DTEL AFF format version is unsupported' ).
    ENDIF.
    IF is_data_aff-header-description IS INITIAL.
      zcx_abapgit_exception=>raise( 'DTEL description is empty' ).
    ENDIF.
    CASE is_data_aff-data_type_information-category.
      WHEN zif_abapgit_aff_dtel_v1=>co_category-predefined_type.
        IF is_data_aff-data_type_information-predefined_type-data_type IS INITIAL.
          zcx_abapgit_exception=>raise( 'DTEL AFF data type is unsupported' ).
        ENDIF.
      WHEN zif_abapgit_aff_dtel_v1=>co_category-domain
          OR zif_abapgit_aff_dtel_v1=>co_category-reference_to_predefined_type
          OR zif_abapgit_aff_dtel_v1=>co_category-reference_dictionary_type
          OR zif_abapgit_aff_dtel_v1=>co_category-reference_clas_int_type.
        IF is_data_aff-data_type_information-type_name IS INITIAL.
          zcx_abapgit_exception=>raise( 'DTEL AFF type name is empty' ).
        ENDIF.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( 'DTEL AFF category is unsupported' ).
    ENDCASE.
  ENDMETHOD.

  METHOD add_enum_mapping.
    DATA ls_mapping TYPE zcl_abapgit_json_handler=>ty_enum_mapping.
    DATA ls_pair TYPE zcl_abapgit_json_handler=>ty_json_abap_mapping.

    READ TABLE ct_result WITH KEY path = iv_path INTO ls_mapping.
    IF sy-subrc <> 0.
      ls_mapping-path = iv_path.
    ENDIF.
    ls_pair-abap = iv_abap.
    ls_pair-json = iv_json.
    APPEND ls_pair TO ls_mapping-mappings.
    DELETE ct_result WHERE path = iv_path.
    APPEND ls_mapping TO ct_result.
  ENDMETHOD.

  METHOD get_enum_mappings.
    FIELD-SYMBOLS <ls_mapping> TYPE zcl_abapgit_json_handler=>ty_enum_mapping.

    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/category'
        iv_abap   = zif_abapgit_aff_dtel_v1=>co_category-domain
        iv_json   = 'domain'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/category'
        iv_abap   = zif_abapgit_aff_dtel_v1=>co_category-predefined_type
        iv_json   = 'predefinedType'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/category'
        iv_abap   = zif_abapgit_aff_dtel_v1=>co_category-reference_to_predefined_type
        iv_json   = 'referenceToPredefinedType'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/category'
        iv_abap   = zif_abapgit_aff_dtel_v1=>co_category-reference_dictionary_type
        iv_json   = 'referenceDictionaryType'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/category'
        iv_abap   = zif_abapgit_aff_dtel_v1=>co_category-reference_clas_int_type
        iv_json   = 'referenceClasIntType'
      CHANGING
        ct_result = rt_result ).

    add_enum_mapping(
      EXPORTING
        iv_path   = '/additionalProperties/bidirectionalOptions/basicDirection'
        iv_abap   = space
        iv_json   = 'leftToRight'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/additionalProperties/bidirectionalOptions/basicDirection'
        iv_abap   = abap_true
        iv_json   = 'rightToLeft'
      CHANGING
        ct_result = rt_result ).

    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = 'D16D'
        iv_json   = 'DF16_DEC'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = 'D16R'
        iv_json   = 'DF16_RAW'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = 'D16S'
        iv_json   = 'DF16_SCL'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = 'D16N'
        iv_json   = 'DECFLOAT16'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = 'D34D'
        iv_json   = 'DF34_DEC'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = 'D34R'
        iv_json   = 'DF34_RAW'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = 'D34S'
        iv_json   = 'DF34_SCL'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = 'D34N'
        iv_json   = 'DECFLOAT34'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = 'GGM1'
        iv_json   = 'GEOM_EWKB'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = 'RSTR'
        iv_json   = 'RAWSTRING'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = 'SSTR'
        iv_json   = 'SSTRING'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = 'STRG'
        iv_json   = 'STRING'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = 'UTCL'
        iv_json   = 'UTCLONG'
      CHANGING
        ct_result = rt_result ).

    " Deserialization maps JSON member names to snake case before applying custom enums.
    IF iv_snake_case = abap_true.
      LOOP AT rt_result ASSIGNING <ls_mapping>.
        CASE <ls_mapping>-path.
          WHEN '/dataTypeInformation/category'.
            <ls_mapping>-path = '/data_type_information/category'.
          WHEN '/additionalProperties/bidirectionalOptions/basicDirection'.
            <ls_mapping>-path = '/additional_properties/bidirectional_options/basic_direction'.
          WHEN '/dataTypeInformation/predefinedType/dataType'.
            <ls_mapping>-path = '/data_type_information/predefined_type/data_type'.
        ENDCASE.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
