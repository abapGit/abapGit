
CLASS lcl_aff_type_mapping DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_aff_type_mapping.

    METHODS constructor
      IMPORTING
        it_files TYPE zif_abapgit_git_definitions=>ty_files_tt OPTIONAL.

    TYPES:
      BEGIN OF ty_dtel_data,
        dd04v                 TYPE dd04v,
        abap_language_version TYPE uccheck,
      END OF ty_dtel_data.
  PRIVATE SECTION.
    DATA mt_files TYPE zif_abapgit_git_definitions=>ty_files_tt.

    METHODS object_exists_in_files
      IMPORTING
        iv_object_type   TYPE tadir-object
        iv_object_name   TYPE dd04v-domname
      RETURNING
        VALUE(rv_exists) TYPE abap_bool.
    METHODS resolve_dictionary_reference
      IMPORTING
        iv_type_name      TYPE dd04v-domname
      RETURNING
        VALUE(rv_reftype) TYPE dd04v-reftype.
    METHODS resolve_clif_reference
      IMPORTING
        iv_type_name      TYPE dd04v-domname
      RETURNING
        VALUE(rv_reftype) TYPE dd04v-reftype.
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

  METHOD constructor.
    mt_files = it_files.
  ENDMETHOD.

  METHOD object_exists_in_files.
    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_filename TYPE string.

    ls_item-obj_type = iv_object_type.
    ls_item-obj_name = iv_object_name.

    lv_filename = zcl_abapgit_filename_logic=>object_to_file(
      is_item = ls_item
      iv_ext  = 'json' ).
    READ TABLE mt_files TRANSPORTING NO FIELDS
      WITH KEY file COMPONENTS filename = lv_filename.
    IF sy-subrc = 0.
      rv_exists = abap_true.
      RETURN.
    ENDIF.

    lv_filename = zcl_abapgit_filename_logic=>object_to_file(
      is_item = ls_item
      iv_ext  = 'xml' ).
    READ TABLE mt_files TRANSPORTING NO FIELDS
      WITH KEY file COMPONENTS filename = lv_filename.
    rv_exists = boolc( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD resolve_dictionary_reference.
    DATA lv_object_type TYPE tadir-object.

    IF object_exists_in_files(
      iv_object_type = 'DTEL'
      iv_object_name = iv_type_name ) = abap_true.
      rv_reftype = 'E'.
      RETURN.
    ELSEIF object_exists_in_files(
      iv_object_type = 'TTYP'
      iv_object_name = iv_type_name ) = abap_true.
      rv_reftype = 'L'.
      RETURN.
    ELSEIF object_exists_in_files(
      iv_object_type = 'TABL'
      iv_object_name = iv_type_name ) = abap_true.
      rv_reftype = 'S'.
      RETURN.
    ENDIF.

    SELECT SINGLE object FROM tadir INTO lv_object_type
      WHERE pgmid = 'R3TR'
      AND object = 'DTEL'
      AND obj_name = iv_type_name.
    IF sy-subrc = 0.
      rv_reftype = 'E'.
      RETURN.
    ENDIF.

    SELECT SINGLE object FROM tadir INTO lv_object_type
      WHERE pgmid = 'R3TR'
      AND object = 'TTYP'
      AND obj_name = iv_type_name.
    IF sy-subrc = 0.
      rv_reftype = 'L'.
      RETURN.
    ENDIF.

    SELECT SINGLE object FROM tadir INTO lv_object_type
      WHERE pgmid = 'R3TR'
      AND object = 'TABL'
      AND obj_name = iv_type_name.
    IF sy-subrc = 0.
      rv_reftype = 'S'.
    ELSE.
      rv_reftype = 'B'.
    ENDIF.
  ENDMETHOD.

  METHOD resolve_clif_reference.
    DATA lv_object_type TYPE tadir-object.

    IF object_exists_in_files(
      iv_object_type = 'INTF'
      iv_object_name = iv_type_name ) = abap_true.
      rv_reftype = 'I'.
      RETURN.
    ELSEIF object_exists_in_files(
      iv_object_type = 'CLAS'
      iv_object_name = iv_type_name ) = abap_true.
      rv_reftype = 'C'.
      RETURN.
    ENDIF.

    SELECT SINGLE object FROM tadir INTO lv_object_type
      WHERE pgmid = 'R3TR'
      AND obj_name = iv_type_name
      AND ( object = 'INTF' OR object = 'CLAS' ).
    IF sy-subrc = 0.
      IF lv_object_type = 'INTF'.
        rv_reftype = 'I'.
      ELSE.
        rv_reftype = 'C'.
      ENDIF.
    ELSEIF iv_type_name CP 'IF_*'
        OR iv_type_name CP 'ZIF_*'
        OR iv_type_name CP 'YIF_*'
        OR iv_type_name CP '/*/IF_*'
        OR iv_type_name CP '/*/ZIF_*'
        OR iv_type_name CP '/*/YIF_*'.
      " Keep the historical naming fallback for references that are neither
      " part of the repository nor installed locally.
      rv_reftype = 'I'.
    ELSE.
      rv_reftype = 'C'.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_aff_type_mapping~to_aff.
    DATA ls_dtel_data TYPE ty_dtel_data.
    DATA ls_data_aff TYPE zif_abapgit_aff_dtel_v1=>ty_main.
    DATA lv_ddic_type TYPE dd04v-datatype.

    ls_dtel_data = iv_data.

    ls_data_aff-format_version = '1'.
    ls_data_aff-header-description = ls_dtel_data-dd04v-ddtext.
    ls_data_aff-header-original_language = ls_dtel_data-dd04v-ddlanguage.
    ls_data_aff-header-abap_language_version = ls_dtel_data-abap_language_version.

    ls_data_aff-data_type_information-category = map_reference_category_to_aff( ls_dtel_data-dd04v ).
    IF ls_data_aff-data_type_information-category = zif_abapgit_aff_dtel_v1=>co_category-predefined_type.
      ls_data_aff-data_type_information-predefined_type-data_type = map_data_type_to_aff(
        iv_ddic_type = ls_dtel_data-dd04v-datatype
        iv_length    = ls_dtel_data-dd04v-leng ).
      ls_data_aff-data_type_information-predefined_type-length = ls_dtel_data-dd04v-leng.
      ls_data_aff-data_type_information-predefined_type-decimals = ls_dtel_data-dd04v-decimals.
    ELSEIF ls_data_aff-data_type_information-category =
        zif_abapgit_aff_dtel_v1=>co_category-reference_to_predefined_type.
      CASE ls_dtel_data-dd04v-reftype.
        WHEN 'A'.
          IF ls_dtel_data-dd04v-datatype = 'REF'.
            " Legacy representation used a type name for REF TO ANY.
            ls_data_aff-data_type_information-type_name = 'ANY'.
          ELSE.
            ls_data_aff-data_type_information-predefined_type-data_type = map_data_type_to_aff(
              iv_ddic_type = ls_dtel_data-dd04v-datatype
              iv_length    = ls_dtel_data-dd04v-leng ).
            ls_data_aff-data_type_information-predefined_type-length = ls_dtel_data-dd04v-leng.
            ls_data_aff-data_type_information-predefined_type-decimals = ls_dtel_data-dd04v-decimals.
          ENDIF.
        WHEN 'B'.
          lv_ddic_type = ls_dtel_data-dd04v-domname.
          ls_data_aff-data_type_information-predefined_type-data_type = map_data_type_to_aff(
            iv_ddic_type = lv_ddic_type
            iv_length    = ls_dtel_data-dd04v-leng ).
          ls_data_aff-data_type_information-predefined_type-length = ls_dtel_data-dd04v-leng.
          ls_data_aff-data_type_information-predefined_type-decimals = ls_dtel_data-dd04v-decimals.
        WHEN 'D'.
          ls_data_aff-data_type_information-type_name = 'DATA'.
        WHEN 'O'.
          ls_data_aff-data_type_information-type_name = 'OBJECT'.
      ENDCASE.
    ELSE.
      ls_data_aff-data_type_information-type_name = ls_dtel_data-dd04v-domname.
    ENDIF.

    ls_data_aff-field_labels-short = ls_dtel_data-dd04v-scrtext_s.
    ls_data_aff-field_labels-short_length = ls_dtel_data-dd04v-scrlen1.
    ls_data_aff-field_labels-medium = ls_dtel_data-dd04v-scrtext_m.
    ls_data_aff-field_labels-medium_length = ls_dtel_data-dd04v-scrlen2.
    ls_data_aff-field_labels-long = ls_dtel_data-dd04v-scrtext_l.
    ls_data_aff-field_labels-long_length = ls_dtel_data-dd04v-scrlen3.
    ls_data_aff-field_labels-heading = ls_dtel_data-dd04v-reptext.
    ls_data_aff-field_labels-heading_length = ls_dtel_data-dd04v-headlen.

    ls_data_aff-additional_properties-search_help-name = ls_dtel_data-dd04v-shlpname.
    ls_data_aff-additional_properties-search_help-parameter = ls_dtel_data-dd04v-shlpfield.
    ls_data_aff-additional_properties-bidirectional_options-basic_direction =
      ls_dtel_data-dd04v-ltrflddis.
    ls_data_aff-additional_properties-bidirectional_options-no_filtering = ls_dtel_data-dd04v-bidictrlc.
    ls_data_aff-additional_properties-parameter_id = ls_dtel_data-dd04v-memoryid.
    ls_data_aff-additional_properties-default_component_name = ls_dtel_data-dd04v-deffdname.
    ls_data_aff-additional_properties-change_document_relevant = ls_dtel_data-dd04v-logflag.
    ls_data_aff-additional_properties-no_input_history = ls_dtel_data-dd04v-nohistory.

    es_data = ls_data_aff.
  ENDMETHOD.

  METHOD zif_abapgit_aff_type_mapping~to_abapgit.
    DATA ls_data_aff TYPE zif_abapgit_aff_dtel_v1=>ty_main.
    DATA ls_dtel_data TYPE ty_dtel_data.

    ls_data_aff = iv_data.

    ls_dtel_data-dd04v-rollname = to_upper( iv_object_name ).
    ls_dtel_data-dd04v-ddtext = ls_data_aff-header-description.
    ls_dtel_data-dd04v-ddlanguage = ls_data_aff-header-original_language.
    ls_dtel_data-abap_language_version = ls_data_aff-header-abap_language_version.

    IF ls_data_aff-data_type_information-category = zif_abapgit_aff_dtel_v1=>co_category-predefined_type.
      ls_dtel_data-dd04v-refkind = 'T'.
      ls_dtel_data-dd04v-datatype = map_data_type_to_ddic(
        ls_data_aff-data_type_information-predefined_type-data_type ).
      ls_dtel_data-dd04v-leng = ls_data_aff-data_type_information-predefined_type-length.
      ls_dtel_data-dd04v-decimals = ls_data_aff-data_type_information-predefined_type-decimals.
    ELSEIF ls_data_aff-data_type_information-category =
        zif_abapgit_aff_dtel_v1=>co_category-reference_to_predefined_type
        AND ls_data_aff-data_type_information-predefined_type-data_type IS NOT INITIAL.
      ls_dtel_data-dd04v-refkind = 'R'.
      ls_dtel_data-dd04v-reftype = 'B'.
      ls_dtel_data-dd04v-datatype = 'REF'.
      ls_dtel_data-dd04v-domname = map_data_type_to_ddic(
        ls_data_aff-data_type_information-predefined_type-data_type ).
      ls_dtel_data-dd04v-leng = ls_data_aff-data_type_information-predefined_type-length.
      ls_dtel_data-dd04v-decimals = ls_data_aff-data_type_information-predefined_type-decimals.
    ELSE.
      map_reference_category_to_ddic(
        EXPORTING
          iv_category  = ls_data_aff-data_type_information-category
          iv_type_name = ls_data_aff-data_type_information-type_name
        CHANGING
          cs_dd04v     = ls_dtel_data-dd04v ).
    ENDIF.

    ls_dtel_data-dd04v-scrtext_s = ls_data_aff-field_labels-short.
    ls_dtel_data-dd04v-scrlen1 = ls_data_aff-field_labels-short_length.
    ls_dtel_data-dd04v-scrtext_m = ls_data_aff-field_labels-medium.
    ls_dtel_data-dd04v-scrlen2 = ls_data_aff-field_labels-medium_length.
    ls_dtel_data-dd04v-scrtext_l = ls_data_aff-field_labels-long.
    ls_dtel_data-dd04v-scrlen3 = ls_data_aff-field_labels-long_length.
    ls_dtel_data-dd04v-reptext = ls_data_aff-field_labels-heading.
    ls_dtel_data-dd04v-headlen = ls_data_aff-field_labels-heading_length.

    ls_dtel_data-dd04v-shlpname = to_upper( ls_data_aff-additional_properties-search_help-name ).
    ls_dtel_data-dd04v-shlpfield = to_upper( ls_data_aff-additional_properties-search_help-parameter ).
    ls_dtel_data-dd04v-ltrflddis =
      ls_data_aff-additional_properties-bidirectional_options-basic_direction.
    ls_dtel_data-dd04v-bidictrlc =
      ls_data_aff-additional_properties-bidirectional_options-no_filtering.
    ls_dtel_data-dd04v-memoryid = to_upper( ls_data_aff-additional_properties-parameter_id ).
    ls_dtel_data-dd04v-deffdname = to_upper( ls_data_aff-additional_properties-default_component_name ).
    ls_dtel_data-dd04v-logflag = ls_data_aff-additional_properties-change_document_relevant.
    ls_dtel_data-dd04v-nohistory = ls_data_aff-additional_properties-no_input_history.

    es_data = ls_dtel_data.
  ENDMETHOD.

  METHOD map_reference_category_to_aff.
    CASE is_dd04v-refkind.
      WHEN 'D'.
        rv_category = zif_abapgit_aff_dtel_v1=>co_category-domain.
      WHEN 'R'.
        CASE is_dd04v-reftype.
          WHEN 'A' OR 'D' OR 'O'.
            rv_category = zif_abapgit_aff_dtel_v1=>co_category-reference_to_predefined_type.
          WHEN 'B'.
            rv_category = zif_abapgit_aff_dtel_v1=>co_category-reference_to_predefined_type.
          WHEN 'E' OR 'L' OR 'S'.
            rv_category = zif_abapgit_aff_dtel_v1=>co_category-reference_dictionary_type.
          WHEN 'C' OR 'I'.
            rv_category = zif_abapgit_aff_dtel_v1=>co_category-reference_clas_int_type.
        ENDCASE.
      WHEN OTHERS.
        rv_category = zif_abapgit_aff_dtel_v1=>co_category-predefined_type.
    ENDCASE.
  ENDMETHOD.

  METHOD map_reference_category_to_ddic.
    DATA lv_reftype TYPE dd04v-reftype.

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
        lv_reftype = resolve_dictionary_reference( cs_dd04v-domname ).
        cs_dd04v-reftype = lv_reftype.
        cs_dd04v-datatype = 'REF'.
      WHEN zif_abapgit_aff_dtel_v1=>co_category-reference_clas_int_type.
        cs_dd04v-refkind = 'R'.
        cs_dd04v-datatype = 'REF'.
        cs_dd04v-reftype = resolve_clif_reference( cs_dd04v-domname ).
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
          it_files                 TYPE zif_abapgit_git_definitions=>ty_files_tt OPTIONAL
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
      get_skip_paths
        IMPORTING
          is_data_aff      TYPE zif_abapgit_aff_dtel_v1=>ty_main
        RETURNING
          VALUE(rt_result) TYPE zcl_abapgit_json_handler=>ty_skip_paths,
      validate
        IMPORTING
          is_data_aff    TYPE zif_abapgit_aff_dtel_v1=>ty_main
          iv_object_name TYPE clike
        RAISING
          zcx_abapgit_exception.
ENDCLASS.

CLASS lcl_aff_metadata_handler IMPLEMENTATION.

  METHOD serialize.
    DATA lo_json_handler TYPE REF TO zcl_abapgit_json_handler.
    DATA lo_mapper TYPE REF TO zif_abapgit_aff_type_mapping.
    DATA ls_dtel_data TYPE lcl_aff_type_mapping=>ty_dtel_data.
    DATA ls_data_aff TYPE zif_abapgit_aff_dtel_v1=>ty_main.
    DATA lx_exception TYPE REF TO cx_root.

    ls_dtel_data-dd04v = is_dd04v.
    ls_dtel_data-abap_language_version = iv_abap_language_version.

    CREATE OBJECT lo_mapper TYPE lcl_aff_type_mapping.
    lo_mapper->to_aff( EXPORTING iv_data = ls_dtel_data IMPORTING es_data = ls_data_aff ).
    validate( is_data_aff    = ls_data_aff
              iv_object_name = is_dd04v-rollname ).

    CREATE OBJECT lo_json_handler.
    TRY.
        rv_json = lo_json_handler->serialize(
          iv_data          = ls_data_aff
          iv_enum_mappings = get_enum_mappings( )
          iv_skip_paths    = get_skip_paths( ls_data_aff ) ).
      CATCH cx_root INTO lx_exception.
        zcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD deserialize.
    DATA lo_json_handler TYPE REF TO zcl_abapgit_json_handler.
    DATA lo_mapper TYPE REF TO zif_abapgit_aff_type_mapping.
    DATA ls_dtel_data TYPE lcl_aff_type_mapping=>ty_dtel_data.
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
    validate( is_data_aff    = ls_data_aff
              iv_object_name = iv_object_name ).

    CREATE OBJECT lo_mapper TYPE lcl_aff_type_mapping
      EXPORTING
        it_files = it_files.
    lo_mapper->to_abapgit(
      EXPORTING
        iv_data        = ls_data_aff
        iv_object_name = iv_object_name
      IMPORTING
        es_data        = ls_dtel_data ).
    es_dd04v = ls_dtel_data-dd04v.
    ev_abap_language_version = ls_dtel_data-abap_language_version.

    " The JSON handler resolves "standard" to the source based representation ('X'),
    " while DD04L expects the DDIC representation (initial)
    IF ev_abap_language_version = zif_abapgit_aff_types_v1=>co_abap_language_version_src-standard.
      ev_abap_language_version = zif_abapgit_aff_types_v1=>co_abap_language_version-standard.
    ENDIF.
  ENDMETHOD.

  METHOD validate.
    IF is_data_aff-format_version <> '1'.
      zcx_abapgit_exception=>raise( |DTEL { iv_object_name }: unsupported AFF format version | &&
                                   |{ is_data_aff-format_version }| ).
    ENDIF.
    IF is_data_aff-header-description IS INITIAL.
      zcx_abapgit_exception=>raise( |DTEL { iv_object_name }: description is empty| ).
    ENDIF.
    CASE is_data_aff-data_type_information-category.
      WHEN zif_abapgit_aff_dtel_v1=>co_category-predefined_type
          OR zif_abapgit_aff_dtel_v1=>co_category-reference_to_predefined_type.
        IF is_data_aff-data_type_information-predefined_type-data_type IS INITIAL
            AND is_data_aff-data_type_information-type_name IS INITIAL.
          zcx_abapgit_exception=>raise( |DTEL { iv_object_name }: unsupported data type| ).
        ENDIF.
      WHEN zif_abapgit_aff_dtel_v1=>co_category-domain
          OR zif_abapgit_aff_dtel_v1=>co_category-reference_dictionary_type
          OR zif_abapgit_aff_dtel_v1=>co_category-reference_clas_int_type.
        IF is_data_aff-data_type_information-type_name IS INITIAL.
          zcx_abapgit_exception=>raise( |DTEL { iv_object_name }: type name is empty| ).
        ENDIF.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( |DTEL { iv_object_name }: unsupported category| ).
    ENDCASE.
  ENDMETHOD.

  METHOD get_skip_paths.
    DATA ls_skip_path TYPE zcl_abapgit_json_handler=>ty_path_value_pair.

    " Numeric fields are serialized even when they are zero, so every
    " optional one has to be skipped explicitly
    ls_skip_path-value = '0'.
    ls_skip_path-path = '/dataTypeInformation/predefinedType/decimals'.
    APPEND ls_skip_path TO rt_result.
    ls_skip_path-path = '/fieldLabels/shortLength'.
    APPEND ls_skip_path TO rt_result.
    ls_skip_path-path = '/fieldLabels/mediumLength'.
    APPEND ls_skip_path TO rt_result.
    ls_skip_path-path = '/fieldLabels/longLength'.
    APPEND ls_skip_path TO rt_result.
    ls_skip_path-path = '/fieldLabels/headingLength'.
    APPEND ls_skip_path TO rt_result.

    IF is_data_aff-data_type_information-category <> zif_abapgit_aff_dtel_v1=>co_category-predefined_type
        AND ( is_data_aff-data_type_information-category <>
              zif_abapgit_aff_dtel_v1=>co_category-reference_to_predefined_type
              OR is_data_aff-data_type_information-predefined_type-data_type IS INITIAL ).
      " "length" is mandatory for predefined types and must be kept even when zero.
      " For all other categories it drops out together with the whole predefinedType node
      ls_skip_path-path = '/dataTypeInformation/predefinedType/length'.
      APPEND ls_skip_path TO rt_result.
    ENDIF.

    ls_skip_path-path = '/additionalProperties/bidirectionalOptions/basicDirection'.
    ls_skip_path-value = 'leftToRight'.
    APPEND ls_skip_path TO rt_result.
  ENDMETHOD.

  METHOD add_enum_mapping.
    DATA ls_mapping TYPE zcl_abapgit_json_handler=>ty_enum_mapping.
    DATA ls_pair TYPE zcl_abapgit_json_handler=>ty_json_abap_mapping.

    FIELD-SYMBOLS <ls_mapping> TYPE zcl_abapgit_json_handler=>ty_enum_mapping.

    ls_pair-abap = iv_abap.
    ls_pair-json = iv_json.

    READ TABLE ct_result ASSIGNING <ls_mapping> WITH KEY path = iv_path.
    IF sy-subrc = 0.
      APPEND ls_pair TO <ls_mapping>-mappings.
    ELSE.
      ls_mapping-path = iv_path.
      APPEND ls_pair TO ls_mapping-mappings.
      APPEND ls_mapping TO ct_result.
    ENDIF.
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
        iv_abap   = zif_abapgit_aff_dtel_v1=>co_bidi_basic_direction-left_to_right
        iv_json   = 'leftToRight'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/additionalProperties/bidirectionalOptions/basicDirection'
        iv_abap   = zif_abapgit_aff_dtel_v1=>co_bidi_basic_direction-right_to_left
        iv_json   = 'rightToLeft'
      CHANGING
        ct_result = rt_result ).

    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = zif_abapgit_aff_ddic_types_v1=>co_data_type-df16_dec
        iv_json   = 'DF16_DEC'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = zif_abapgit_aff_ddic_types_v1=>co_data_type-df16_raw
        iv_json   = 'DF16_RAW'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = zif_abapgit_aff_ddic_types_v1=>co_data_type-df16_scl
        iv_json   = 'DF16_SCL'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = zif_abapgit_aff_ddic_types_v1=>co_data_type-decfloat16
        iv_json   = 'DECFLOAT16'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = zif_abapgit_aff_ddic_types_v1=>co_data_type-df34_dec
        iv_json   = 'DF34_DEC'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = zif_abapgit_aff_ddic_types_v1=>co_data_type-df34_raw
        iv_json   = 'DF34_RAW'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = zif_abapgit_aff_ddic_types_v1=>co_data_type-df34_scl
        iv_json   = 'DF34_SCL'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = zif_abapgit_aff_ddic_types_v1=>co_data_type-decfloat34
        iv_json   = 'DECFLOAT34'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = zif_abapgit_aff_ddic_types_v1=>co_data_type-geom_ewkb
        iv_json   = 'GEOM_EWKB'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = zif_abapgit_aff_ddic_types_v1=>co_data_type-rawstring
        iv_json   = 'RAWSTRING'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = zif_abapgit_aff_ddic_types_v1=>co_data_type-sstring
        iv_json   = 'SSTRING'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = zif_abapgit_aff_ddic_types_v1=>co_data_type-string
        iv_json   = 'STRING'
      CHANGING
        ct_result = rt_result ).
    add_enum_mapping(
      EXPORTING
        iv_path   = '/dataTypeInformation/predefinedType/dataType'
        iv_abap   = zif_abapgit_aff_ddic_types_v1=>co_data_type-utclong
        iv_json   = 'UTCLONG'
      CHANGING
        ct_result = rt_result ).

    IF iv_snake_case = abap_true.
      " Deserialization renames the JSON members to snake case before the custom enums are applied
      LOOP AT rt_result ASSIGNING <ls_mapping>.
        REPLACE ALL OCCURRENCES OF REGEX `([a-z])([A-Z])`
          IN <ls_mapping>-path WITH `$1_$2` ##REGEX_POSIX.
        <ls_mapping>-path = to_lower( <ls_mapping>-path ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
