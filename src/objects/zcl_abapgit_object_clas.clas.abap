CLASS zcl_abapgit_object_clas DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_program
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .

    METHODS constructor
      IMPORTING
        !is_item     TYPE zif_abapgit_definitions=>ty_item
        !iv_language TYPE spras .
  PROTECTED SECTION.
    DATA: mi_object_oriented_object_fct TYPE REF TO zif_abapgit_oo_object_fnc,
          mv_skip_testclass             TYPE abap_bool,
          mv_classpool_name             TYPE progname.
    METHODS:
      deserialize_abap
        IMPORTING ii_xml     TYPE REF TO zif_abapgit_xml_input
                  iv_package TYPE devclass
        RAISING   zcx_abapgit_exception,
      deserialize_docu
        IMPORTING ii_xml TYPE REF TO zif_abapgit_xml_input
        RAISING   zcx_abapgit_exception,
      deserialize_tpool
        IMPORTING ii_xml TYPE REF TO zif_abapgit_xml_input
        RAISING   zcx_abapgit_exception,
      deserialize_tpool_i18n
        IMPORTING ii_xml TYPE REF TO zif_abapgit_xml_input
        RAISING   zcx_abapgit_exception,
      deserialize_sotr
        IMPORTING ii_xml     TYPE REF TO zif_abapgit_xml_input
                  iv_package TYPE devclass
        RAISING   zcx_abapgit_exception,
      serialize_xml
        IMPORTING ii_xml TYPE REF TO zif_abapgit_xml_output
        RAISING   zcx_abapgit_exception,
      serialize_attr
        IMPORTING
          !ii_xml     TYPE REF TO zif_abapgit_xml_output
          !iv_clsname TYPE seoclsname
        RAISING
          zcx_abapgit_exception,
      serialize_descr
        IMPORTING
          !ii_xml     TYPE REF TO zif_abapgit_xml_output
          !iv_clsname TYPE seoclsname
        RAISING
          zcx_abapgit_exception,
      serialize_descr_sub
        IMPORTING
          !ii_xml     TYPE REF TO zif_abapgit_xml_output
          !iv_clsname TYPE seoclsname
        RAISING
          zcx_abapgit_exception,
      serialize_docu
        IMPORTING
          !ii_xml              TYPE REF TO zif_abapgit_xml_output
          !it_langu_additional TYPE zif_abapgit_lang_definitions=>ty_langus OPTIONAL
          !iv_clsname          TYPE seoclsname
        RAISING
          zcx_abapgit_exception,
      serialize_tpool
        IMPORTING
          !ii_xml         TYPE REF TO zif_abapgit_xml_output
          !iv_clsname     TYPE seoclsname
        RETURNING
          VALUE(rt_tpool) TYPE textpool_table
        RAISING
          zcx_abapgit_exception,
      serialize_tpool_i18n
        IMPORTING
          !ii_xml              TYPE REF TO zif_abapgit_xml_output
          !it_langu_additional TYPE zif_abapgit_lang_definitions=>ty_langus OPTIONAL
          !iv_clsname          TYPE seoclsname
          !it_tpool_main       TYPE textpool_table
        RAISING
          zcx_abapgit_exception,
      serialize_sotr
        IMPORTING
          !ii_xml TYPE REF TO zif_abapgit_xml_output
        RAISING
          zcx_abapgit_exception,
      source_apack_replacement
        CHANGING
          !ct_source TYPE seop_source_string
        RAISING
          zcx_abapgit_exception,
      repo_apack_replacement
        CHANGING
          !ct_source TYPE seop_source_string
        RAISING
          zcx_abapgit_exception.

  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_longtext_name,
        attributes TYPE string VALUE 'LONGTEXTS_CA',
        methods    TYPE string VALUE 'LONGTEXTS_CO',
        events     TYPE string VALUE 'LONGTEXTS_CE',
        types      TYPE string VALUE 'LONGTEXTS_CT',
      END OF c_longtext_name.
    CONSTANTS:
      BEGIN OF c_longtext_id,
        class      TYPE dokil-id VALUE 'CL',
        attributes TYPE dokil-id VALUE 'CA',
        methods    TYPE dokil-id VALUE 'CO',
        events     TYPE dokil-id VALUE 'CE',
        types      TYPE dokil-id VALUE 'CT',
      END OF c_longtext_id.

    METHODS deserialize_pre_ddic
      IMPORTING
        !ii_xml     TYPE REF TO zif_abapgit_xml_input
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception.

    METHODS is_class_locked
      RETURNING
        VALUE(rv_is_class_locked) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.

    METHODS interface_replacement
      IMPORTING
        !iv_from_interface TYPE seoclsname
        !iv_to_interface   TYPE seoclsname
      CHANGING
        !ct_source         TYPE seop_source_string.

ENDCLASS.



CLASS zcl_abapgit_object_clas IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    CREATE OBJECT mi_object_oriented_object_fct TYPE zcl_abapgit_oo_class.

    mv_classpool_name = cl_oo_classname_service=>get_classpool_name( |{ is_item-obj_name }| ).

  ENDMETHOD.


  METHOD deserialize_abap.

    DATA: ls_vseoclass             TYPE vseoclass,
          lt_source                TYPE seop_source_string,
          lt_local_definitions     TYPE seop_source_string,
          lt_local_implementations TYPE seop_source_string,
          lt_local_macros          TYPE seop_source_string,
          lt_test_classes          TYPE seop_source_string,
          lt_descriptions          TYPE zif_abapgit_oo_object_fnc=>ty_seocompotx_tt,
          lt_descriptions_sub      TYPE zif_abapgit_oo_object_fnc=>ty_seosubcotx_tt,
          ls_class_key             TYPE seoclskey,
          lt_attributes            TYPE zif_abapgit_definitions=>ty_obj_attribute_tt.


    lt_source = zif_abapgit_object~mo_files->read_abap( ).

    lt_local_definitions = zif_abapgit_object~mo_files->read_abap(
      iv_extra = zif_abapgit_oo_object_fnc=>c_parts-locals_def
      iv_error = abap_false ).

    lt_local_implementations = zif_abapgit_object~mo_files->read_abap(
      iv_extra = zif_abapgit_oo_object_fnc=>c_parts-locals_imp
      iv_error = abap_false ).

    lt_local_macros = zif_abapgit_object~mo_files->read_abap(
      iv_extra = zif_abapgit_oo_object_fnc=>c_parts-macros
      iv_error = abap_false ).

    lt_test_classes = zif_abapgit_object~mo_files->read_abap(
      iv_extra = zif_abapgit_oo_object_fnc=>c_parts-testclasses
      iv_error = abap_false ).

    ls_class_key-clsname = ms_item-obj_name.

    ii_xml->read( EXPORTING iv_name = 'VSEOCLASS'
                  CHANGING  cg_data = ls_vseoclass ).

    ii_xml->read( EXPORTING iv_name = 'ATTRIBUTES'
                  CHANGING  cg_data = lt_attributes ).

    " Remove code for test classes if they have been deleted
    IF ls_vseoclass-with_unit_tests = abap_false.
      CLEAR lt_test_classes.
    ENDIF.

    mi_object_oriented_object_fct->create(
      EXPORTING
        iv_check      = abap_true
        iv_package    = iv_package
        it_attributes = lt_attributes
      CHANGING
        cg_properties = ls_vseoclass ).

    mi_object_oriented_object_fct->generate_locals(
      is_key                   = ls_class_key
      it_local_definitions     = lt_local_definitions
      it_local_implementations = lt_local_implementations
      it_local_macros          = lt_local_macros
      it_local_test_classes    = lt_test_classes ).

    repo_apack_replacement( CHANGING ct_source = lt_source ).
    mi_object_oriented_object_fct->deserialize_source(
      is_key    = ls_class_key
      it_source = lt_source ).

    ii_xml->read( EXPORTING iv_name = 'DESCRIPTIONS'
                  CHANGING cg_data = lt_descriptions ).

    mi_object_oriented_object_fct->update_descriptions(
      is_key          = ls_class_key
      it_descriptions = lt_descriptions ).

    ii_xml->read( EXPORTING iv_name = 'DESCRIPTIONS_SUB'
                  CHANGING cg_data = lt_descriptions_sub ).

    mi_object_oriented_object_fct->update_descriptions_sub(
      is_key          = ls_class_key
      it_descriptions = lt_descriptions_sub ).

    mi_object_oriented_object_fct->add_to_activation_list( ms_item ).

  ENDMETHOD.


  METHOD deserialize_docu.

    DATA: lt_lines      TYPE tlinetab,
          lv_object     TYPE dokhl-object,
          lt_i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_lines,
          ls_i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_line.

    ii_xml->read( EXPORTING iv_name = 'LINES'
                  CHANGING cg_data = lt_lines ).

    lv_object = ms_item-obj_name.

    IF lines( lt_lines ) = 0.
      mi_object_oriented_object_fct->delete_documentation(
        iv_id          = c_longtext_id-class
        iv_object_name = lv_object
        iv_language    = mv_language ).
      RETURN.
    ENDIF.

    mi_object_oriented_object_fct->create_documentation(
      it_lines       = lt_lines
      iv_id          = c_longtext_id-class
      iv_object_name = lv_object
      iv_language    = mv_language ).

    ii_xml->read( EXPORTING iv_name = 'I18N_LINES'
                  CHANGING cg_data = lt_i18n_lines ).

    LOOP AT lt_i18n_lines INTO ls_i18n_lines.
      mi_object_oriented_object_fct->create_documentation(
        it_lines         = ls_i18n_lines-lines
        iv_id            = c_longtext_id-class
        iv_object_name   = lv_object
        iv_language      = ls_i18n_lines-language
        iv_no_masterlang = abap_true ).
    ENDLOOP.

    deserialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-attributes
      iv_longtext_id   = c_longtext_id-attributes ).

    deserialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-methods
      iv_longtext_id   = c_longtext_id-methods ).

    deserialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-events
      iv_longtext_id   = c_longtext_id-events ).

    deserialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-types
      iv_longtext_id   = c_longtext_id-types ).

  ENDMETHOD.


  METHOD deserialize_pre_ddic.

    DATA: ls_vseoclass TYPE vseoclass.

    ii_xml->read( EXPORTING iv_name = 'VSEOCLASS'
                  CHANGING  cg_data = ls_vseoclass ).

    mi_object_oriented_object_fct->create(
      EXPORTING
        iv_check      = abap_false
        iv_package    = iv_package
      CHANGING
        cg_properties = ls_vseoclass ).

  ENDMETHOD.


  METHOD deserialize_sotr.
    "OTR stands for Online Text Repository
    mi_object_oriented_object_fct->create_sotr(
      iv_object_name = ms_item-obj_name
      iv_package     = iv_package
      ii_xml         = ii_xml ).
  ENDMETHOD.


  METHOD deserialize_tpool.

    DATA: lv_clsname   TYPE seoclsname,
          lt_tpool_ext TYPE zif_abapgit_definitions=>ty_tpool_tt,
          lt_tpool     TYPE textpool_table.

    ii_xml->read( EXPORTING iv_name = 'TPOOL'
                  CHANGING cg_data = lt_tpool_ext ).
    lt_tpool = read_tpool( lt_tpool_ext ).

    IF lines( lt_tpool ) = 0.
      RETURN.
    ENDIF.

    lv_clsname = ms_item-obj_name.

    mi_object_oriented_object_fct->insert_text_pool(
      iv_class_name = lv_clsname
      it_text_pool  = lt_tpool
      iv_language   = mv_language ).

  ENDMETHOD.


  METHOD deserialize_tpool_i18n.

    DATA: lv_clsname    TYPE seoclsname,
          lt_tpool      TYPE textpool_table,
          lt_i18n_tpool TYPE zif_abapgit_lang_definitions=>ty_i18n_tpools,
          ls_i18n_tpool TYPE zif_abapgit_lang_definitions=>ty_i18n_tpool.

    lv_clsname = ms_item-obj_name.

    ii_xml->read( EXPORTING iv_name = 'I18N_TPOOL'
                  CHANGING  cg_data = lt_i18n_tpool ).

    zcl_abapgit_lxe_texts=>trim_tab_w_saplang_by_iso(
      EXPORTING
        it_iso_filter = ii_xml->i18n_params( )-translation_languages
        iv_lang_field_name = 'LANGUAGE'
      CHANGING
        ct_tab = lt_i18n_tpool ).

    LOOP AT lt_i18n_tpool INTO ls_i18n_tpool.
      lt_tpool = read_tpool( ls_i18n_tpool-textpool ).
      mi_object_oriented_object_fct->insert_text_pool(
        iv_class_name = lv_clsname
        it_text_pool  = lt_tpool
        iv_language   = ls_i18n_tpool-language
        iv_state      = 'A' ).
    ENDLOOP.

  ENDMETHOD.


  METHOD interface_replacement.

    DATA lv_tabix TYPE sy-tabix.

    FIELD-SYMBOLS <lv_source> LIKE LINE OF ct_source.

    FIND REGEX '^\s*INTERFACES(:| )\s*' && iv_from_interface && '\s*.' IN TABLE ct_source MATCH LINE lv_tabix.
    IF sy-subrc = 0.
      READ TABLE ct_source ASSIGNING <lv_source> INDEX lv_tabix.
      ASSERT sy-subrc = 0.

      REPLACE FIRST OCCURRENCE OF iv_from_interface IN <lv_source>
                             WITH iv_to_interface IGNORING CASE.

      REPLACE ALL OCCURRENCES OF iv_from_interface && '~descriptor' IN TABLE ct_source
                            WITH iv_to_interface && '~descriptor' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF iv_from_interface && '=>' IN TABLE ct_source
                            WITH iv_to_interface && '=>' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF iv_from_interface && '->' IN TABLE ct_source
                            WITH iv_to_interface && '->' IGNORING CASE.
    ENDIF.

  ENDMETHOD.


  METHOD is_class_locked.

    DATA: lv_argument TYPE seqg3-garg.

    lv_argument = ms_item-obj_name.
    OVERLAY lv_argument WITH '=============================='.
    lv_argument = lv_argument && '*'.

    rv_is_class_locked = exists_a_lock_entry_for( iv_lock_object = 'ESEOCLASS'
                                                  iv_argument    = lv_argument ).

  ENDMETHOD.


  METHOD repo_apack_replacement.

    DATA lv_apack TYPE seoclsname.

    " Check if SAP-version of APACK manifest exists
    SELECT SINGLE clsname INTO lv_apack
      FROM seoclass
      WHERE clsname = zif_abapgit_apack_definitions=>c_apack_interface_sap.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    " If not, replace with abapGit version
    interface_replacement(
      EXPORTING
        iv_from_interface = to_lower( zif_abapgit_apack_definitions=>c_apack_interface_sap )
        iv_to_interface   = to_lower( zif_abapgit_apack_definitions=>c_apack_interface_cust )
      CHANGING
        ct_source         = ct_source ).

  ENDMETHOD.


  METHOD serialize_attr.

    DATA: lt_attributes TYPE zif_abapgit_definitions=>ty_obj_attribute_tt.

    lt_attributes = mi_object_oriented_object_fct->read_attributes( iv_clsname ).
    IF lines( lt_attributes ) = 0.
      RETURN.
    ENDIF.

    ii_xml->add( iv_name = 'ATTRIBUTES'
                 ig_data = lt_attributes ).

  ENDMETHOD.


  METHOD serialize_descr.

    DATA: lt_descriptions    TYPE zif_abapgit_oo_object_fnc=>ty_seocompotx_tt,
          lv_language        TYPE spras,
          lt_language_filter TYPE zif_abapgit_environment=>ty_system_language_filter.

    IF ii_xml->i18n_params( )-main_language_only = abap_true.
      lv_language = mv_language.
    ENDIF.

    lt_descriptions = mi_object_oriented_object_fct->read_descriptions(
      iv_object_name = iv_clsname
      iv_language    = lv_language ).

    IF lines( lt_descriptions ) = 0.
      RETURN.
    ENDIF.
    " Remove technical languages
    lt_language_filter = zcl_abapgit_factory=>get_environment( )->get_system_language_filter( ).
    DELETE lt_descriptions WHERE NOT langu IN lt_language_filter.

    ii_xml->add( iv_name = 'DESCRIPTIONS'
                 ig_data = lt_descriptions ).

  ENDMETHOD.


  METHOD serialize_descr_sub.

    DATA: lt_descriptions    TYPE zif_abapgit_oo_object_fnc=>ty_seosubcotx_tt,
          lv_language        TYPE spras,
          lt_language_filter TYPE zif_abapgit_environment=>ty_system_language_filter.

    IF ii_xml->i18n_params( )-main_language_only = abap_true.
      lv_language = mv_language.
    ENDIF.

    lt_descriptions = mi_object_oriented_object_fct->read_descriptions_sub(
      iv_object_name = iv_clsname
      iv_language    = lv_language ).

    IF lines( lt_descriptions ) = 0.
      RETURN.
    ENDIF.
    " Remove technical languages
    lt_language_filter = zcl_abapgit_factory=>get_environment( )->get_system_language_filter( ).
    DELETE lt_descriptions WHERE NOT langu IN lt_language_filter.

    ii_xml->add( iv_name = 'DESCRIPTIONS_SUB'
                 ig_data = lt_descriptions ).

  ENDMETHOD.


  METHOD serialize_docu.

    DATA: lt_lines      TYPE tlinetab,
          lv_object     TYPE dokhl-object,
          lv_langu      TYPE sy-langu,
          lt_i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_lines,
          ls_i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_line.

    lv_object = iv_clsname.

    lt_lines = mi_object_oriented_object_fct->read_documentation(
      iv_id          = c_longtext_id-class
      iv_object_name = lv_object
      iv_language    = mv_language ).
    IF lines( lt_lines ) > 0.
      ii_xml->add( iv_name = 'LINES'
                   ig_data = lt_lines ).
    ENDIF.

    IF ii_xml->i18n_params( )-main_language_only = abap_true.
      RETURN.
    ENDIF.

    LOOP AT it_langu_additional INTO lv_langu.

      lt_lines = mi_object_oriented_object_fct->read_documentation(
        iv_id          = c_longtext_id-class
        iv_object_name = lv_object
        iv_language    = lv_langu ).

      IF lines( lt_lines ) > 0.
        CLEAR ls_i18n_lines.
        ls_i18n_lines-language = lv_langu.
        ls_i18n_lines-lines    = lt_lines.
        INSERT ls_i18n_lines INTO TABLE lt_i18n_lines.
      ENDIF.

    ENDLOOP.

    IF lines( lt_i18n_lines ) > 0.
      ii_xml->add( iv_name = 'I18N_LINES'
                   ig_data = lt_i18n_lines ).
    ENDIF.

    serialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-attributes
      iv_longtext_id   = c_longtext_id-attributes ).

    serialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-methods
      iv_longtext_id   = c_longtext_id-methods ).

    serialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-events
      iv_longtext_id   = c_longtext_id-events ).

    serialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-types
      iv_longtext_id   = c_longtext_id-types ).

  ENDMETHOD.


  METHOD serialize_sotr.
    mi_object_oriented_object_fct->read_sotr(
      iv_object_name = ms_item-obj_name
      ii_xml         = ii_xml ).
  ENDMETHOD.


  METHOD serialize_tpool.

    DATA lt_tpool TYPE textpool_table.

    lt_tpool = mi_object_oriented_object_fct->read_text_pool(
      iv_class_name = iv_clsname
      iv_language   = mv_language ).
    ii_xml->add( iv_name = 'TPOOL'
                 ig_data = add_tpool( lt_tpool ) ).

    rt_tpool = lt_tpool.

  ENDMETHOD.


  METHOD serialize_tpool_i18n.

    DATA: lt_tpool      TYPE textpool_table,
          lv_index      TYPE i,
          lv_langu      TYPE sy-langu,
          lt_i18n_tpool TYPE zif_abapgit_lang_definitions=>ty_i18n_tpools,
          ls_i18n_tpool TYPE zif_abapgit_lang_definitions=>ty_i18n_tpool.

    FIELD-SYMBOLS <ls_tpool> LIKE LINE OF it_tpool_main.

    DATA lt_tpool_main LIKE SORTED TABLE OF <ls_tpool> WITH UNIQUE KEY id key.

    IF ii_xml->i18n_params( )-main_language_only = abap_true OR lines( it_tpool_main ) = 0.
      RETURN.
    ENDIF.

    " Copy single records to be able to catch duplicate key error
    LOOP AT it_tpool_main ASSIGNING <ls_tpool>.
      INSERT <ls_tpool> INTO TABLE lt_tpool_main.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Inconsistent textpool in { ms_item-obj_type } { ms_item-obj_name }| ).
      ENDIF.
    ENDLOOP.

    LOOP AT it_langu_additional INTO lv_langu.

      lt_tpool = mi_object_oriented_object_fct->read_text_pool(
        iv_class_name = iv_clsname
        iv_language   = lv_langu ).

      LOOP AT lt_tpool ASSIGNING <ls_tpool>.
        lv_index = sy-tabix.
        READ TABLE lt_tpool_main WITH KEY id = <ls_tpool>-id key = <ls_tpool>-key
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          DELETE lt_tpool INDEX lv_index.
        ENDIF.
      ENDLOOP.

      IF lines( lt_tpool ) > 0.
        CLEAR ls_i18n_tpool.
        ls_i18n_tpool-language = lv_langu.
        ls_i18n_tpool-textpool = add_tpool( lt_tpool ).
        INSERT ls_i18n_tpool INTO TABLE lt_i18n_tpool.
      ENDIF.

    ENDLOOP.

    IF lines( lt_i18n_tpool ) > 0.
      ii_xml->add( iv_name = 'I18N_TPOOL'
                   ig_data = lt_i18n_tpool ).
    ENDIF.

  ENDMETHOD.


  METHOD serialize_xml.

    DATA: ls_vseoclass        TYPE vseoclass,
          lt_tpool            TYPE textpool_table,
          ls_clskey           TYPE seoclskey,
          lt_langu_additional TYPE zif_abapgit_lang_definitions=>ty_langus,
          lt_language_filter  TYPE zif_abapgit_environment=>ty_system_language_filter.

    ls_clskey-clsname = ms_item-obj_name.

    "If class was deserialized with a previous versions of abapGit and current language was different
    "from main language at this time, this call would return SY-LANGU as main language. To fix
    "these objects, set SY-LANGU to main language temporarily.
    zcl_abapgit_language=>set_current_language( mv_language ).

    TRY.
        ls_vseoclass = mi_object_oriented_object_fct->get_class_properties( ls_clskey ).

      CLEANUP.
        zcl_abapgit_language=>restore_login_language( ).

    ENDTRY.

    zcl_abapgit_language=>restore_login_language( ).

    IF mv_skip_testclass = abap_true.
      CLEAR ls_vseoclass-with_unit_tests.
    ENDIF.

    " Table d010tinf stores info. on languages in which program is maintained
    " Select all active translations of program texts
    " Skip main language - it was already serialized
    lt_language_filter = zcl_abapgit_factory=>get_environment( )->get_system_language_filter( ).

    zcl_abapgit_lxe_texts=>add_iso_langs_to_lang_filter(
      EXPORTING it_iso_filter      = ii_xml->i18n_params( )-translation_languages
      CHANGING  ct_language_filter = lt_language_filter ).

    SELECT DISTINCT language
      INTO TABLE lt_langu_additional
      FROM d010tinf
      WHERE r3state  = 'A'
        AND prog     = mv_classpool_name
        AND language IN lt_language_filter
        AND language <> mv_language.

    ii_xml->add( iv_name = 'VSEOCLASS'
                 ig_data = ls_vseoclass ).

    lt_tpool = serialize_tpool(
      ii_xml     = ii_xml
      iv_clsname = ls_clskey-clsname ).

    IF ii_xml->i18n_params( )-translation_languages IS INITIAL OR ii_xml->i18n_params( )-use_lxe = abap_false.
      serialize_tpool_i18n(
        ii_xml              = ii_xml
        it_langu_additional = lt_langu_additional
        it_tpool_main       = lt_tpool
        iv_clsname          = ls_clskey-clsname ).
    ELSE.
      serialize_lxe_texts( ii_xml ).
    ENDIF.

    IF ls_vseoclass-category = seoc_category_exception.
      serialize_sotr( ii_xml ).
    ENDIF.

    SELECT DISTINCT langu
      INTO TABLE lt_langu_additional
      FROM dokhl
      WHERE id     = 'CL'
        AND object = ls_clskey-clsname
        AND langu IN lt_language_filter
        AND langu <> mv_language.

    serialize_docu( ii_xml              = ii_xml
                    iv_clsname          = ls_clskey-clsname
                    it_langu_additional = lt_langu_additional ).

    serialize_descr( ii_xml     = ii_xml
                     iv_clsname = ls_clskey-clsname ).

    serialize_descr_sub( ii_xml     = ii_xml
                         iv_clsname = ls_clskey-clsname ).

    serialize_attr( ii_xml     = ii_xml
                    iv_clsname = ls_clskey-clsname ).

  ENDMETHOD.


  METHOD source_apack_replacement.

    DATA lv_clsname TYPE seoclsname.

    " Check if abapGit version of APACK manifest is used
    SELECT SINGLE clsname INTO lv_clsname
      FROM seometarel
      WHERE clsname    = ms_item-obj_name
        AND refclsname = zif_abapgit_apack_definitions=>c_apack_interface_cust
        AND version    = '1'.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " If yes, replace with SAP-version
    interface_replacement(
      EXPORTING
        iv_from_interface = to_lower( zif_abapgit_apack_definitions=>c_apack_interface_cust )
        iv_to_interface   = to_lower( zif_abapgit_apack_definitions=>c_apack_interface_sap )
      CHANGING
        ct_source         = ct_source ).

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    TYPES: BEGIN OF ty_reposrc,
             unam  TYPE reposrc-unam,
             udat  TYPE reposrc-udat,
             utime TYPE reposrc-utime,
           END OF ty_reposrc.

    DATA: lt_reposrc  TYPE STANDARD TABLE OF ty_reposrc,
          ls_reposrc  LIKE LINE OF lt_reposrc,
          lv_include  TYPE programm,
          lt_includes TYPE STANDARD TABLE OF programm.

    CASE iv_extra.
      WHEN zif_abapgit_oo_object_fnc=>c_parts-locals_def.
        lv_include = cl_oo_classname_service=>get_ccdef_name( |{ ms_item-obj_name }| ).
        INSERT lv_include INTO TABLE lt_includes.
      WHEN zif_abapgit_oo_object_fnc=>c_parts-locals_imp.
        lv_include = cl_oo_classname_service=>get_ccimp_name( |{ ms_item-obj_name }| ).
        INSERT lv_include INTO TABLE lt_includes.
      WHEN zif_abapgit_oo_object_fnc=>c_parts-macros.
        lv_include = cl_oo_classname_service=>get_ccmac_name( |{ ms_item-obj_name }| ).
        INSERT lv_include INTO TABLE lt_includes.
      WHEN zif_abapgit_oo_object_fnc=>c_parts-testclasses.
        lv_include = cl_oo_classname_service=>get_ccau_name( |{ ms_item-obj_name }| ).
        INSERT lv_include INTO TABLE lt_includes.
      WHEN OTHERS.
        lt_includes = mi_object_oriented_object_fct->get_includes( ms_item-obj_name ).
    ENDCASE.

    ASSERT lines( lt_includes ) > 0.

    SELECT unam udat utime FROM reposrc
      INTO TABLE lt_reposrc
      FOR ALL ENTRIES IN lt_includes
      WHERE progname = lt_includes-table_line
      AND r3state = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ELSE.
      SORT lt_reposrc BY udat DESCENDING utime DESCENDING.
      READ TABLE lt_reposrc INDEX 1 INTO ls_reposrc.
      ASSERT sy-subrc = 0.
      rv_user = ls_reposrc-unam.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    DATA: ls_clskey TYPE seoclskey.
    ls_clskey-clsname = ms_item-obj_name.

    corr_insert( iv_package ).

    mi_object_oriented_object_fct->delete( ls_clskey ).
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    IF iv_step = zif_abapgit_object=>gc_step_id-abap.

      deserialize_abap( ii_xml     = io_xml
                        iv_package = iv_package ).

      deserialize_tpool( io_xml ).

      IF io_xml->i18n_params( )-translation_languages IS INITIAL OR io_xml->i18n_params( )-use_lxe = abap_false.
        deserialize_tpool_i18n( io_xml ).
      ELSE.
        deserialize_lxe_texts( io_xml ).
      ENDIF.

      deserialize_sotr( ii_xml     = io_xml
                        iv_package = iv_package ).

      deserialize_docu( io_xml ).

    ELSEIF iv_step = zif_abapgit_object=>gc_step_id-early.

      " If class does not exist, create it
      " so DDIC that depends on it does not fail activation
      IF zif_abapgit_object~exists( ) = abap_false.
        deserialize_pre_ddic(
          ii_xml     = io_xml
          iv_package = iv_package ).
      ELSE.
        corr_insert( iv_package ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA ls_class_key TYPE seoclskey.

    ls_class_key-clsname = ms_item-obj_name.

    rv_bool = mi_object_oriented_object_fct->exists( ls_class_key ).

    " Skip classes generated by DDLS (SADL)
    IF rv_bool = abap_true AND
      mi_object_oriented_object_fct->read_superclass( ls_class_key-clsname ) = 'CL_SADL_GTK_EXPOSURE_MPC'.
      rv_bool = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-early TO rt_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    IF is_class_locked( ) = abap_true OR is_text_locked( mv_classpool_name ) = abap_true.
      rv_is_locked = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.

    ls_item-obj_type = 'PROG'.

    CASE iv_extra.
      WHEN zif_abapgit_oo_object_fnc=>c_parts-locals_def.
        ls_item-obj_name = cl_oo_classname_service=>get_ccdef_name( |{ ms_item-obj_name }| ).
      WHEN zif_abapgit_oo_object_fnc=>c_parts-locals_imp.
        ls_item-obj_name = cl_oo_classname_service=>get_ccimp_name( |{ ms_item-obj_name }| ).
      WHEN zif_abapgit_oo_object_fnc=>c_parts-macros.
        ls_item-obj_name = cl_oo_classname_service=>get_ccmac_name( |{ ms_item-obj_name }| ).
      WHEN zif_abapgit_oo_object_fnc=>c_parts-testclasses.
        ls_item-obj_name = cl_oo_classname_service=>get_ccau_name( |{ ms_item-obj_name }| ).
    ENDCASE.

    IF ls_item-obj_name IS NOT INITIAL.
      rv_exit = zcl_abapgit_ui_factory=>get_gui_jumper( )->jump( ls_item ).
    ENDIF.

    " Otherwise covered by ZCL_ABAPGIT_OBJECTS=>JUMP

  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lt_source    TYPE seop_source_string,
          ls_class_key TYPE seoclskey.

    ls_class_key-clsname = ms_item-obj_name.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_active
        force   = abap_true.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_inactive
        force   = abap_true.

    lt_source = mi_object_oriented_object_fct->serialize_abap( ls_class_key ).

    source_apack_replacement( CHANGING ct_source = lt_source ).

    zif_abapgit_object~mo_files->add_abap( lt_source ).

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key = ls_class_key
      iv_type      = seop_ext_class_locals_def ).
    IF lines( lt_source ) > 0.
      zif_abapgit_object~mo_files->add_abap(
        iv_extra = zif_abapgit_oo_object_fnc=>c_parts-locals_def
        it_abap  = lt_source ).
    ENDIF.

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key = ls_class_key
      iv_type      = seop_ext_class_locals_imp ).
    IF lines( lt_source ) > 0.
      zif_abapgit_object~mo_files->add_abap(
        iv_extra = zif_abapgit_oo_object_fnc=>c_parts-locals_imp
        it_abap  = lt_source ).
    ENDIF.

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key            = ls_class_key
      iv_type                 = seop_ext_class_testclasses ).

    mv_skip_testclass = mi_object_oriented_object_fct->get_skip_test_classes( ).
    IF lines( lt_source ) > 0 AND mv_skip_testclass = abap_false.
      zif_abapgit_object~mo_files->add_abap(
        iv_extra = zif_abapgit_oo_object_fnc=>c_parts-testclasses
        it_abap  = lt_source ).
    ENDIF.

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key = ls_class_key
      iv_type      = seop_ext_class_macros ).
    IF lines( lt_source ) > 0.
      zif_abapgit_object~mo_files->add_abap(
        iv_extra = zif_abapgit_oo_object_fnc=>c_parts-macros
        it_abap  = lt_source ).
    ENDIF.

    serialize_xml( io_xml ).

  ENDMETHOD.
ENDCLASS.
