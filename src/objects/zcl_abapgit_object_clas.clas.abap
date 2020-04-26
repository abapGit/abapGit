CLASS zcl_abapgit_object_clas DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_program.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.
    METHODS: constructor
      IMPORTING
        is_item     TYPE zif_abapgit_definitions=>ty_item
        iv_language TYPE spras.

  PROTECTED SECTION.
    DATA: mi_object_oriented_object_fct TYPE REF TO zif_abapgit_oo_object_fnc,
          mv_skip_testclass             TYPE abap_bool,
          mv_classpool_name             TYPE progname.
    METHODS:
      deserialize_abap
        IMPORTING io_xml     TYPE REF TO zcl_abapgit_xml_input
                  iv_package TYPE devclass
        RAISING   zcx_abapgit_exception,
      deserialize_docu
        IMPORTING io_xml TYPE REF TO zcl_abapgit_xml_input
        RAISING   zcx_abapgit_exception,
      deserialize_tpool
        IMPORTING io_xml TYPE REF TO zcl_abapgit_xml_input
        RAISING   zcx_abapgit_exception,
      deserialize_sotr
        IMPORTING io_xml     TYPE REF TO zcl_abapgit_xml_input
                  iv_package TYPE devclass
        RAISING   zcx_abapgit_exception,
      serialize_xml
        IMPORTING io_xml TYPE REF TO zcl_abapgit_xml_output
        RAISING   zcx_abapgit_exception,
      serialize_attr
        IMPORTING
          !io_xml     TYPE REF TO zcl_abapgit_xml_output
          !iv_clsname TYPE seoclsname
        RAISING
          zcx_abapgit_exception,
      serialize_descr
        IMPORTING
          !io_xml     TYPE REF TO zcl_abapgit_xml_output
          !iv_clsname TYPE seoclsname
        RAISING
          zcx_abapgit_exception,
      serialize_docu
        IMPORTING
          !io_xml              TYPE REF TO zcl_abapgit_xml_output
          !it_langu_additional TYPE zif_abapgit_lang_definitions=>tt_langu OPTIONAL
          !iv_clsname          TYPE seoclsname
        RAISING
          zcx_abapgit_exception,
      serialize_tpool
        IMPORTING
          !io_xml              TYPE REF TO zcl_abapgit_xml_output
          !it_langu_additional TYPE zif_abapgit_lang_definitions=>tt_langu OPTIONAL
          !iv_clsname          TYPE seoclsname
        RAISING
          zcx_abapgit_exception,
      serialize_sotr
        IMPORTING
          !io_xml TYPE REF TO zcl_abapgit_xml_output
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
    METHODS:
      is_class_locked
        RETURNING VALUE(rv_is_class_locked) TYPE abap_bool
        RAISING   zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_object_clas IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    CREATE OBJECT mi_object_oriented_object_fct TYPE zcl_abapgit_oo_class.

    mv_classpool_name = cl_oo_classname_service=>get_classpool_name( |{ is_item-obj_name }| ).

  ENDMETHOD.


  METHOD deserialize_abap.
* same as in zcl_abapgit_object_clas, but without "mo_object_oriented_object_fct->add_to_activation_list"

    DATA: ls_vseoclass             TYPE vseoclass,
          lt_source                TYPE seop_source_string,
          lt_local_definitions     TYPE seop_source_string,
          lt_local_implementations TYPE seop_source_string,
          lt_local_macros          TYPE seop_source_string,
          lt_test_classes          TYPE seop_source_string,
          lt_descriptions          TYPE zif_abapgit_definitions=>ty_seocompotx_tt,
          ls_class_key             TYPE seoclskey,
          lt_attributes            TYPE zif_abapgit_definitions=>ty_obj_attribute_tt.


    lt_source = mo_files->read_abap( ).

    lt_local_definitions = mo_files->read_abap( iv_extra = 'locals_def'
                                                iv_error = abap_false ). "#EC NOTEXT

    lt_local_implementations = mo_files->read_abap( iv_extra = 'locals_imp'
                                                    iv_error = abap_false ). "#EC NOTEXT

    lt_local_macros = mo_files->read_abap( iv_extra = 'macros'
                                           iv_error = abap_false ). "#EC NOTEXT

    lt_test_classes = mo_files->read_abap( iv_extra = 'testclasses'
                                           iv_error = abap_false ). "#EC NOTEXT

    ls_class_key-clsname = ms_item-obj_name.

    io_xml->read( EXPORTING iv_name = 'VSEOCLASS'
                  CHANGING  cg_data = ls_vseoclass ).

    io_xml->read( EXPORTING iv_name = 'ATTRIBUTES'
                  CHANGING  cg_data = lt_attributes ).

    mi_object_oriented_object_fct->create(
      EXPORTING
        iv_package    = iv_package
        it_attributes = lt_attributes
      CHANGING
        cg_properties = ls_vseoclass ).

    mi_object_oriented_object_fct->generate_locals(
      is_key                   = ls_class_key
      iv_force                 = abap_true
      it_local_definitions     = lt_local_definitions
      it_local_implementations = lt_local_implementations
      it_local_macros          = lt_local_macros
      it_local_test_classes    = lt_test_classes ).

    repo_apack_replacement( CHANGING ct_source = lt_source ).
    mi_object_oriented_object_fct->deserialize_source(
      is_key    = ls_class_key
      it_source = lt_source ).

    io_xml->read( EXPORTING iv_name = 'DESCRIPTIONS'
                  CHANGING cg_data = lt_descriptions ).

    mi_object_oriented_object_fct->update_descriptions(
      is_key          = ls_class_key
      it_descriptions = lt_descriptions ).

  ENDMETHOD.


  METHOD deserialize_docu.

    DATA: lt_lines      TYPE tlinetab,
          lv_object     TYPE dokhl-object,
          lt_i18n_lines TYPE zif_abapgit_lang_definitions=>tt_i18n_lines,
          ls_i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_lines.

    io_xml->read( EXPORTING iv_name = 'LINES'
                  CHANGING cg_data = lt_lines ).

    IF lines( lt_lines ) = 0.
      RETURN.
    ENDIF.

    lv_object = ms_item-obj_name.

    mi_object_oriented_object_fct->create_documentation(
      it_lines       = lt_lines
      iv_object_name = lv_object
      iv_language    = mv_language ).

    io_xml->read( EXPORTING iv_name = 'I18N_LINES'
                  CHANGING cg_data = lt_i18n_lines ).

    LOOP AT lt_i18n_lines INTO ls_i18n_lines.
      mi_object_oriented_object_fct->create_documentation(
        it_lines         = ls_i18n_lines-lines
        iv_object_name   = lv_object
        iv_language      = ls_i18n_lines-language
        iv_no_masterlang = abap_true ).
    ENDLOOP.

  ENDMETHOD.


  METHOD deserialize_sotr.
    "OTR stands for Online Text Repository
    DATA: lt_sotr    TYPE zif_abapgit_definitions=>ty_sotr_tt.

    io_xml->read( EXPORTING iv_name = 'SOTR'
                  CHANGING cg_data = lt_sotr ).

    IF lines( lt_sotr ) = 0.
      RETURN.
    ENDIF.

    mi_object_oriented_object_fct->create_sotr(
      iv_package    = iv_package
      it_sotr       = lt_sotr ).
  ENDMETHOD.


  METHOD deserialize_tpool.

    DATA: lv_clsname    TYPE seoclsname,
          lt_tpool_ext  TYPE zif_abapgit_definitions=>ty_tpool_tt,
          lt_tpool      TYPE textpool_table,
          lt_i18n_tpool TYPE zif_abapgit_lang_definitions=>tt_i18n_tpool,
          ls_i18n_tpool TYPE zif_abapgit_lang_definitions=>ty_i18n_tpool.


    io_xml->read( EXPORTING iv_name = 'TPOOL'
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

    io_xml->read( EXPORTING iv_name = 'I18N_TPOOL'
                  CHANGING  cg_data = lt_i18n_tpool ).

    LOOP AT lt_i18n_tpool INTO ls_i18n_tpool.
      lt_tpool = read_tpool( ls_i18n_tpool-textpool ).
      mi_object_oriented_object_fct->insert_text_pool(
        iv_class_name = lv_clsname
        it_text_pool  = lt_tpool
        iv_language   = ls_i18n_tpool-language
        iv_state      = 'A' ).
    ENDLOOP.

  ENDMETHOD.


  METHOD is_class_locked.

    DATA: lv_argument TYPE seqg3-garg.

    lv_argument = ms_item-obj_name.
    OVERLAY lv_argument WITH '=============================='.
    lv_argument = lv_argument && '*'.

    rv_is_class_locked = exists_a_lock_entry_for( iv_lock_object = 'ESEOCLASS'
                                                  iv_argument    = lv_argument ).

  ENDMETHOD.


  METHOD serialize_xml.

    DATA: ls_vseoclass        TYPE vseoclass,
          ls_clskey           TYPE seoclskey,
          lt_langu_additional TYPE zif_abapgit_lang_definitions=>tt_langu.

    ls_clskey-clsname = ms_item-obj_name.

    "If class was deserialized with a previous versions of abapGit and current language was different
    "from master language at this time, this call would return SY-LANGU as master language. To fix
    "these objects, set SY-LANGU to master language temporarily.
    zcl_abapgit_language=>set_current_language( mv_language ).

    TRY.
        ls_vseoclass = mi_object_oriented_object_fct->get_class_properties( ls_clskey ).

      CLEANUP.
        zcl_abapgit_language=>restore_login_language( ).

    ENDTRY.

    zcl_abapgit_language=>restore_login_language( ).

    CLEAR: ls_vseoclass-uuid,
           ls_vseoclass-author,
           ls_vseoclass-createdon,
           ls_vseoclass-changedby,
           ls_vseoclass-changedon,
           ls_vseoclass-r3release,
           ls_vseoclass-chgdanyby,
           ls_vseoclass-chgdanyon,
           ls_vseoclass-clsfinal,
           ls_vseoclass-clsabstrct,
           ls_vseoclass-exposure,
           ls_vseoclass-version.

    IF mv_skip_testclass = abap_true.
      CLEAR ls_vseoclass-with_unit_tests.
    ENDIF.

    " Table d010tinf stores info. on languages in which program is maintained
    " Select all active translations of program texts
    " Skip master language - it was already serialized
    SELECT DISTINCT language
      INTO TABLE lt_langu_additional
      FROM d010tinf
      WHERE r3state  = 'A'
        AND prog     = mv_classpool_name
        AND language <> mv_language.

    io_xml->add( iv_name = 'VSEOCLASS'
                 ig_data = ls_vseoclass ).

    serialize_tpool( io_xml              = io_xml
                     iv_clsname          = ls_clskey-clsname
                     it_langu_additional = lt_langu_additional ).

    IF ls_vseoclass-category = seoc_category_exception.
      serialize_sotr( io_xml ).
    ENDIF.

    serialize_docu( io_xml              = io_xml
                    iv_clsname          = ls_clskey-clsname
                    it_langu_additional = lt_langu_additional ).

    serialize_descr( io_xml     = io_xml
                     iv_clsname = ls_clskey-clsname ).

    serialize_attr( io_xml     = io_xml
                    iv_clsname = ls_clskey-clsname ).

  ENDMETHOD.


  METHOD serialize_attr.

    DATA: lt_attributes TYPE zif_abapgit_definitions=>ty_obj_attribute_tt.

    lt_attributes = mi_object_oriented_object_fct->read_attributes( iv_clsname ).
    IF lines( lt_attributes ) = 0.
      RETURN.
    ENDIF.

    io_xml->add( iv_name = 'ATTRIBUTES'
                 ig_data = lt_attributes ).

  ENDMETHOD.


  METHOD serialize_descr.

    DATA: lt_descriptions TYPE zif_abapgit_definitions=>ty_seocompotx_tt.

    lt_descriptions = mi_object_oriented_object_fct->read_descriptions( iv_clsname ).
    IF lines( lt_descriptions ) = 0.
      RETURN.
    ENDIF.

    io_xml->add( iv_name = 'DESCRIPTIONS'
                 ig_data = lt_descriptions ).

  ENDMETHOD.


  METHOD serialize_docu.

    DATA: lt_lines      TYPE tlinetab,
          lv_langu      TYPE langu,
          lt_i18n_lines TYPE zif_abapgit_lang_definitions=>tt_i18n_lines,
          ls_i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_lines.

    lt_lines = mi_object_oriented_object_fct->read_documentation(
      iv_class_name = iv_clsname
      iv_language   = mv_language ).
    IF lines( lt_lines ) > 0.
      io_xml->add( iv_name = 'LINES'
                   ig_data = lt_lines ).
    ENDIF.

    IF io_xml->i18n_params( )-serialize_master_lang_only IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT it_langu_additional INTO lv_langu.
      CLEAR: ls_i18n_lines.

      lt_lines = mi_object_oriented_object_fct->read_documentation(
        iv_class_name = iv_clsname
        iv_language   = lv_langu ).

      ls_i18n_lines-language = lv_langu.
      ls_i18n_lines-lines    = lt_lines.
      INSERT ls_i18n_lines INTO TABLE lt_i18n_lines.

    ENDLOOP.

    IF lines( lt_i18n_lines ) > 0.
      io_xml->add( iv_name = 'I18N_LINES'
                   ig_data = lt_i18n_lines ).
    ENDIF.

  ENDMETHOD.


  METHOD serialize_tpool.

    DATA: lt_tpool      TYPE textpool_table,
          lv_langu      TYPE langu,
          lt_i18n_tpool TYPE zif_abapgit_lang_definitions=>tt_i18n_tpool,
          ls_i18n_tpool TYPE zif_abapgit_lang_definitions=>ty_i18n_tpool.

    lt_tpool = mi_object_oriented_object_fct->read_text_pool(
      iv_class_name = iv_clsname
      iv_language   = mv_language ).
    io_xml->add( iv_name = 'TPOOL'
                 ig_data = add_tpool( lt_tpool ) ).

    IF io_xml->i18n_params( )-serialize_master_lang_only IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT it_langu_additional INTO lv_langu.
      CLEAR: ls_i18n_tpool.

      lt_tpool = mi_object_oriented_object_fct->read_text_pool(
            iv_class_name = iv_clsname
            iv_language   = lv_langu ).

      ls_i18n_tpool-language = lv_langu.
      ls_i18n_tpool-textpool = add_tpool( lt_tpool ).
      INSERT ls_i18n_tpool INTO TABLE lt_i18n_tpool.

    ENDLOOP.

    IF lines( lt_i18n_tpool ) > 0.
      io_xml->add( iv_name = 'I18N_TPOOL'
                   ig_data = lt_i18n_tpool ).
    ENDIF.

  ENDMETHOD.


  METHOD serialize_sotr.

    DATA: lt_sotr TYPE zif_abapgit_definitions=>ty_sotr_tt.

    lt_sotr = mi_object_oriented_object_fct->read_sotr( ms_item-obj_name ).
    IF lines( lt_sotr ) = 0.
      RETURN.
    ENDIF.

    io_xml->add( iv_name = 'SOTR'
                 ig_data = lt_sotr ).

  ENDMETHOD.


  METHOD source_apack_replacement.

    FIELD-SYMBOLS: <lv_source> LIKE LINE OF ct_source.

    LOOP AT ct_source ASSIGNING <lv_source>.

      FIND FIRST OCCURRENCE OF REGEX '^\s*INTERFACES(:| )\s*zif_apack_manifest\s*.' IN <lv_source>.
      IF sy-subrc = 0.
        REPLACE FIRST OCCURRENCE OF 'zif_apack_manifest' IN <lv_source> WITH 'if_apack_manifest' IGNORING CASE.

        REPLACE ALL OCCURRENCES OF 'zif_apack_manifest~descriptor' IN TABLE ct_source
                              WITH 'if_apack_manifest~descriptor' IGNORING CASE.

        EXIT.
      ENDIF.

      FIND FIRST OCCURRENCE OF REGEX '^\s*PROTECTED\s*SECTION\s*.' IN <lv_source>.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD repo_apack_replacement.

    FIELD-SYMBOLS: <lv_source> LIKE LINE OF ct_source.

    LOOP AT ct_source ASSIGNING <lv_source>.

      FIND FIRST OCCURRENCE OF REGEX '^\s*INTERFACES(:| )\s*if_apack_manifest\s*.' IN <lv_source>.
      IF sy-subrc = 0.
        REPLACE FIRST OCCURRENCE OF 'if_apack_manifest' IN <lv_source> WITH 'zif_apack_manifest' IGNORING CASE.

        REPLACE ALL OCCURRENCES OF 'if_apack_manifest~descriptor' IN TABLE ct_source
                              WITH 'zif_apack_manifest~descriptor' IGNORING CASE.

        EXIT.
      ENDIF.

      FIND FIRST OCCURRENCE OF REGEX '^\s*PROTECTED\s*SECTION\s*.' IN <lv_source>.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    TYPES: BEGIN OF ty_includes,
             programm TYPE programm,
           END OF ty_includes.

    TYPES: BEGIN OF ty_reposrc,
             unam  TYPE reposrc-unam,
             udat  TYPE reposrc-udat,
             utime TYPE reposrc-utime,
           END OF ty_reposrc.

    DATA: lt_reposrc  TYPE STANDARD TABLE OF ty_reposrc,
          ls_reposrc  LIKE LINE OF lt_reposrc,
          lt_includes TYPE STANDARD TABLE OF ty_includes.

    lt_includes = mi_object_oriented_object_fct->get_includes( ms_item-obj_name ).
    ASSERT lines( lt_includes ) > 0.

    SELECT unam udat utime FROM reposrc
      INTO TABLE lt_reposrc
      FOR ALL ENTRIES IN lt_includes
      WHERE progname = lt_includes-programm
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

    mi_object_oriented_object_fct->delete( ls_clskey ).
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.
    deserialize_abap( io_xml     = io_xml
                      iv_package = iv_package ).

    deserialize_tpool( io_xml ).

    deserialize_sotr( io_xml     = io_xml
                      iv_package = iv_package ).

    deserialize_docu( io_xml ).
  ENDMETHOD.


  METHOD zif_abapgit_object~exists.
    DATA: ls_class_key TYPE seoclskey.
    ls_class_key-clsname = ms_item-obj_name.

    rv_bool = mi_object_oriented_object_fct->exists( ls_class_key ).
  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
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
    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'CLAS'
        in_new_window = abap_true.
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

    mo_files->add_abap( lt_source ).

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key = ls_class_key
      iv_type      = seop_ext_class_locals_def ).
    IF lines( lt_source ) > 0.
      mo_files->add_abap( iv_extra = 'locals_def'
                          it_abap  = lt_source ).           "#EC NOTEXT
    ENDIF.

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key = ls_class_key
      iv_type      = seop_ext_class_locals_imp ).
    IF lines( lt_source ) > 0.
      mo_files->add_abap( iv_extra = 'locals_imp'
                          it_abap  = lt_source ).           "#EC NOTEXT
    ENDIF.

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key            = ls_class_key
      iv_type                 = seop_ext_class_testclasses ).

    mv_skip_testclass = mi_object_oriented_object_fct->get_skip_test_classes( ).
    IF lines( lt_source ) > 0 AND mv_skip_testclass = abap_false.
      mo_files->add_abap( iv_extra = 'testclasses'
                          it_abap  = lt_source ).           "#EC NOTEXT
    ENDIF.

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key = ls_class_key
      iv_type      = seop_ext_class_macros ).
    IF lines( lt_source ) > 0.
      mo_files->add_abap( iv_extra = 'macros'
                          it_abap  = lt_source ).           "#EC NOTEXT
    ENDIF.

    serialize_xml( io_xml ).

  ENDMETHOD.


ENDCLASS.
