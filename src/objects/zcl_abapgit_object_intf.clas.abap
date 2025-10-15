CLASS zcl_abapgit_object_intf DEFINITION PUBLIC FINAL INHERITING FROM zcl_abapgit_objects_program.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    TYPES:
      BEGIN OF ty_docu,
        lines      TYPE tlinetab,
        i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_lines,
      END OF ty_docu.

    TYPES:
      BEGIN OF ty_intf,
        vseointerf      TYPE vseointerf,
        docu            TYPE ty_docu,
        description_int TYPE zif_abapgit_oo_object_fnc=>ty_seoclasstx_tt,
        description     TYPE zif_abapgit_oo_object_fnc=>ty_seocompotx_tt,
        description_sub TYPE zif_abapgit_oo_object_fnc=>ty_seosubcotx_tt,
      END OF ty_intf.

    METHODS constructor
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !iv_language    TYPE spras
        !io_files       TYPE REF TO zcl_abapgit_objects_files OPTIONAL
        !io_i18n_params TYPE REF TO zcl_abapgit_i18n_params OPTIONAL
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
    METHODS deserialize_proxy
      IMPORTING
        iv_transport TYPE trkorr
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_docu
      IMPORTING
        !ii_xml  TYPE REF TO zif_abapgit_xml_input
        !is_docu TYPE ty_docu
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_docu
      IMPORTING
                !it_langu_additional TYPE zif_abapgit_lang_definitions=>ty_langus OPTIONAL
                !iv_clsname          TYPE seoclsname
      RETURNING VALUE(rs_docu)       TYPE ty_docu
      RAISING
                zcx_abapgit_exception.
    METHODS serialize_descr_class
      IMPORTING
        !iv_clsname           TYPE seoclsname
      RETURNING
        VALUE(rs_description) TYPE ty_intf-description_int
      RAISING
        zcx_abapgit_exception.
    METHODS serialize_descr_compo
      IMPORTING
        !iv_clsname           TYPE seoclsname
      RETURNING
        VALUE(rs_description) TYPE ty_intf-description
      RAISING
        zcx_abapgit_exception.
    METHODS serialize_descr_subco
      IMPORTING
        !iv_clsname           TYPE seoclsname
      RETURNING
        VALUE(rs_description) TYPE ty_intf-description_sub
      RAISING
        zcx_abapgit_exception.
    METHODS serialize_xml
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_longtext_name,
        attributes TYPE string VALUE 'LONGTEXTS_IA',
        methods    TYPE string VALUE 'LONGTEXTS_IO',
        events     TYPE string VALUE 'LONGTEXTS_IE',
      END OF c_longtext_name.

    CONSTANTS:
      BEGIN OF c_longtext_id,
        interface  TYPE dokil-id VALUE 'IF',
        attributes TYPE dokil-id VALUE 'IA',
        methods    TYPE dokil-id VALUE 'IO',
        events     TYPE dokil-id VALUE 'IE',
      END OF c_longtext_id.

    DATA mv_aff_enabled TYPE abap_bool.
    DATA mi_object_oriented_object_fct TYPE REF TO zif_abapgit_oo_object_fnc .

    METHODS deserialize_pre_ddic
      IMPORTING
        ii_xml     TYPE REF TO zif_abapgit_xml_input
        iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_descr_class
      IMPORTING
        it_description TYPE zif_abapgit_oo_object_fnc=>ty_seoclasstx_tt OPTIONAL.
    METHODS deserialize_descr_compo
      IMPORTING
        it_description TYPE zif_abapgit_oo_object_fnc=>ty_seocompotx_tt OPTIONAL.
    METHODS deserialize_descr_subco
      IMPORTING
        it_description TYPE zif_abapgit_oo_object_fnc=>ty_seosubcotx_tt OPTIONAL.
    METHODS read_xml
      IMPORTING
                ii_xml         TYPE REF TO zif_abapgit_xml_input
      RETURNING VALUE(rs_intf) TYPE ty_intf
      RAISING
                zcx_abapgit_exception.
    METHODS read_json
      RETURNING VALUE(rs_intf) TYPE ty_intf
      RAISING
                zcx_abapgit_exception.
    METHODS extract_languages_for_transl
      IMPORTING is_intf          TYPE ty_intf
      RETURNING VALUE(rs_result) TYPE zif_abapgit_definitions=>ty_languages.
ENDCLASS.



CLASS zcl_abapgit_object_intf IMPLEMENTATION.


  METHOD constructor.

    super->constructor(
      is_item        = is_item
      iv_language    = iv_language
      io_files       = io_files
      io_i18n_params = io_i18n_params ).

    mi_object_oriented_object_fct = zcl_abapgit_oo_factory=>get_by_type( ms_item-obj_type ).

    mv_aff_enabled = zcl_abapgit_aff_factory=>get_registry( )->is_supported_object_type( 'INTF' ).

  ENDMETHOD.


  METHOD deserialize_descr_class.
    DATA ls_clskey TYPE seoclskey.
    ls_clskey-clsname = ms_item-obj_name.

    mi_object_oriented_object_fct->update_descriptions_class(
      is_key          = ls_clskey
      iv_language     = mv_language
      it_descriptions = it_description ).
  ENDMETHOD.


  METHOD deserialize_descr_compo.
    DATA ls_clskey TYPE seoclskey.
    ls_clskey-clsname = ms_item-obj_name.

    mi_object_oriented_object_fct->update_descriptions_compo(
      is_key          = ls_clskey
      it_descriptions = it_description ).
  ENDMETHOD.


  METHOD deserialize_descr_subco.
    DATA ls_clskey TYPE seoclskey.
    ls_clskey-clsname = ms_item-obj_name.

    mi_object_oriented_object_fct->update_descriptions_subco(
      is_key          = ls_clskey
      it_descriptions = it_description ).
  ENDMETHOD.


  METHOD deserialize_docu.
    DATA: lv_object     TYPE dokhl-object,
          ls_i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_line.

    lv_object = ms_item-obj_name.

    IF lines( is_docu-lines ) = 0.
      mi_object_oriented_object_fct->delete_documentation(
        iv_id          = c_longtext_id-interface
        iv_object_name = lv_object
        iv_language    = mv_language ).
      RETURN.
    ENDIF.

    mi_object_oriented_object_fct->create_documentation(
      it_lines       = is_docu-lines
      iv_id          = c_longtext_id-interface
      iv_object_name = lv_object
      iv_language    = mv_language ).

    LOOP AT is_docu-i18n_lines INTO ls_i18n_lines.
      mi_object_oriented_object_fct->create_documentation(
        it_lines         = ls_i18n_lines-lines
        iv_id            = c_longtext_id-interface
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

  ENDMETHOD.


  METHOD deserialize_pre_ddic.

    DATA ls_intf TYPE ty_intf.

    IF mv_aff_enabled = abap_true.
      ls_intf = read_json( ).
    ELSE.
      ii_xml->read( EXPORTING iv_name = 'VSEOINTERF'
                    CHANGING  cg_data = ls_intf-vseointerf ).
    ENDIF.

    set_abap_language_version( CHANGING cv_abap_language_version = ls_intf-vseointerf-unicode ).

    mi_object_oriented_object_fct->create(
      EXPORTING
        iv_check      = abap_false
        iv_package    = iv_package
      CHANGING
        cg_properties = ls_intf-vseointerf ).

  ENDMETHOD.


  METHOD deserialize_proxy.

    DATA: lv_transport    TYPE trkorr,
          li_proxy_object TYPE REF TO if_px_main,
          lv_name         TYPE prx_r3name,
          lx_proxy_fault  TYPE REF TO cx_proxy_fault.

    lv_name = ms_item-obj_name.

    lv_transport = iv_transport.

    TRY.
        li_proxy_object = cl_pxn_factory=>create(
                              application  = 'PROXY_UI'
                              display_only = abap_false
                              saveable     = abap_true
                          )->if_pxn_factory~load_by_abap_name(
                              object   = ms_item-obj_type
                              obj_name = lv_name ).

        li_proxy_object->activate(
          EXPORTING
            activate_all     = abap_true
          CHANGING
            transport_number = lv_transport ).

        li_proxy_object->dequeue( ).

      CATCH cx_proxy_fault INTO lx_proxy_fault.
        IF li_proxy_object IS BOUND.
          TRY.
              li_proxy_object->dequeue( ).
            CATCH cx_proxy_fault ##NO_HANDLER.
          ENDTRY.
        ENDIF.
        zcx_abapgit_exception=>raise_with_text( lx_proxy_fault ).
    ENDTRY.

  ENDMETHOD.


  METHOD extract_languages_for_transl.
    DATA: lv_desc              TYPE seocompotx,
          lv_desc_int          TYPE seoclasstx,
          lv_desc_sub          TYPE seosubcotx,
          lv_unique            TYPE sy-langu,
          lv_sap2              TYPE string,
          lt_unique_language   TYPE STANDARD TABLE OF sy-langu,
          lv_original_language TYPE sy-langu.


    lv_original_language = mo_i18n_params->ms_params-main_language.

    LOOP AT is_intf-description INTO lv_desc WHERE langu <> lv_original_language.
      APPEND lv_desc-langu TO lt_unique_language.
    ENDLOOP.

    LOOP AT is_intf-description_int INTO lv_desc_int WHERE langu <> lv_original_language.
      APPEND lv_desc_int-langu TO lt_unique_language.
    ENDLOOP.

    LOOP AT is_intf-description_sub INTO lv_desc_sub WHERE langu <> lv_original_language.
      APPEND lv_desc_sub-langu TO lt_unique_language.
    ENDLOOP.

    SORT lt_unique_language ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_unique_language.

    LOOP AT lt_unique_language INTO lv_unique.
      lv_sap2 = zcl_abapgit_convert=>language_sap1_to_sap2( lv_unique ).
      APPEND lv_sap2 TO rs_result.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_json.
    DATA lv_json_data TYPE string.
    DATA ls_intf_aff TYPE zif_abapgit_aff_intf_v1=>ty_main.
    DATA lo_aff_mapper TYPE REF TO zif_abapgit_aff_type_mapping.

    lv_json_data = mo_files->read_string( 'json' ).
    ls_intf_aff = lcl_aff_metadata_handler=>deserialize( lv_json_data ).

    CREATE OBJECT lo_aff_mapper TYPE lcl_aff_type_mapping.
    lo_aff_mapper->to_abapgit( EXPORTING iv_data        = ls_intf_aff
                                         iv_object_name = ms_item-obj_name
                               IMPORTING es_data        = rs_intf ).
  ENDMETHOD.


  METHOD read_xml.
    ii_xml->read( EXPORTING iv_name = 'VSEOINTERF'
                  CHANGING  cg_data = rs_intf-vseointerf ).
    ii_xml->read( EXPORTING iv_name = 'DESCRIPTIONS_INTERFACE'
                  CHANGING  cg_data = rs_intf-description_int ).
    ii_xml->read( EXPORTING iv_name = 'DESCRIPTIONS'
                  CHANGING  cg_data = rs_intf-description ).
    ii_xml->read( EXPORTING iv_name = 'DESCRIPTIONS_SUB'
                  CHANGING  cg_data = rs_intf-description_sub ).
    ii_xml->read( EXPORTING iv_name = 'LINES'
                  CHANGING  cg_data = rs_intf-docu-lines ).
    ii_xml->read( EXPORTING iv_name = 'I18N_LINES'
                  CHANGING  cg_data = rs_intf-docu-i18n_lines ).
  ENDMETHOD.


  METHOD serialize_descr_class.

    DATA: lt_descriptions    TYPE zif_abapgit_oo_object_fnc=>ty_seoclasstx_tt,
          lt_language_filter TYPE zif_abapgit_environment=>ty_system_language_filter.

    " Main language is already in VSEOCLASS so we serialize only translations
    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    lt_descriptions = mi_object_oriented_object_fct->read_descriptions_class(
      iv_object_name = iv_clsname
      iv_language    = mv_language ).

    " Remove technical languages
    lt_language_filter = mo_i18n_params->build_language_filter( ).
    DELETE lt_descriptions WHERE NOT langu IN lt_language_filter AND langu <> mv_language.

    IF lines( lt_descriptions ) = 0.
      RETURN.
    ENDIF.

    rs_description = lt_descriptions.

  ENDMETHOD.


  METHOD serialize_descr_compo.

    DATA: lt_descriptions    TYPE zif_abapgit_oo_object_fnc=>ty_seocompotx_tt,
          lv_language        TYPE spras,
          lt_language_filter TYPE zif_abapgit_environment=>ty_system_language_filter.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      lv_language = mv_language.
    ENDIF.

    lt_descriptions = mi_object_oriented_object_fct->read_descriptions_compo(
      iv_object_name = iv_clsname
      iv_language    = lv_language ).

    " Remove technical languages
    lt_language_filter = mo_i18n_params->build_language_filter( ).
    DELETE lt_descriptions WHERE NOT langu IN lt_language_filter AND langu <> mv_language.

    IF lines( lt_descriptions ) = 0.
      RETURN.
    ENDIF.

    rs_description = lt_descriptions.

  ENDMETHOD.


  METHOD serialize_descr_subco.

    DATA: lt_descriptions    TYPE zif_abapgit_oo_object_fnc=>ty_seosubcotx_tt,
          lv_language        TYPE spras,
          lt_language_filter TYPE zif_abapgit_environment=>ty_system_language_filter.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      lv_language = mv_language.
    ENDIF.

    lt_descriptions = mi_object_oriented_object_fct->read_descriptions_subco(
      iv_object_name = iv_clsname
      iv_language    = lv_language ).

    " Remove technical languages
    lt_language_filter = mo_i18n_params->build_language_filter( ).
    DELETE lt_descriptions WHERE NOT langu IN lt_language_filter AND langu <> mv_language.

    IF lines( lt_descriptions ) = 0.
      RETURN.
    ENDIF.

    rs_description = lt_descriptions.

  ENDMETHOD.


  METHOD serialize_docu.

    DATA: lt_lines      TYPE tlinetab,
          lv_object     TYPE dokhl-object,
          lv_langu      TYPE sy-langu,
          lt_i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_lines,
          ls_i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_line.

    lv_object = iv_clsname.

    lt_lines = mi_object_oriented_object_fct->read_documentation(
      iv_id          = c_longtext_id-interface
      iv_object_name = lv_object
      iv_language    = mv_language ).

    rs_docu-lines = lt_lines.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    LOOP AT it_langu_additional INTO lv_langu.

      lt_lines = mi_object_oriented_object_fct->read_documentation(
        iv_id          = c_longtext_id-interface
        iv_object_name = lv_object
        iv_language    = lv_langu ).

      IF lines( lt_lines ) > 0.
        CLEAR ls_i18n_lines.
        ls_i18n_lines-language = lv_langu.
        ls_i18n_lines-lines    = lt_lines.
        INSERT ls_i18n_lines INTO TABLE lt_i18n_lines.
      ENDIF.

    ENDLOOP.

    rs_docu-i18n_lines = lt_i18n_lines.

  ENDMETHOD.


  METHOD serialize_xml.

    DATA:
      ls_intf                      TYPE ty_intf,
      ls_clskey                    TYPE seoclskey,
      lv_serialized_data           TYPE xstring,
      lt_langu_additional          TYPE zif_abapgit_lang_definitions=>ty_langus,
      lt_i18n_file                 TYPE zif_abapgit_i18n_file=>ty_table_of,
      lo_i18n_file                 TYPE REF TO zif_abapgit_i18n_file,
      lt_languages_for_translation TYPE zif_abapgit_definitions=>ty_languages.

    ls_clskey-clsname = ms_item-obj_name.

    ls_intf-vseointerf = mi_object_oriented_object_fct->get_interface_properties( ls_clskey ).

    clear_abap_language_version( CHANGING cv_abap_language_version = ls_intf-vseointerf-unicode ).

    " Select all active translations of documentation
    " Skip main language - it was already serialized
    SELECT DISTINCT langu
      INTO TABLE lt_langu_additional
      FROM dokhl
      WHERE id     = c_longtext_id-interface
        AND object = ls_clskey-clsname
        AND langu  <> mv_language
      ORDER BY langu.

    ls_intf-docu = serialize_docu(
      iv_clsname          = ls_clskey-clsname
      it_langu_additional = lt_langu_additional ).

    ls_intf-description_int = serialize_descr_class( ls_clskey-clsname ).
    ls_intf-description     = serialize_descr_compo( ls_clskey-clsname ).
    ls_intf-description_sub = serialize_descr_subco( ls_clskey-clsname ).

    " HERE: switch with feature flag for XML or JSON file format
    IF mv_aff_enabled = abap_true.
      lv_serialized_data = lcl_aff_metadata_handler=>serialize( ls_intf ).
      mo_files->add_raw( iv_ext  = 'json'
                         iv_data = lv_serialized_data ).

      lt_languages_for_translation = extract_languages_for_transl( ls_intf ).

      lt_i18n_file = lcl_aff_metadata_handler=>serialize_translations(
        is_intf     = ls_intf
        it_language = lt_languages_for_translation ).

      LOOP AT lt_i18n_file INTO lo_i18n_file.
        mo_files->add_i18n_file( lo_i18n_file ).
      ENDLOOP.
    ELSE.
      io_xml->add( iv_name = 'VSEOINTERF'
                   ig_data = ls_intf-vseointerf ).
      io_xml->add( iv_name = 'DESCRIPTIONS_INTERFACE'
                   ig_data = ls_intf-description_int ).
      io_xml->add( iv_name = 'DESCRIPTIONS'
                   ig_data = ls_intf-description ).
      io_xml->add( iv_name = 'DESCRIPTIONS_SUB'
                   ig_data = ls_intf-description_sub ).
      io_xml->add( iv_name = 'LINES'
                   ig_data = ls_intf-docu-lines ).
      io_xml->add( iv_name = 'I18N_LINES'
                   ig_data = ls_intf-docu-i18n_lines ).

      serialize_longtexts(
        ii_xml           = io_xml
        iv_longtext_name = c_longtext_name-attributes
        iv_longtext_id   = c_longtext_id-attributes ).

      serialize_longtexts(
        ii_xml           = io_xml
        iv_longtext_name = c_longtext_name-methods
        iv_longtext_id   = c_longtext_id-methods ).

      serialize_longtexts(
        ii_xml           = io_xml
        iv_longtext_name = c_longtext_name-events
        iv_longtext_id   = c_longtext_id-events ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    TYPES: BEGIN OF ty_includes,
             programm TYPE syrepid,
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
    DATA: ls_clskey     TYPE seoclskey,
          ls_vseointerf TYPE vseointerf.

    ls_clskey-clsname = ms_item-obj_name.
    ls_vseointerf = mi_object_oriented_object_fct->get_interface_properties( ls_clskey ).

    IF ls_vseointerf-clsproxy = abap_true.
      " Proxy interfaces are managed via SPRX
      RETURN.
    ENDIF.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    corr_insert( iv_package ).

    mi_object_oriented_object_fct->delete( ls_clskey ).
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.
    DATA:
      lt_source          TYPE rswsourcet,
      ls_clskey          TYPE seoclskey,
      ls_intf            TYPE ty_intf,
      lt_description     TYPE zif_abapgit_oo_object_fnc=>ty_seocompotx_tt,
      lt_description_int TYPE zif_abapgit_oo_object_fnc=>ty_seoclasstx_tt,
      lt_description_sub TYPE zif_abapgit_oo_object_fnc=>ty_seosubcotx_tt.

    IF iv_step = zif_abapgit_object=>gc_step_id-abap.
      " HERE: switch with feature flag between XML and JSON file format
      IF mv_aff_enabled = abap_true.
        ls_intf = read_json( ).

        lcl_aff_metadata_handler=>deserialize_translation(
          EXPORTING
            io_files           = mo_files
            is_item            = ms_item
          IMPORTING
            et_description     = lt_description
            et_description_int = lt_description_int
            et_description_sub = lt_description_sub ).

        APPEND LINES OF lt_description TO ls_intf-description.
        APPEND LINES OF lt_description_int TO ls_intf-description_int.
        APPEND LINES OF lt_description_sub TO ls_intf-description_sub.

      ELSE.
        ls_intf = read_xml( io_xml ).
      ENDIF.

      set_abap_language_version( CHANGING cv_abap_language_version = ls_intf-vseointerf-unicode ).

      IF ls_intf-vseointerf-clsproxy = abap_true.
        " Proxy interfaces are managed via SPRX
        deserialize_proxy( iv_transport ).

      ELSE.
        mi_object_oriented_object_fct->create(
          EXPORTING
            iv_check      = abap_true
            iv_package    = iv_package
          CHANGING
            cg_properties = ls_intf-vseointerf ).

        ls_clskey-clsname = ms_item-obj_name.
        lt_source = mo_files->read_abap( ).

        mi_object_oriented_object_fct->deserialize_source(
          is_key     = ls_clskey
          iv_package = iv_package
          iv_version = ls_intf-vseointerf-unicode
          it_source  = lt_source ).

        deserialize_descr_class( ls_intf-description_int ).

        deserialize_descr_compo( ls_intf-description ).

        deserialize_descr_subco( ls_intf-description_sub ).

        deserialize_docu(
          is_docu = ls_intf-docu
          ii_xml  = io_xml ).

        mi_object_oriented_object_fct->add_to_activation_list( ms_item ).
      ENDIF.

    ELSEIF iv_step = zif_abapgit_object=>gc_step_id-early.

      " If interface does not exist, create it
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

    DATA: ls_class_key TYPE seoclskey,
          lv_category  TYPE seoclassdf-category.

    ls_class_key-clsname = ms_item-obj_name.

    rv_bool = mi_object_oriented_object_fct->exists( ls_class_key-clsname ).

    IF rv_bool = abap_true.
      SELECT SINGLE category FROM seoclassdf INTO lv_category
        WHERE clsname = ls_class_key-clsname
        AND ( version = '1'
        OR version = '0' ) ##WARN_OK.                   "#EC CI_GENBUFF
      IF sy-subrc = 0 AND lv_category = seoc_category_webdynpro_class.
        rv_bool = abap_false.
      ELSE.
        SELECT SINGLE obj_name FROM sproxhdr INTO ls_class_key-clsname
          WHERE object = 'INTF' AND obj_name = ls_class_key-clsname.
        IF sy-subrc = 0.
          " generated by proxy
          rv_bool = abap_false.
        ENDIF.
      ENDIF.
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
    APPEND zif_abapgit_object=>gc_step_id-lxe TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object = |{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '==============================P'.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESEOCLASS'
                                            iv_argument    = lv_object ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lt_source        TYPE seop_source_string,
          ls_interface_key TYPE seoclskey.

    ls_interface_key-clsname = ms_item-obj_name.

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

    lt_source = mi_object_oriented_object_fct->serialize_abap( ls_interface_key ).

    mo_files->add_abap( lt_source ).

    serialize_xml( io_xml ).

  ENDMETHOD.
ENDCLASS.
