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
        description     TYPE zif_abapgit_oo_object_fnc=>ty_seocompotx_tt,
        description_sub TYPE zif_abapgit_oo_object_fnc=>ty_seosubcotx_tt,
      END OF ty_intf.

    METHODS constructor
      IMPORTING
        is_item     TYPE zif_abapgit_definitions=>ty_item
        iv_language TYPE spras.
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
                !ii_xml              TYPE REF TO zif_abapgit_xml_output
                !it_langu_additional TYPE zif_abapgit_lang_definitions=>ty_langus OPTIONAL
                !iv_clsname          TYPE seoclsname
      RETURNING VALUE(rs_docu)       TYPE ty_docu
      RAISING
                zcx_abapgit_exception.
    METHODS serialize_descr
      IMPORTING
                !ii_xml               TYPE REF TO zif_abapgit_xml_output
                !iv_clsname           TYPE seoclsname
      RETURNING VALUE(rs_description) TYPE ty_intf-description
      RAISING
                zcx_abapgit_exception.
    METHODS serialize_descr_sub
      IMPORTING
                !ii_xml               TYPE REF TO zif_abapgit_xml_output
                !iv_clsname           TYPE seoclsname
      RETURNING VALUE(rs_description) TYPE ty_intf-description_sub
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

    DATA mi_object_oriented_object_fct TYPE REF TO zif_abapgit_oo_object_fnc .

    METHODS deserialize_pre_ddic
      IMPORTING
        ii_xml     TYPE REF TO zif_abapgit_xml_input
        iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_descriptions
      IMPORTING
        it_description TYPE zif_abapgit_oo_object_fnc=>ty_seocompotx_tt OPTIONAL.
    METHODS deserialize_descr_sub
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
ENDCLASS.



CLASS zcl_abapgit_object_intf IMPLEMENTATION.


  METHOD constructor.
    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).
    mi_object_oriented_object_fct = zcl_abapgit_oo_factory=>make( ms_item-obj_type ).
  ENDMETHOD.


  METHOD deserialize_descriptions.
    DATA:  ls_clskey TYPE seoclskey.
    ls_clskey-clsname = ms_item-obj_name.

    mi_object_oriented_object_fct->update_descriptions(
      is_key          = ls_clskey
      it_descriptions = it_description ).
  ENDMETHOD.


  METHOD deserialize_descr_sub.
    DATA:  ls_clskey TYPE seoclskey.
    ls_clskey-clsname = ms_item-obj_name.

    mi_object_oriented_object_fct->update_descriptions_sub(
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

    IF zcl_abapgit_persist_factory=>get_settings( )->read( )->get_experimental_features( ) = abap_true.
      ls_intf = read_json( ).
    ELSE.
      ii_xml->read( EXPORTING iv_name = 'VSEOINTERF'
                    CHANGING  cg_data = ls_intf-vseointerf ).
    ENDIF.

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


  METHOD read_json.
    DATA lv_json_data TYPE xstring.
    DATA ls_intf_aff TYPE zif_abapgit_aff_intf_v1=>ty_main.
    DATA lo_aff_mapper TYPE REF TO zif_abapgit_aff_type_mapping.

    lv_json_data = zif_abapgit_object~mo_files->read_raw( iv_ext = 'json' ).
    ls_intf_aff = lcl_aff_metadata_handler=>deserialize( lv_json_data ).

    CREATE OBJECT lo_aff_mapper TYPE lcl_aff_type_mapping.
    lo_aff_mapper->to_abapgit( EXPORTING iv_data = ls_intf_aff
                                         iv_object_name = ms_item-obj_name
                               IMPORTING es_data = rs_intf ).
  ENDMETHOD.


  METHOD read_xml.
    ii_xml->read( EXPORTING iv_name = 'VSEOINTERF'
                  CHANGING  cg_data = rs_intf-vseointerf ).
    ii_xml->read( EXPORTING iv_name = 'DESCRIPTIONS'
                  CHANGING  cg_data = rs_intf-description ).
    ii_xml->read( EXPORTING iv_name = 'DESCRIPTIONS_SUB'
                  CHANGING  cg_data = rs_intf-description_sub ).
    ii_xml->read( EXPORTING iv_name = 'LINES'
                  CHANGING  cg_data = rs_intf-docu-lines ).
    ii_xml->read( EXPORTING iv_name = 'I18N_LINES'
                  CHANGING  cg_data = rs_intf-docu-i18n_lines ).
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

    " Remove technical languages
    lt_language_filter = zcl_abapgit_factory=>get_environment( )->get_system_language_filter( ).
    DELETE lt_descriptions WHERE NOT langu IN lt_language_filter.

    IF lines( lt_descriptions ) = 0.
      RETURN.
    ENDIF.

    rs_description = lt_descriptions.

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

    " Remove technical languages
    lt_language_filter = zcl_abapgit_factory=>get_environment( )->get_system_language_filter( ).
    DELETE lt_descriptions WHERE NOT langu IN lt_language_filter.

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

    IF ii_xml->i18n_params( )-main_language_only = abap_true.
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
      ls_intf             TYPE ty_intf,
      ls_clskey           TYPE seoclskey,
      lv_serialized_data  TYPE xstring,
      lt_langu_additional TYPE zif_abapgit_lang_definitions=>ty_langus.

    ls_clskey-clsname = ms_item-obj_name.

    ls_intf-vseointerf = mi_object_oriented_object_fct->get_interface_properties( ls_clskey ).

    " Select all active translations of documentation
    " Skip main language - it was already serialized
    SELECT DISTINCT langu
      INTO TABLE lt_langu_additional
      FROM dokhl
      WHERE id     = c_longtext_id-interface
        AND object = ls_clskey-clsname
        AND langu  <> mv_language.

    ls_intf-docu = serialize_docu(
      ii_xml              = io_xml
      iv_clsname          = ls_clskey-clsname
      it_langu_additional = lt_langu_additional ).

    ls_intf-description = serialize_descr( ii_xml     = io_xml
                                           iv_clsname = ls_clskey-clsname ).
    ls_intf-description_sub = serialize_descr_sub( ii_xml     = io_xml
                                                   iv_clsname = ls_clskey-clsname ).

    " HERE: switch with feature flag for XML or JSON file format
    IF zcl_abapgit_persist_factory=>get_settings( )->read( )->get_experimental_features( ) = abap_true.
      lv_serialized_data = lcl_aff_metadata_handler=>serialize( ls_intf ).
      zif_abapgit_object~mo_files->add_raw( iv_ext  = 'json'
                                            iv_data = lv_serialized_data ).

    ELSE.
      io_xml->add( iv_name = 'VSEOINTERF'
                   ig_data = ls_intf-vseointerf ).
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
    DATA: lt_source TYPE rswsourcet,
          ls_clskey TYPE seoclskey,
          ls_intf   TYPE ty_intf.

    IF iv_step = zif_abapgit_object=>gc_step_id-abap.
      " HERE: switch with feature flag between XML and JSON file format
      IF zcl_abapgit_persist_factory=>get_settings( )->read( )->get_experimental_features( ) = abap_true.
        ls_intf = read_json( ).
      ELSE.
        ls_intf = read_xml( io_xml ).
      ENDIF.

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
        lt_source = zif_abapgit_object~mo_files->read_abap( ).
        mi_object_oriented_object_fct->deserialize_source(
          is_key    = ls_clskey
          it_source = lt_source ).

        deserialize_descriptions( it_description = ls_intf-description ).

        deserialize_descr_sub( it_description = ls_intf-description_sub ).

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

    rv_bool = mi_object_oriented_object_fct->exists( ls_class_key ).

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

    zif_abapgit_object~mo_files->add_abap( lt_source ).

    serialize_xml( io_xml ).
  ENDMETHOD.
ENDCLASS.
