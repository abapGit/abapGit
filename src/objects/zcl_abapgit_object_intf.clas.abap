CLASS zcl_abapgit_object_intf DEFINITION PUBLIC FINAL INHERITING FROM zcl_abapgit_objects_program.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    METHODS constructor
      IMPORTING
        is_item     TYPE zif_abapgit_definitions=>ty_item
        iv_language TYPE spras.
  PROTECTED SECTION.

    METHODS deserialize_proxy
      IMPORTING
        !iv_transport TYPE trkorr
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_abap
      IMPORTING
        ii_xml     TYPE REF TO zif_abapgit_xml_input
        iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_docu
      IMPORTING
        ii_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_docu
      IMPORTING
        ii_xml              TYPE REF TO zif_abapgit_xml_output
        it_langu_additional TYPE zif_abapgit_lang_definitions=>ty_langus OPTIONAL
        iv_clsname          TYPE seoclsname
      RAISING
        zcx_abapgit_exception.
    METHODS serialize_descr
      IMPORTING
        ii_xml     TYPE REF TO zif_abapgit_xml_output
        iv_clsname TYPE seoclsname
      RAISING
        zcx_abapgit_exception.
  PRIVATE SECTION.

    DATA mi_object_oriented_object_fct TYPE REF TO zif_abapgit_oo_object_fnc .

    METHODS deserialize_pre_ddic
      IMPORTING
        ii_xml     TYPE REF TO zif_abapgit_xml_input
        iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception.
    METHODS serialize_xml
      IMPORTING
        io_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .

    METHODS get_aff_content_from_json_file
      IMPORTING
        ii_log          TYPE REF TO zif_abapgit_log OPTIONAL
      RETURNING
        VALUE(rs_result) TYPE zif_abapgit_aff_intf_v1=>ty_main
      RAISING
        zcx_abapgit_exception.
    METHODS get_aff_content_for_intf
      IMPORTING
        is_interface_key   TYPE seoclskey
      RETURNING
        VALUE(rs_intf_aff) TYPE zif_abapgit_aff_intf_v1=>ty_main.
    METHODS get_descriptions_from_aff
      IMPORTING
                is_clskey             TYPE seoclskey
                is_intf_aff           TYPE zif_abapgit_aff_intf_v1=>ty_main
      RETURNING VALUE(rs_descriptions) TYPE zif_abapgit_oo_object_fnc=>ty_seocompotx_tt.
ENDCLASS.



CLASS zcl_abapgit_object_intf IMPLEMENTATION.


  METHOD constructor.
    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).
    mi_object_oriented_object_fct = zcl_abapgit_oo_factory=>make( ms_item-obj_type ).
  ENDMETHOD.


  METHOD deserialize_abap.
    DATA: ls_vseointerf   TYPE vseointerf,
          lt_source       TYPE seop_source_string,
          lt_descriptions TYPE zif_abapgit_oo_object_fnc=>ty_seocompotx_tt,
          ls_clskey       TYPE seoclskey.

    DATA ls_intf_aff TYPE zif_abapgit_aff_intf_v1=>ty_main.

    ls_clskey-clsname = ms_item-obj_name.
    lt_source = zif_abapgit_object~mo_files->read_abap( ).

    IF zcl_abapgit_persist_factory=>get_settings( )->read( )->get_experimental_features( ) = abap_true.

      ls_intf_aff = get_aff_content_from_json_file( ).

      ls_vseointerf-clsname  = ls_clskey-clsname.
      ls_vseointerf-descript = ls_intf_aff-header-description.
      ls_vseointerf-unicode  = ls_intf_aff-header-abap_language_version.
      ls_vseointerf-langu    = ls_intf_aff-header-original_language.
      ls_vseointerf-clsproxy = ls_intf_aff-proxy.
      ls_vseointerf-exposure = seoc_exposure_public.
      ls_vseointerf-state    = seoc_state_implemented.

      "  to do : fill it right!
      ls_vseointerf-author    = sy-uname.
      ls_vseointerf-createdon = sy-datum.
      ls_vseointerf-changedby = sy-uname.
    ELSE.
      ii_xml->read( EXPORTING iv_name = 'VSEOINTERF'
                    CHANGING cg_data = ls_vseointerf ).
    ENDIF.


    mi_object_oriented_object_fct->create(
      EXPORTING
        iv_package    = iv_package
      CHANGING
        cg_properties = ls_vseointerf ).

    mi_object_oriented_object_fct->deserialize_source(
      is_key               = ls_clskey
      it_source            = lt_source ).

    IF zcl_abapgit_persist_factory=>get_settings( )->read( )->get_experimental_features( ) = abap_true.
      lt_descriptions = get_descriptions_from_aff(
         is_clskey = ls_clskey
         is_intf_aff = ls_intf_aff ).
    ELSE.
      " what's about seosubcotx ??
      ii_xml->read( EXPORTING iv_name = 'DESCRIPTIONS'
                    CHANGING cg_data = lt_descriptions ).
    ENDIF.

    mi_object_oriented_object_fct->update_descriptions(
      is_key          = ls_clskey
      it_descriptions = lt_descriptions ).

    mi_object_oriented_object_fct->add_to_activation_list( ms_item ).
  ENDMETHOD.


  METHOD deserialize_docu.

    DATA: lt_lines      TYPE tlinetab,
          lv_object     TYPE dokhl-object,
          lt_i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_lines,
          ls_i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_line.

    IF ii_xml IS NOT BOUND.
      RETURN.
    ENDIF.

    ii_xml->read( EXPORTING iv_name = 'LINES'
                  CHANGING cg_data = lt_lines ).

    lv_object = ms_item-obj_name.

    IF lines( lt_lines ) = 0.
      mi_object_oriented_object_fct->delete_documentation(
        iv_id          = 'IF'
        iv_object_name = lv_object
        iv_language    = mv_language ).
      RETURN.
    ENDIF.

    mi_object_oriented_object_fct->create_documentation(
      it_lines       = lt_lines
      iv_id          = 'IF'
      iv_object_name = lv_object
      iv_language    = mv_language ).

    ii_xml->read( EXPORTING iv_name = 'I18N_LINES'
                  CHANGING cg_data = lt_i18n_lines ).

    LOOP AT lt_i18n_lines INTO ls_i18n_lines.
      mi_object_oriented_object_fct->create_documentation(
        it_lines         = ls_i18n_lines-lines
        iv_id            = 'IF'
        iv_object_name   = lv_object
        iv_language      = ls_i18n_lines-language
        iv_no_masterlang = abap_true ).
    ENDLOOP.

  ENDMETHOD.


  METHOD deserialize_pre_ddic.

    DATA: ls_vseointerf TYPE vseointerf,
          ls_clskey     TYPE seoclskey.

    DATA ls_intf_aff TYPE zif_abapgit_aff_intf_v1=>ty_main.

    ls_clskey-clsname = ms_item-obj_name.

    IF zcl_abapgit_persist_factory=>get_settings( )->read( )->get_experimental_features( ) = abap_true.

      ls_intf_aff = get_aff_content_from_json_file( ).

      ls_vseointerf-clsname  = ls_clskey-clsname.
      ls_vseointerf-descript = ls_intf_aff-header-description.
      ls_vseointerf-unicode  = ls_intf_aff-header-abap_language_version.
      ls_vseointerf-langu    = ls_intf_aff-header-original_language.
      ls_vseointerf-clsproxy = ls_intf_aff-proxy.
      ls_vseointerf-exposure = seoc_exposure_public.
      ls_vseointerf-state    = seoc_state_implemented.

      "  to do : fill it right!
      ls_vseointerf-author    = sy-uname.
      ls_vseointerf-createdon = sy-datum.
      ls_vseointerf-changedby = sy-uname.
    ELSE.
      ii_xml->read( EXPORTING iv_name = 'VSEOINTERF'
                    CHANGING cg_data = ls_vseointerf ).
    ENDIF.

    mi_object_oriented_object_fct->create(
      EXPORTING
        iv_package    = iv_package
      CHANGING
        cg_properties = ls_vseointerf ).

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

    ii_xml->add( iv_name = 'DESCRIPTIONS'
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
      iv_id          = 'IF'
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
        iv_id          = 'IF'
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

  ENDMETHOD.


  METHOD serialize_xml.

    DATA:
      ls_vseointerf       TYPE vseointerf,
      ls_clskey           TYPE seoclskey,
      lt_langu_additional TYPE zif_abapgit_lang_definitions=>ty_langus.

    ls_clskey-clsname = ms_item-obj_name.

    ls_vseointerf = mi_object_oriented_object_fct->get_interface_properties( ls_clskey ).

    CLEAR: ls_vseointerf-uuid,
           ls_vseointerf-author,
           ls_vseointerf-createdon,
           ls_vseointerf-changedby,
           ls_vseointerf-changedon,
           ls_vseointerf-chgdanyby,
           ls_vseointerf-chgdanyon,
           ls_vseointerf-r3release,
           ls_vseointerf-version.

    io_xml->add( iv_name = 'VSEOINTERF'
                 ig_data = ls_vseointerf ).

    " Select all active translations of documentation
    " Skip main language - it was already serialized
    SELECT DISTINCT langu
      INTO TABLE lt_langu_additional
      FROM dokhl
      WHERE id     = 'IF'
        AND object = ls_clskey-clsname
        AND langu  <> mv_language.

    serialize_docu( ii_xml              = io_xml
                    iv_clsname          = ls_clskey-clsname
                    it_langu_additional = lt_langu_additional ).

    serialize_descr( ii_xml     = io_xml
                     iv_clsname = ls_clskey-clsname ).

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

    DATA: ls_vseointerf TYPE vseointerf.

    DATA ls_intf_aff TYPE zif_abapgit_aff_intf_v1=>ty_main.

    IF zcl_abapgit_persist_factory=>get_settings( )->read( )->get_experimental_features( ) = abap_true.
      ls_intf_aff = get_aff_content_from_json_file( ii_log ).
    ELSE.
      io_xml->read( EXPORTING iv_name = 'VSEOINTERF'
                    CHANGING cg_data = ls_vseointerf ).
    ENDIF.

    IF iv_step = zif_abapgit_object=>gc_step_id-abap.

      IF ls_vseointerf-clsproxy = abap_true.
        " Proxy interfaces are managed via SPRX
        deserialize_proxy( iv_transport ).
        RETURN.
      ENDIF.

      deserialize_abap( ii_xml     = io_xml
                        iv_package = iv_package ).

      deserialize_docu( io_xml ).

    ELSEIF iv_step = zif_abapgit_object=>gc_step_id-early.

      " If interface does not exist, create it
      " so DDIC that depends on it does not fail activation
      IF zif_abapgit_object~exists( ) = abap_false.
        deserialize_pre_ddic(
          ii_xml     = io_xml
          iv_package = iv_package ).
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
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
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


  METHOD zif_abapgit_object~serialize.

    DATA: lt_source        TYPE seop_source_string,
          ls_interface_key TYPE seoclskey.

    DATA lv_json_xstring   TYPE xstring.
    DATA lx_exception      TYPE REF TO cx_root.
    DATA lt_langu_additional TYPE zif_abapgit_lang_definitions=>ty_langus.
    DATA ls_aff              TYPE zif_abapgit_aff_intf_v1=>ty_main.
    DATA lo_ajson          TYPE REF TO zcl_abapgit_json_handler.

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
    " get infos for json file
    IF zcl_abapgit_persist_factory=>get_settings( )->read( )->get_experimental_features( ) = abap_true.

      CREATE OBJECT lo_ajson.

      ls_aff = get_aff_content_for_intf( ls_interface_key ).

      TRY.

          lv_json_xstring = lo_ajson->serialize( ls_aff ).

          zif_abapgit_object~mo_files->add_raw( iv_ext = 'json'
                             iv_data = lv_json_xstring ).

        CATCH cx_root INTO lx_exception.
          zcx_abapgit_exception=>raise_with_text( lx_exception ).
      ENDTRY.

      " Select all active translations of documentation
      " Skip main language - it was already serialized
      SELECT DISTINCT langu
        INTO TABLE lt_langu_additional
        FROM dokhl
        WHERE id     = 'IF'
          AND object = ls_interface_key-clsname
          AND langu  <> mv_language.

      serialize_docu( ii_xml              = io_xml
                      iv_clsname          = ls_interface_key-clsname
                      it_langu_additional = lt_langu_additional ).

    ELSE.
      serialize_xml( io_xml ).
    ENDIF.
  ENDMETHOD.


  METHOD get_aff_content_for_intf.

    " get metadata and fill 'ZIF_ABAPGIT_AFF_INTF_V1=>TY_MAIN' in 702 compatible way

    rs_intf_aff-format_version = '1'.

    SELECT SINGLE masterlang FROM tadir INTO rs_intf_aff-header-original_language
       WHERE pgmid = seok_pgmid_r3tr AND object = 'INTF' AND obj_name = is_interface_key-clsname.

    SELECT SINGLE descript AS description category clsproxy AS proxy unicode AS abap_language_version
           FROM vseointerf
           INTO (rs_intf_aff-header-description, rs_intf_aff-category, rs_intf_aff-proxy,
                 rs_intf_aff-header-abap_language_version)
           WHERE clsname = is_interface_key-clsname AND version = '1' AND
                 langu = rs_intf_aff-header-original_language.

    rs_intf_aff-descriptions = cl_oo_aff_clif_helper=>get_descriptions_compo_subco(
                              language  = rs_intf_aff-header-original_language
                              clif_name = is_interface_key-clsname ).

  ENDMETHOD.


  METHOD get_aff_content_from_json_file.

    DATA lv_object TYPE trkey.
    DATA lo_ajson TYPE REF TO zcl_abapgit_json_handler.
    DATA lv_json_as_xstring TYPE xstring.
    DATA lx_exception TYPE REF TO cx_static_check.

    CREATE OBJECT lo_ajson.

    " get INTF metadata
    lv_json_as_xstring = zif_abapgit_object~mo_files->read_raw( iv_ext = 'json' ).

    lv_object-devclass = ms_item-devclass.
    lv_object-obj_type = ms_item-obj_type.
    lv_object-obj_name = ms_item-obj_name.

    TRY.
        lo_ajson->deserialize(
           EXPORTING
             iv_content = lv_json_as_xstring
           IMPORTING
             ev_data    = rs_result ).

        rs_result-header-original_language = 'E'.

      CATCH cx_static_check INTO lx_exception.
        IF ii_log IS BOUND.
          ii_log->add_exception(
              ix_exc  = lx_exception
              is_item = ms_item ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD get_descriptions_from_aff.

    DATA ls_description TYPE seocompotx.
    FIELD-SYMBOLS <ls_description>      TYPE zif_abapgit_aff_oo_types_v1=>ty_component_description.
    FIELD-SYMBOLS <ls_meth_description> TYPE zif_abapgit_aff_oo_types_v1=>ty_method.
    FIELD-SYMBOLS <ls_evt_description>  TYPE zif_abapgit_aff_oo_types_v1=>ty_event.


    LOOP AT is_intf_aff-descriptions-types ASSIGNING <ls_description>.
      ls_description-clsname  = is_clskey-clsname.
      ls_description-cmpname  = <ls_description>-name.
      ls_description-langu    = is_intf_aff-header-original_language.
      ls_description-descript = <ls_description>-description.
      APPEND ls_description TO rs_descriptions.
    ENDLOOP.

    LOOP AT is_intf_aff-descriptions-attributes ASSIGNING <ls_description>.
      ls_description-clsname  = is_clskey-clsname.
      ls_description-cmpname  = <ls_description>-name.
      ls_description-langu    = is_intf_aff-header-original_language.
      ls_description-descript = <ls_description>-description.
      APPEND ls_description TO rs_descriptions.
    ENDLOOP.

    LOOP AT is_intf_aff-descriptions-methods ASSIGNING <ls_meth_description>.
      ls_description-clsname  = is_clskey-clsname.
      ls_description-cmpname  = <ls_meth_description>-name.
      ls_description-langu    = is_intf_aff-header-original_language.
      ls_description-descript = <ls_meth_description>-description.
      APPEND ls_description TO rs_descriptions.
    ENDLOOP.

    LOOP AT is_intf_aff-descriptions-events ASSIGNING <ls_evt_description>.
      ls_description-clsname  = is_clskey-clsname.
      ls_description-cmpname  = <ls_evt_description>-name.
      ls_description-langu    = is_intf_aff-header-original_language.
      ls_description-descript = <ls_evt_description>-description.
      APPEND ls_description TO rs_descriptions.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
