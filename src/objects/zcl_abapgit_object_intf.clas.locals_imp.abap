CLASS lcl_aff_helper DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:

*    create_empty_interface

*      IMPORTING iv_intf_name   TYPE seoclsname

*                io_lock_handle TYPE REF TO if_adt_lock_handle

*      RAISING   zcx_abapgit_exception,
      generate_class_pool
        IMPORTING iv_class_name TYPE seoclsname,
      get_descriptions_compo_subco
        IMPORTING iv_language          TYPE sy-langu
                  iv_clif_name         TYPE seoclsname
                  it_descriptions      TYPE zcl_abapgit_object_intf=>ty_intf-description
        RETURNING VALUE(rs_properties) TYPE zif_abapgit_aff_oo_types_v1=>ty_descriptions ,
      set_descriptions_compo_subco
        IMPORTING iv_clif_name  TYPE seoclsname
                  iv_language   TYPE langu
                  is_properties TYPE zif_abapgit_aff_oo_types_v1=>ty_descriptions .
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_component,
        visibility TYPE seoexpose,
        cmpname    TYPE seocmpname,
        descript   TYPE seodescr,
        cmptype    TYPE seocmptype,
      END OF ty_component,
      BEGIN OF ty_sub_component,
        cmpname  TYPE seocmpname,
        sconame  TYPE seosconame,
        descript TYPE seodescr,
        scotype  TYPE seoscotype,
      END OF ty_sub_component,
      ty_compontents     TYPE STANDARD TABLE OF ty_component,
      ty_sub_compontents TYPE STANDARD TABLE OF ty_sub_component.

    CLASS-METHODS:
      get_attributes
        IMPORTING is_components    TYPE ty_compontents
                  it_descriptions  TYPE zcl_abapgit_object_intf=>ty_intf-description
        RETURNING VALUE(rs_result) TYPE zif_abapgit_aff_oo_types_v1=>ty_component_descriptions,
      get_methods
        IMPORTING is_components    TYPE ty_compontents
                  it_descriptions  TYPE zcl_abapgit_object_intf=>ty_intf-description
        RETURNING VALUE(rs_result) TYPE zif_abapgit_aff_oo_types_v1=>ty_methods,
      get_types
        IMPORTING is_components    TYPE ty_compontents
                  it_descriptions  TYPE zcl_abapgit_object_intf=>ty_intf-description
        RETURNING VALUE(rs_result) TYPE zif_abapgit_aff_oo_types_v1=>ty_component_descriptions,
      get_events
        IMPORTING is_components    TYPE ty_compontents
                  it_descriptions  TYPE zcl_abapgit_object_intf=>ty_intf-description
        RETURNING VALUE(rs_result) TYPE zif_abapgit_aff_oo_types_v1=>ty_events,
      set_methods
        IMPORTING iv_clif_name  TYPE seoclsname
                  iv_language   TYPE langu
                  is_properties TYPE zif_abapgit_aff_oo_types_v1=>ty_descriptions,
      set_attributes
        IMPORTING iv_clif_name  TYPE seoclsname
                  iv_language   TYPE langu
                  is_properties TYPE zif_abapgit_aff_oo_types_v1=>ty_descriptions,
      set_events
        IMPORTING iv_clif_name  TYPE seoclsname
                  iv_language   TYPE langu
                  is_properties TYPE zif_abapgit_aff_oo_types_v1=>ty_descriptions,
      set_types
        IMPORTING iv_clif_name  TYPE seoclsname
                  iv_language   TYPE langu
                  is_properties TYPE zif_abapgit_aff_oo_types_v1=>ty_descriptions .
ENDCLASS.


CLASS lcl_aff_helper IMPLEMENTATION.


*  METHOD create_empty_interface.
*    DATA:
*      lo_interface_error TYPE string,
*      ls_empty_interface TYPE vseointerf.
*
*    ls_empty_interface-clsname = iv_intf_name.
*    ls_empty_interface-version = seoc_version_active.
*    ls_empty_interface-langu = sy-langu.
*    ls_empty_interface-descript = space.
*    ls_empty_interface-state = seoc_state_implemented.
*    ls_empty_interface-exposure = seoc_exposure_public.
*
*    CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
*      EXPORTING
*        version       = seoc_version_active
*        suppress_corr = abap_true
*        lock_handle   = io_lock_handle
*      CHANGING
*        interface     = ls_empty_interface
*      EXCEPTIONS
*        OTHERS        = 1.
*    IF sy-subrc <> 0.
*      IF sy-msgid IS NOT INITIAL.
*        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lo_interface_error.
*      ELSE.
*        lo_interface_error = 'Internal error'.
*      ENDIF.
*      " todo: raise exception here
*    ENDIF.
*  ENDMETHOD.


  METHOD generate_class_pool.
    DATA:
      lo_cifkey TYPE seoclskey.

    lo_cifkey-clsname = iv_class_name.
    PERFORM set_wbinactive IN PROGRAM saplseok USING ' '.
    CALL FUNCTION 'SEO_WBINACTIVE_BROADCAST'
      EXPORTING
        wbia = ' '.
    CALL FUNCTION 'SEO_CLIF_SET_WBINACTIVE'
      EXPORTING
        wbia = ' '.

    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        cifkey  = lo_cifkey
        version = seoc_version_active.

    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        cifkey  = lo_cifkey
        version = seoc_version_inactive.

    CALL FUNCTION 'SEO_CLASS_GENERATE_CLASSPOOL'
      EXPORTING
        clskey        = lo_cifkey
        suppress_corr = seox_true
      EXCEPTIONS
        OTHERS        = 1 ##FM_SUBRC_OK.
  ENDMETHOD.

  METHOD get_descriptions_compo_subco.
    DATA:
      lt_components_exp TYPE ty_compontents.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_components_exp
      FROM seocompo
      WHERE clsname = iv_clif_name.                    "#EC CI_BUFFJOIN

    rs_properties-attributes = get_attributes( is_components   = lt_components_exp
                                               it_descriptions = it_descriptions ).
    rs_properties-methods = get_methods( is_components   = lt_components_exp
                                         it_descriptions = it_descriptions ).
    rs_properties-events = get_events( is_components   = lt_components_exp
                                       it_descriptions = it_descriptions ).
    rs_properties-types = get_types( is_components   = lt_components_exp
                                     it_descriptions = it_descriptions ).

  ENDMETHOD.


  METHOD get_attributes.
    DATA lv_attribute TYPE zif_abapgit_aff_oo_types_v1=>ty_component_description.
    FIELD-SYMBOLS:
      <ls_component>   TYPE ty_component,
      <ls_description> TYPE seocompotx.

    LOOP AT is_components ASSIGNING <ls_component> WHERE cmptype = seoo_cmptype_attribute.
      lv_attribute-name = <ls_component>-cmpname.
      READ TABLE it_descriptions WITH KEY cmpname = <ls_component>-cmpname ASSIGNING <ls_description>.
      IF sy-subrc = 0.
        lv_attribute-description = <ls_description>-descript.
        INSERT lv_attribute INTO TABLE rs_result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_methods.
    DATA lv_method    TYPE zif_abapgit_aff_oo_types_v1=>ty_method.
    FIELD-SYMBOLS:
      <ls_component>   TYPE ty_component,
      <ls_description> TYPE seocompotx.

    LOOP AT is_components ASSIGNING <ls_component> WHERE cmptype = seoo_cmptype_method.
      lv_method-name = <ls_component>-cmpname.
      READ TABLE it_descriptions WITH KEY cmpname = <ls_component>-cmpname ASSIGNING <ls_description>.
      IF sy-subrc = 0.
        lv_method-description = <ls_description>-descript.
        INSERT lv_method INTO TABLE rs_result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_types.
    DATA lv_type TYPE zif_abapgit_aff_oo_types_v1=>ty_component_description.
    FIELD-SYMBOLS:
      <ls_component>   TYPE ty_component,
      <ls_description> TYPE seocompotx.

    LOOP AT is_components ASSIGNING <ls_component> WHERE cmptype = seoo_cmptype_type AND descript IS NOT INITIAL.
      lv_type-name = <ls_component>-cmpname.
      READ TABLE it_descriptions WITH KEY cmpname = <ls_component>-cmpname ASSIGNING <ls_description>.
      IF sy-subrc = 0.
        lv_type-description = <ls_description>-descript.
        INSERT lv_type INTO TABLE rs_result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_events.
    DATA lv_event     TYPE zif_abapgit_aff_oo_types_v1=>ty_event.
    FIELD-SYMBOLS:
      <ls_component>   TYPE ty_component,
      <ls_description> TYPE seocompotx.

    LOOP AT is_components ASSIGNING <ls_component> WHERE cmptype = seoo_cmptype_event.
      lv_event-name = <ls_component>-cmpname.
      READ TABLE it_descriptions WITH KEY cmpname = <ls_component>-cmpname ASSIGNING <ls_description>.
      IF sy-subrc = 0.
        lv_event-description = <ls_description>-descript.
        INSERT lv_event INTO TABLE rs_result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_attributes.
    DATA:
      lo_attribute TYPE seocompotx.
    FIELD-SYMBOLS: <ls_attribute> TYPE zif_abapgit_aff_oo_types_v1=>ty_component_description.

    LOOP AT is_properties-attributes ASSIGNING <ls_attribute>.
      lo_attribute-clsname  = iv_clif_name.
      lo_attribute-cmpname  = <ls_attribute>-name.
      lo_attribute-langu    = iv_language.
      lo_attribute-descript = <ls_attribute>-description.
      MODIFY seocompotx FROM lo_attribute.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_methods.
    DATA:
      lo_method           TYPE seocompotx,
      lo_method_exception TYPE seosubcotx,
      lo_method_parameter TYPE seosubcotx.
    FIELD-SYMBOLS: <ls_method>    TYPE zif_abapgit_aff_oo_types_v1=>ty_method,
                   <ls_parameter> TYPE zif_abapgit_aff_oo_types_v1=>ty_component_description,
                   <ls_exception> TYPE zif_abapgit_aff_oo_types_v1=>ty_component_description.

    LOOP AT is_properties-methods ASSIGNING <ls_method>.
      lo_method-clsname  = iv_clif_name.
      lo_method-cmpname  = <ls_method>-name.
      lo_method-langu    = iv_language.
      lo_method-descript = <ls_method>-description.
      MODIFY seocompotx FROM lo_method.

      LOOP AT <ls_method>-parameters ASSIGNING <ls_parameter>.
        lo_method_parameter-clsname  = iv_clif_name.
        lo_method_parameter-cmpname  = <ls_method>-name.
        lo_method_parameter-sconame  = <ls_parameter>-name.
        lo_method_parameter-langu    = iv_language.
        lo_method_parameter-descript = <ls_parameter>-description.
        MODIFY seosubcotx FROM lo_method_parameter.
      ENDLOOP.

      LOOP AT <ls_method>-exceptions ASSIGNING <ls_exception>.
        lo_method_exception-clsname  = iv_clif_name.
        lo_method_exception-cmpname  = <ls_method>-name.
        lo_method_exception-sconame  = <ls_exception>-name.
        lo_method_exception-langu    = iv_language.
        lo_method_exception-descript = <ls_exception>-description.
        MODIFY seosubcotx FROM lo_method_exception.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_events.
    DATA:
      lo_event_parameter TYPE seosubcotx,
      lo_event           TYPE seocompotx.
    FIELD-SYMBOLS: <ls_event>     TYPE zif_abapgit_aff_oo_types_v1=>ty_event,
                   <ls_parameter> TYPE zif_abapgit_aff_oo_types_v1=>ty_component_description.

    LOOP AT is_properties-events ASSIGNING <ls_event>.
      lo_event-clsname  = iv_clif_name.
      lo_event-cmpname  = <ls_event>-name.
      lo_event-langu    = iv_language.
      lo_event-descript = <ls_event>-description.
      MODIFY seocompotx FROM lo_event.

      LOOP AT <ls_event>-parameters ASSIGNING <ls_parameter>.
        lo_event_parameter-clsname  = iv_clif_name.
        lo_event_parameter-cmpname  = <ls_event>-name.
        lo_event_parameter-sconame  = <ls_parameter>-name.
        lo_event_parameter-langu    = iv_language.
        lo_event_parameter-descript = <ls_parameter>-description.
        MODIFY seosubcotx FROM lo_event_parameter.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_types.
    DATA:
      lo_type TYPE seocompotx.
    FIELD-SYMBOLS: <ls_type> TYPE zif_abapgit_aff_oo_types_v1=>ty_component_description.

    LOOP AT is_properties-types ASSIGNING <ls_type>.
      lo_type-clsname  = iv_clif_name.
      lo_type-cmpname  = <ls_type>-name.
      lo_type-langu    = iv_language.
      lo_type-descript = <ls_type>-description.
      MODIFY seocompotx FROM lo_type.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_descriptions_compo_subco.
    set_attributes( is_properties = is_properties
                    iv_clif_name  = iv_clif_name
                    iv_language   = iv_language ).
    set_methods( is_properties = is_properties
                 iv_clif_name  = iv_clif_name
                 iv_language   = iv_language ).
    set_events( is_properties = is_properties
                iv_clif_name  = iv_clif_name
                iv_language   = iv_language ).
    set_types( is_properties = is_properties
               iv_clif_name  = iv_clif_name
               iv_language   = iv_language ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_aff_type_mapping DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_aff_type_mapping.
  PRIVATE SECTION.
    METHODS set_abapgit_descriptions
      IMPORTING is_clsname       TYPE seoclsname
                is_intf_aff      TYPE zif_abapgit_aff_intf_v1=>ty_main
      RETURNING VALUE(rs_return) TYPE zif_abapgit_oo_object_fnc=>ty_seocompotx_tt.
ENDCLASS.

CLASS lcl_aff_type_mapping IMPLEMENTATION.

  METHOD zif_abapgit_aff_type_mapping~to_aff.
    DATA:
      ls_data_abapgit TYPE zcl_abapgit_object_intf=>ty_intf,
      ls_data_aff     TYPE zif_abapgit_aff_intf_v1=>ty_main,
      lt_seocompo     TYPE STANDARD TABLE OF seocompo.

    FIELD-SYMBOLS:
        <ls_seocompo> TYPE seocompo.

    ls_data_abapgit = iv_data.

    ls_data_aff-format_version = '1'.

    " get header
    ls_data_aff-header-description = ls_data_abapgit-vseointerf-descript.
    ls_data_aff-header-abap_language_version = ls_data_abapgit-vseointerf-unicode.
    ls_data_aff-header-original_language = ls_data_abapgit-vseointerf-langu.

    " get category and proxy
    ls_data_aff-category = ls_data_abapgit-vseointerf-category.
    ls_data_aff-proxy = ls_data_abapgit-vseointerf-clsproxy.

    " get descriptions
    ls_data_aff-descriptions = lcl_aff_helper=>get_descriptions_compo_subco(
      iv_language     = ls_data_aff-header-original_language
      iv_clif_name    = ls_data_abapgit-vseointerf-clsname
      it_descriptions = ls_data_abapgit-description ).

    es_data = ls_data_aff.
  ENDMETHOD.

  METHOD zif_abapgit_aff_type_mapping~to_abapgit.
    DATA:
      ls_data_abapgit TYPE zcl_abapgit_object_intf=>ty_intf,
      ls_data_aff     TYPE zif_abapgit_aff_intf_v1=>ty_main,
      lv_classname    TYPE seoclsname.


    ls_data_aff = iv_data.

    lv_classname = iv_object_name.

    ls_data_abapgit-description = set_abapgit_descriptions( is_clsname  = lv_classname
                                                            is_intf_aff = ls_data_aff ).

    ls_data_abapgit-vseointerf-clsname = iv_object_name.
    ls_data_abapgit-vseointerf-descript = ls_data_aff-header-description.
    ls_data_abapgit-vseointerf-category = ls_data_aff-category.
    ls_data_abapgit-vseointerf-unicode  = ls_data_aff-header-abap_language_version.
    ls_data_abapgit-vseointerf-langu    = ls_data_aff-header-original_language.
    ls_data_abapgit-vseointerf-clsproxy = ls_data_aff-proxy.
    ls_data_abapgit-vseointerf-exposure = seoc_exposure_public.
    ls_data_abapgit-vseointerf-state    = seoc_state_implemented.

    es_data = ls_data_abapgit.

  ENDMETHOD.

  METHOD set_abapgit_descriptions.

    DATA ls_description TYPE seocompotx.
    FIELD-SYMBOLS <ls_description>      TYPE zif_abapgit_aff_oo_types_v1=>ty_component_description.
    FIELD-SYMBOLS <ls_meth_description> TYPE zif_abapgit_aff_oo_types_v1=>ty_method.
    FIELD-SYMBOLS <ls_evt_description>  TYPE zif_abapgit_aff_oo_types_v1=>ty_event.


    LOOP AT is_intf_aff-descriptions-types ASSIGNING <ls_description>.
      ls_description-clsname  = is_clsname.
      ls_description-cmpname  = <ls_description>-name.
      ls_description-langu    = is_intf_aff-header-original_language.
      ls_description-descript = <ls_description>-description.
      APPEND ls_description TO rs_return.
    ENDLOOP.

    LOOP AT is_intf_aff-descriptions-attributes ASSIGNING <ls_description>.
      ls_description-clsname  = is_clsname.
      ls_description-cmpname  = <ls_description>-name.
      ls_description-langu    = is_intf_aff-header-original_language.
      ls_description-descript = <ls_description>-description.
      APPEND ls_description TO rs_return.
    ENDLOOP.

    LOOP AT is_intf_aff-descriptions-methods ASSIGNING <ls_meth_description>.
      ls_description-clsname  = is_clsname.
      ls_description-cmpname  = <ls_meth_description>-name.
      ls_description-langu    = is_intf_aff-header-original_language.
      ls_description-descript = <ls_meth_description>-description.
      APPEND ls_description TO rs_return.
    ENDLOOP.

    LOOP AT is_intf_aff-descriptions-events ASSIGNING <ls_evt_description>.
      ls_description-clsname  = is_clsname.
      ls_description-cmpname  = <ls_evt_description>-name.
      ls_description-langu    = is_intf_aff-header-original_language.
      ls_description-descript = <ls_evt_description>-description.
      APPEND ls_description TO rs_return.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_aff_metadata_handler DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS serialize
      IMPORTING is_intf          TYPE zcl_abapgit_object_intf=>ty_intf
      RETURNING VALUE(rv_result) TYPE xstring
      RAISING   zcx_abapgit_exception.
    CLASS-METHODS deserialize
      IMPORTING iv_data          TYPE xstring
      RETURNING VALUE(rv_result) TYPE zif_abapgit_aff_intf_v1=>ty_main
      RAISING   zcx_abapgit_exception.
  PRIVATE SECTION.
    CLASS-METHODS:
      "! For serialization
      "! @parameter rt_result | Map/table that associates ABAP values to JSON values (enums)
      get_mappings
        RETURNING VALUE(rt_result) TYPE zcl_abapgit_json_handler=>ty_enum_mappings,
      "! For serialization
      "! @parameter rt_result | Paths that will not be serialized (depending on value)
      get_paths_to_skip
        RETURNING VALUE(rt_result) TYPE zcl_abapgit_json_handler=>ty_skip_paths.
ENDCLASS.

CLASS lcl_aff_metadata_handler IMPLEMENTATION.

  METHOD serialize.
    DATA:
      ls_data_aff      TYPE zif_abapgit_aff_intf_v1=>ty_main,
      lx_exception     TYPE REF TO cx_root,
      lo_aff_handler   TYPE REF TO zcl_abapgit_json_handler,
      lo_aff_mapper    TYPE REF TO zif_abapgit_aff_type_mapping,
      lt_enum_mappings TYPE zcl_abapgit_json_handler=>ty_enum_mappings,
      lt_paths_to_skip TYPE zcl_abapgit_json_handler=>ty_skip_paths.


    CREATE OBJECT lo_aff_mapper TYPE lcl_aff_type_mapping.
    lo_aff_mapper->to_aff( EXPORTING iv_data = is_intf
                           IMPORTING es_data = ls_data_aff ).

    lt_enum_mappings = get_mappings( ).
    lt_paths_to_skip = get_paths_to_skip( ).

    CREATE OBJECT lo_aff_handler.
    TRY.
        rv_result = lo_aff_handler->serialize( iv_data          = ls_data_aff
                                               iv_enum_mappings = lt_enum_mappings
                                               iv_skip_paths    = lt_paths_to_skip ).
      CATCH cx_root INTO lx_exception.
        zcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDTRY.

  ENDMETHOD.

  METHOD get_mappings.
    DATA:
      ls_category_mapping   TYPE zcl_abapgit_json_handler=>ty_enum_mapping,
      ls_json_abap_mapping  TYPE zcl_abapgit_json_handler=>ty_json_abap_mapping,
      lt_json_abap_mappings TYPE zcl_abapgit_json_handler=>ty_json_abap_mappings.

    ls_json_abap_mapping-abap = zif_abapgit_aff_intf_v1=>co_category-general.
    ls_json_abap_mapping-json = 'standard'.
    APPEND ls_json_abap_mapping TO lt_json_abap_mappings.
    ls_json_abap_mapping-abap = zif_abapgit_aff_intf_v1=>co_category-classic_badi.
    ls_json_abap_mapping-json = 'classicBadi'.
    APPEND ls_json_abap_mapping TO lt_json_abap_mappings.
    ls_json_abap_mapping-abap = zif_abapgit_aff_intf_v1=>co_category-business_static_components.
    ls_json_abap_mapping-json = 'businessStaticComponents'.
    APPEND ls_json_abap_mapping TO lt_json_abap_mappings.
    ls_json_abap_mapping-abap = zif_abapgit_aff_intf_v1=>co_category-db_procedure_proxy.
    ls_json_abap_mapping-json = 'dbProcedureProxy'.
    APPEND ls_json_abap_mapping TO lt_json_abap_mappings.
    ls_json_abap_mapping-abap = zif_abapgit_aff_intf_v1=>co_category-web_dynpro_runtime.
    ls_json_abap_mapping-json = 'webDynproRuntime'.
    APPEND ls_json_abap_mapping TO lt_json_abap_mappings.
    ls_json_abap_mapping-abap = zif_abapgit_aff_intf_v1=>co_category-enterprise_service.
    ls_json_abap_mapping-json = 'enterpriseService'.
    APPEND ls_json_abap_mapping TO lt_json_abap_mappings.

    ls_category_mapping-path = '/category'.
    ls_category_mapping-mappings = lt_json_abap_mappings.

    APPEND ls_category_mapping TO rt_result.
  ENDMETHOD.

  METHOD get_paths_to_skip.
    DATA:
      ls_path_to_skipp TYPE zcl_abapgit_json_handler=>ty_path_value_pair.

    ls_path_to_skipp-path  = '/category'.
    ls_path_to_skipp-value = 'standard'.

    APPEND ls_path_to_skipp TO rt_result.
  ENDMETHOD.

  METHOD deserialize.
    DATA:
      lo_ajson                      TYPE REF TO zcl_abapgit_json_handler,
      lx_exception                  TYPE REF TO cx_static_check,
      lt_enum_mappings              TYPE zcl_abapgit_json_handler=>ty_enum_mappings,
      lt_default_abap_langu_version TYPE zcl_abapgit_json_handler=>ty_path_value_pair,
      lt_values_for_initial         TYPE zcl_abapgit_json_handler=>ty_skip_paths.

    lt_values_for_initial = get_paths_to_skip( ).

    lt_default_abap_langu_version-path  = '/header/abapLanguageVersion'.
    lt_default_abap_langu_version-value = 'standard'.
    APPEND lt_default_abap_langu_version TO lt_values_for_initial.

    lt_enum_mappings = get_mappings( ).


    CREATE OBJECT lo_ajson.
    TRY.
        lo_ajson->deserialize(
          EXPORTING
            iv_content       = iv_data
            iv_defaults      = lt_values_for_initial
            iv_enum_mappings = lt_enum_mappings
          IMPORTING
            ev_data          = rv_result ).
      CATCH cx_static_check INTO lx_exception.
        zcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDTRY.


  ENDMETHOD.

ENDCLASS.
