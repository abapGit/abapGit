CLASS lcl_aff_helper DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_descriptions_compo_subco
        IMPORTING iv_language          TYPE sy-langu
                  iv_clif_name         TYPE seoclsname
        RETURNING VALUE(rs_properties) TYPE zif_abapgit_aff_oo_types_v1=>ty_descriptions ,
      get_descr_comp_subc_w_exposure
        IMPORTING iv_language          TYPE sy-langu
                  iv_clif_name         TYPE seoclsname
                  iv_exposure          TYPE seoexpose DEFAULT seoc_exposure_public
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
      ty_compontents     TYPE SORTED TABLE OF ty_component WITH UNIQUE DEFAULT KEY,
      ty_sub_compontents TYPE SORTED TABLE OF ty_sub_component WITH UNIQUE DEFAULT KEY.

    CLASS-METHODS:
      get_attributes
        IMPORTING is_components    TYPE ty_compontents
        RETURNING VALUE(rs_result) TYPE zif_abapgit_aff_oo_types_v1=>ty_component_descriptions,
      get_methods
        IMPORTING is_components     TYPE ty_compontents
                  is_sub_components TYPE ty_sub_compontents
        RETURNING VALUE(rs_result)  TYPE zif_abapgit_aff_oo_types_v1=>ty_methods,
      get_types
        IMPORTING is_components    TYPE ty_compontents
        RETURNING VALUE(rs_result) TYPE zif_abapgit_aff_oo_types_v1=>ty_component_descriptions,
      get_events
        IMPORTING is_components     TYPE ty_compontents
                  is_sub_components TYPE ty_sub_compontents
        RETURNING VALUE(rs_result)  TYPE zif_abapgit_aff_oo_types_v1=>ty_events,
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

  METHOD get_descr_comp_subc_w_exposure.
    DATA:
      lt_components     TYPE ty_compontents,
      lt_sub_components TYPE ty_sub_compontents.


    SELECT df~exposure AS visibility component~cmpname component_text~descript component~cmptype
      INTO TABLE lt_components
      FROM seocompo AS component
      LEFT OUTER JOIN seocompotx AS component_text
      ON component~cmpname = component_text~cmpname AND component~clsname = component_text~clsname AND
         component_text~langu = iv_language
      INNER JOIN seocompodf AS df
      ON component~clsname = df~clsname AND
         component~cmpname = df~cmpname
      WHERE component~clsname = iv_clif_name AND
            df~exposure       = iv_exposure.           "#EC CI_BUFFJOIN

    SELECT sub_component~cmpname sub_component~sconame sub_component_text~descript sub_component~scotype
      INTO TABLE lt_sub_components
      FROM seosubco AS sub_component JOIN seosubcotx AS sub_component_text
      ON sub_component~clsname = sub_component_text~clsname AND
         sub_component~cmpname = sub_component_text~cmpname AND
         sub_component~sconame = sub_component_text~sconame
      INNER JOIN seocompodf AS df
      ON sub_component~clsname = df~clsname AND
         sub_component~cmpname = df~cmpname
      WHERE sub_component~clsname    = iv_clif_name
        AND df~exposure              = iv_exposure
        AND sub_component_text~langu = iv_language
        AND sub_component_text~descript <> space.      "#EC CI_BUFFJOIN



    rs_properties-attributes = get_attributes( lt_components ).
    rs_properties-methods = get_methods( is_components     = lt_components
                                         is_sub_components = lt_sub_components ).
    rs_properties-events = get_events( is_components     = lt_components
                                       is_sub_components = lt_sub_components ).
    rs_properties-types = get_types( lt_components ).
  ENDMETHOD.


  METHOD get_descriptions_compo_subco.
    TYPES:
      BEGIN OF ty_helper_type,
        cmpname  TYPE seocmpname,
        descript TYPE seodescr,
        cmptype  TYPE seocmptype,
      END OF ty_helper_type.
    DATA:
      lt_components     TYPE STANDARD TABLE OF ty_helper_type,
      lt_sub_components TYPE ty_sub_compontents,
      lt_components_exp TYPE ty_compontents,
      ls_component_exp  LIKE LINE OF lt_components_exp.
    FIELD-SYMBOLS:
      <ls_component> LIKE LINE OF lt_components.


    SELECT component~cmpname component_text~descript component~cmptype
      INTO TABLE lt_components
      FROM seocompo AS component
      LEFT OUTER JOIN seocompotx AS component_text
      ON component~cmpname = component_text~cmpname AND component~clsname    = component_text~clsname
                                                    AND component_text~langu = iv_language
      WHERE component~clsname = iv_clif_name
      ORDER BY component~cmpname.                      "#EC CI_BUFFJOIN

    SELECT sub_component~cmpname sub_component~sconame sub_component_text~descript sub_component~scotype
      INTO TABLE lt_sub_components
      FROM seosubco AS sub_component JOIN seosubcotx AS sub_component_text
      ON sub_component~clsname      = sub_component_text~clsname
          AND sub_component~cmpname = sub_component_text~cmpname
          AND sub_component~sconame = sub_component_text~sconame
      WHERE sub_component~clsname    = iv_clif_name
        AND sub_component_text~langu = iv_language
        AND sub_component_text~descript <> space.      "#EC CI_BUFFJOIN

    LOOP AT lt_components ASSIGNING <ls_component>.
      CLEAR ls_component_exp.
      MOVE-CORRESPONDING <ls_component> TO ls_component_exp.
      INSERT ls_component_exp INTO TABLE lt_components_exp.
    ENDLOOP.

    rs_properties-attributes = get_attributes( lt_components_exp ).
    rs_properties-methods = get_methods( is_components     = lt_components_exp
                                         is_sub_components = lt_sub_components ).
    rs_properties-events = get_events( is_components     = lt_components_exp
                                       is_sub_components = lt_sub_components ).
    rs_properties-types = get_types( lt_components_exp ).

  ENDMETHOD.


  METHOD get_attributes.
    DATA:
      lo_component TYPE zif_abapgit_aff_oo_types_v1=>ty_component_description.
    FIELD-SYMBOLS <lo_attribute> TYPE ty_component.

    LOOP AT is_components ASSIGNING <lo_attribute> WHERE cmptype = seoo_cmptype_attribute AND descript IS NOT INITIAL.
      lo_component-name = <lo_attribute>-cmpname.
      lo_component-description = <lo_attribute>-descript.
      INSERT lo_component INTO TABLE rs_result.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_methods.
    DATA:
      lo_method    TYPE zif_abapgit_aff_oo_types_v1=>ty_method,
      lo_exception TYPE zif_abapgit_aff_oo_types_v1=>ty_component_description,
      lo_parameter TYPE zif_abapgit_aff_oo_types_v1=>ty_component_description.

    FIELD-SYMBOLS <ls_sub_component> TYPE ty_sub_component.
    FIELD-SYMBOLS <ls_component> TYPE ty_component.

    LOOP AT is_components ASSIGNING <ls_component> WHERE cmptype = seoo_cmptype_method.
      lo_method-name = <ls_component>-cmpname.
      lo_method-description = <ls_component>-descript.

      LOOP AT is_sub_components ASSIGNING <ls_sub_component> WHERE cmpname = <ls_component>-cmpname.
        CASE <ls_sub_component>-scotype.
          WHEN seos_scotype_parameter.
            lo_parameter-name = <ls_sub_component>-sconame.
            lo_parameter-description = <ls_sub_component>-descript.
            INSERT lo_parameter INTO TABLE lo_method-parameters.
          WHEN seos_scotype_exception.
            lo_exception-name = <ls_sub_component>-sconame.
            lo_exception-description = <ls_sub_component>-descript.
            INSERT lo_exception INTO TABLE lo_method-exceptions.
        ENDCASE.
      ENDLOOP.

      IF lo_method-description IS NOT INITIAL
          OR lo_method-exceptions IS NOT INITIAL
          OR lo_method-parameters IS NOT INITIAL.
        INSERT lo_method INTO TABLE rs_result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_types.
    DATA:
        lo_type TYPE zif_abapgit_aff_oo_types_v1=>ty_component_description.
    FIELD-SYMBOLS: <ls_types> TYPE ty_component.

    LOOP AT is_components ASSIGNING <ls_types>
        WHERE cmptype = seoo_cmptype_type AND descript IS NOT INITIAL.
      lo_type-name = <ls_types>-cmpname.
      lo_type-description = <ls_types>-descript.
      INSERT lo_type INTO TABLE rs_result.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_events.
    DATA:
      lo_parameter TYPE zif_abapgit_aff_oo_types_v1=>ty_component_description,
      lo_event     TYPE zif_abapgit_aff_oo_types_v1=>ty_event.
    FIELD-SYMBOLS <ls_event> TYPE ty_component.
    FIELD-SYMBOLS <ls_sub_component> TYPE ty_sub_component.

    LOOP AT is_components ASSIGNING <ls_event> WHERE cmptype = seoo_cmptype_event.
      lo_event-name = <ls_event>-cmpname.
      lo_event-description = <ls_event>-descript.

      LOOP AT is_sub_components ASSIGNING <ls_sub_component> WHERE cmpname = <ls_event>-cmpname.
        lo_parameter-name = <ls_sub_component>-sconame.
        lo_parameter-description = <ls_sub_component>-descript.
        INSERT lo_parameter INTO TABLE lo_event-parameters.
      ENDLOOP.

      IF lo_event-description IS NOT INITIAL OR lo_event-parameters IS NOT INITIAL.
        INSERT lo_event INTO TABLE rs_result.
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
      IMPORTING is_clsname          TYPE seoclsname
                is_intf_aff         TYPE zif_abapgit_aff_intf_v1=>ty_main
      EXPORTING et_descriptions     TYPE zif_abapgit_oo_object_fnc=>ty_seocompotx_tt
                et_descriptions_sub TYPE zif_abapgit_oo_object_fnc=>ty_seosubcotx_tt.
ENDCLASS.

CLASS lcl_aff_type_mapping IMPLEMENTATION.

  METHOD zif_abapgit_aff_type_mapping~to_aff.
    DATA:
      ls_data_abapgit TYPE zcl_abapgit_object_intf=>ty_intf,
      ls_data_aff     TYPE zif_abapgit_aff_intf_v1=>ty_main.

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
      iv_language  = ls_data_aff-header-original_language
      iv_clif_name = ls_data_abapgit-vseointerf-clsname ).

    es_data = ls_data_aff.
  ENDMETHOD.

  METHOD zif_abapgit_aff_type_mapping~to_abapgit.
    DATA:
      ls_data_abapgit TYPE zcl_abapgit_object_intf=>ty_intf,
      ls_data_aff     TYPE zif_abapgit_aff_intf_v1=>ty_main,
      lv_classname    TYPE seoclsname.


    ls_data_aff = iv_data.

    lv_classname = to_upper( iv_object_name ).

    set_abapgit_descriptions( EXPORTING is_clsname          = lv_classname
                                        is_intf_aff         = ls_data_aff
                              IMPORTING et_descriptions     = ls_data_abapgit-description
                                        et_descriptions_sub = ls_data_abapgit-description_sub ).

    ls_data_abapgit-vseointerf-clsname  = lv_classname.
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

    DATA ls_description       TYPE seocompotx.
    DATA ls_description_subco TYPE seosubcotx.
    FIELD-SYMBOLS <ls_description>      TYPE zif_abapgit_aff_oo_types_v1=>ty_component_description.
    FIELD-SYMBOLS <ls_meth_description> TYPE zif_abapgit_aff_oo_types_v1=>ty_method.
    FIELD-SYMBOLS <ls_evt_description>  TYPE zif_abapgit_aff_oo_types_v1=>ty_event.


    LOOP AT is_intf_aff-descriptions-types ASSIGNING <ls_description>.
      ls_description-clsname  = is_clsname.
      ls_description-cmpname  = <ls_description>-name.
      ls_description-langu    = is_intf_aff-header-original_language.
      ls_description-descript = <ls_description>-description.
      APPEND ls_description TO et_descriptions.
    ENDLOOP.

    LOOP AT is_intf_aff-descriptions-attributes ASSIGNING <ls_description>.
      ls_description-clsname  = is_clsname.
      ls_description-cmpname  = <ls_description>-name.
      ls_description-langu    = is_intf_aff-header-original_language.
      ls_description-descript = <ls_description>-description.
      APPEND ls_description TO et_descriptions.
    ENDLOOP.

    LOOP AT is_intf_aff-descriptions-methods ASSIGNING <ls_meth_description>.
      ls_description-clsname  = is_clsname.
      ls_description-cmpname  = <ls_meth_description>-name.
      ls_description-langu    = is_intf_aff-header-original_language.
      ls_description-descript = <ls_meth_description>-description.
      APPEND ls_description TO et_descriptions.

      LOOP AT <ls_meth_description>-parameters ASSIGNING <ls_description>.
        ls_description_subco-clsname  = ls_description-clsname.
        ls_description_subco-cmpname  = ls_description-cmpname.
        ls_description_subco-langu    = ls_description-langu.
        ls_description_subco-sconame  = <ls_description>-name.
        ls_description_subco-descript = <ls_description>-description.
        APPEND ls_description_subco TO et_descriptions_sub.
      ENDLOOP.

      LOOP AT <ls_meth_description>-exceptions ASSIGNING <ls_description>.
        ls_description_subco-clsname  = ls_description-clsname.
        ls_description_subco-cmpname  = ls_description-cmpname.
        ls_description_subco-langu    = ls_description-langu.
        ls_description_subco-sconame  = <ls_description>-name.
        ls_description_subco-descript = <ls_description>-description.
        APPEND ls_description_subco TO et_descriptions_sub.
      ENDLOOP.
    ENDLOOP.

    LOOP AT is_intf_aff-descriptions-events ASSIGNING <ls_evt_description>.
      ls_description-clsname  = is_clsname.
      ls_description-cmpname  = <ls_evt_description>-name.
      ls_description-langu    = is_intf_aff-header-original_language.
      ls_description-descript = <ls_evt_description>-description.
      APPEND ls_description TO et_descriptions.

      LOOP AT <ls_evt_description>-parameters ASSIGNING <ls_description>.
        ls_description_subco-clsname  = ls_description-clsname.
        ls_description_subco-cmpname  = ls_description-cmpname.
        ls_description_subco-langu    = ls_description-langu.
        ls_description_subco-sconame  = <ls_description>-name.
        ls_description_subco-descript = <ls_description>-description.
        APPEND ls_description_subco TO et_descriptions_sub.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_aff_metadata_handler DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS serialize
      IMPORTING is_intf          TYPE zcl_abapgit_object_intf=>ty_intf
      RETURNING VALUE(rv_result) TYPE xstring
      RAISING   zcx_abapgit_exception.
    CLASS-METHODS serialize_translations
      IMPORTING is_intf          TYPE zcl_abapgit_object_intf=>ty_intf
                it_language      TYPE zif_abapgit_definitions=>ty_languages
      RETURNING VALUE(rt_result) TYPE zif_abapgit_i18n_file=>ty_table_of
      RAISING   zcx_abapgit_exception.
    CLASS-METHODS deserialize
      IMPORTING iv_data          TYPE xstring
      RETURNING VALUE(rv_result) TYPE zif_abapgit_aff_intf_v1=>ty_main
      RAISING   zcx_abapgit_exception.
    CLASS-METHODS deserialize_translation
      IMPORTING io_files           TYPE REF TO zcl_abapgit_objects_files
      EXPORTING et_description     TYPE zcl_abapgit_object_intf=>ty_intf-description
                et_description_sub TYPE zcl_abapgit_object_intf=>ty_intf-description_sub
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
        RETURNING VALUE(rt_result) TYPE zcl_abapgit_json_handler=>ty_skip_paths,
      fill_translation
        IMPORTING iv_name          TYPE seoclsname
                  iv_language      TYPE laiso
        RETURNING VALUE(rt_result) TYPE zif_abapgit_aff_intf_v1=>ty_main,
      get_string_table
        IMPORTING iv_xstring       TYPE xstring
        RETURNING VALUE(rt_result) TYPE string_table
        RAISING   zcx_abapgit_exception.
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

    lt_default_abap_langu_version-path  = '/header/abap_language_version'.
    lt_default_abap_langu_version-value = zif_abapgit_dot_abapgit=>c_abap_language_version-standard.
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

  METHOD serialize_translations.
    DATA: ls_data        TYPE zif_abapgit_aff_intf_v1=>ty_main,
          lv_langu       TYPE laiso,
          lv_json        TYPE string,
          lo_ajson       TYPE REF TO zif_abapgit_ajson,
          lo_json_path   TYPE REF TO zcl_abapgit_json_path,
          lt_translation TYPE string_table,
          lx_exception   TYPE REF TO zcx_abapgit_ajson_error,
          lo_trans_file  TYPE REF TO zcl_abapgit_properties_file.


    LOOP AT it_language INTO lv_langu.

      ls_data = fill_translation( iv_name  = is_intf-vseointerf-clsname
                                  iv_language = lv_langu ).

      " convert AFF type to JSON
      TRY.
          lo_ajson = zcl_abapgit_ajson=>new( iv_keep_item_order = abap_true
            )->set( iv_path = '/'
                    iv_val  = ls_data
            )->map( zcl_abapgit_ajson_mapping=>create_to_camel_case( )
            )->filter( zcl_abapgit_ajson_filter_lib=>create_empty_filter( ) ).
          " remove manually the non-primitive types that are initial or not relevant for translation
          lo_ajson->delete( '/category/' ).
          lo_ajson->delete( '/proxy/' ).
          lv_json = lo_ajson->stringify( ).
        CATCH zcx_abapgit_ajson_error INTO lx_exception.
          zcx_abapgit_exception=>raise_with_text( lx_exception ).
      ENDTRY.


      CREATE OBJECT lo_json_path.
      lt_translation = lo_json_path->serialize( lv_json ).

      CREATE OBJECT lo_trans_file
        EXPORTING iv_lang = lv_langu.

      lo_trans_file->push_text_pairs( lt_translation ).

      APPEND lo_trans_file TO rt_result.
    ENDLOOP.

  ENDMETHOD.


  METHOD fill_translation.
    DATA: lv_langu_sap1 TYPE sy-langu.

    lv_langu_sap1 = zcl_abapgit_convert=>language_sap2_to_sap1( iv_language ).

    rt_result-descriptions = lcl_aff_helper=>get_descriptions_compo_subco(
      iv_clif_name = iv_name
      iv_language  = lv_langu_sap1 ).

    SELECT SINGLE descript FROM seoclasstx INTO rt_result-header-description
    WHERE clsname = iv_name AND
          langu   = lv_langu_sap1.

  ENDMETHOD.


  METHOD deserialize_translation.
    DATA: lt_data             TYPE string_table,
          lv_xtranslation     TYPE xstring,
          lo_ajson            TYPE REF TO zcl_abapgit_json_handler,
          lx_exception        TYPE REF TO cx_static_check,
          lv_obj_name         TYPE seoclsname,
          lv_langu            TYPE laiso,
          lt_translation_file TYPE zif_abapgit_git_definitions=>ty_files_tt,
          ls_translation_file TYPE zif_abapgit_git_definitions=>ty_file,
          lo_json_path        TYPE REF TO zcl_abapgit_json_path,
          ls_aff_data         TYPE zif_abapgit_aff_intf_v1=>ty_main,
          lo_type_mapper      TYPE REF TO zif_abapgit_aff_type_mapping,
          ls_ag_data          TYPE zcl_abapgit_object_intf=>ty_intf.

    lt_translation_file = io_files->get_i18n_properties_file( ).
    CREATE OBJECT lo_json_path.

    LOOP AT lt_translation_file INTO ls_translation_file.

      CLEAR ls_ag_data.

      lt_data = get_string_table( ls_translation_file-data ).
      lv_xtranslation = lo_json_path->deserialize( lt_data ).

      CREATE OBJECT lo_ajson.
      TRY.
          lo_ajson->deserialize(
            EXPORTING
              iv_content = lv_xtranslation
            IMPORTING
              ev_data    = ls_aff_data ).
        CATCH cx_static_check INTO lx_exception.
          zcx_abapgit_exception=>raise_with_text( lx_exception ).
      ENDTRY.

      FIND FIRST OCCURRENCE OF REGEX '^([\w]+).*\.i18n.([a-z]{2})\.'
        IN ls_translation_file-filename SUBMATCHES lv_obj_name lv_langu.

      ls_aff_data-header-original_language = to_upper( lv_langu ). " is target language

      CREATE OBJECT lo_type_mapper TYPE lcl_aff_type_mapping.
      lo_type_mapper->to_abapgit(
        EXPORTING
          iv_data        = ls_aff_data
          iv_object_name = to_upper( lv_obj_name )
        IMPORTING
          es_data        = ls_ag_data ).

      APPEND LINES OF ls_ag_data-description TO et_description.
      APPEND LINES OF ls_ag_data-description_sub TO et_description_sub.

    ENDLOOP.


  ENDMETHOD.


  METHOD get_string_table.

    DATA lv_data TYPE string.

    lv_data = zcl_abapgit_convert=>xstring_to_string_utf8( iv_xstring ).
    SPLIT lv_data AT cl_abap_char_utilities=>newline INTO TABLE rt_result.

  ENDMETHOD.

ENDCLASS.
