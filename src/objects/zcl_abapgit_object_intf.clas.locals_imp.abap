CLASS lcl_aff_helper DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: create_empty_interface
      IMPORTING iv_intf_name   TYPE seoclsname
                io_lock_handle TYPE REF TO if_adt_lock_handle
      RAISING   zcx_abapgit_exception,
      generate_class_pool
        IMPORTING iv_class_name TYPE seoclsname,
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
      ty_compontents     TYPE STANDARD TABLE OF ty_component,
      ty_sub_compontents TYPE STANDARD TABLE OF ty_sub_component.

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


  METHOD create_empty_interface.
    DATA:
      lo_interface_error TYPE string,
      ls_empty_interface TYPE vseointerf.

    ls_empty_interface-clsname = iv_intf_name.
    ls_empty_interface-version = seoc_version_active.
    ls_empty_interface-langu = sy-langu.
    ls_empty_interface-descript = space.
    ls_empty_interface-state = seoc_state_implemented.
    ls_empty_interface-exposure = seoc_exposure_public.

    CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
      EXPORTING
        version       = seoc_version_active
        suppress_corr = abap_true
        lock_handle   = io_lock_handle
      CHANGING
        interface     = ls_empty_interface
      EXCEPTIONS
        OTHERS        = 1.
    IF sy-subrc <> 0.
      IF sy-msgid IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lo_interface_error.
      ELSE.
        lo_interface_error = 'Internal error'.
      ENDIF.
      " todo: raise exception here
    ENDIF.
  ENDMETHOD.


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
      ON  sub_component~clsname = sub_component_text~clsname
          AND sub_component~cmpname = sub_component_text~cmpname
          AND sub_component~sconame = sub_component_text~sconame
      INNER JOIN seocompodf AS df
      ON sub_component~clsname = df~clsname AND
         sub_component~cmpname = df~cmpname
      WHERE sub_component~clsname    = iv_clif_name
        AND df~exposure              = iv_exposure
        AND sub_component_text~langu = iv_language
        AND sub_component_text~descript <> space.      "#EC CI_BUFFJOIN



    rs_properties-attributes = get_attributes( lt_components ).
    rs_properties-methods = get_methods( is_components = lt_components
                                         is_sub_components = lt_sub_components ).
    rs_properties-events = get_events( is_components = lt_components
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
      lt_components_exp TYPE ty_compontents.


    SELECT component~cmpname component_text~descript component~cmptype
      INTO TABLE lt_components
      FROM seocompo AS component
     LEFT OUTER JOIN seocompotx AS component_text
      ON component~cmpname = component_text~cmpname AND component~clsname    = component_text~clsname
                                                    AND component_text~langu = iv_language
      WHERE component~clsname = iv_clif_name.          "#EC CI_BUFFJOIN

    SELECT sub_component~cmpname sub_component~sconame sub_component_text~descript sub_component~scotype
      INTO TABLE lt_sub_components
      FROM seosubco AS sub_component JOIN seosubcotx AS sub_component_text
      ON sub_component~clsname      = sub_component_text~clsname
          AND sub_component~cmpname = sub_component_text~cmpname
          AND sub_component~sconame = sub_component_text~sconame
      WHERE sub_component~clsname    = iv_clif_name
        AND sub_component_text~langu = iv_language
        AND sub_component_text~descript <> space.      "#EC CI_BUFFJOIN

    MOVE-CORRESPONDING lt_components TO lt_components_exp.

    rs_properties-attributes = get_attributes( lt_components_exp ).
    rs_properties-methods = get_methods( is_components = lt_components_exp
                                         is_sub_components = lt_sub_components ).
    rs_properties-events = get_events( is_components = lt_components_exp
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
                    iv_clif_name = iv_clif_name
                    iv_language = iv_language ).
    set_methods( is_properties = is_properties
                 iv_clif_name = iv_clif_name
                 iv_language = iv_language ).
    set_events( is_properties = is_properties
                iv_clif_name = iv_clif_name
                iv_language = iv_language ).
    set_types( is_properties = is_properties
               iv_clif_name = iv_clif_name
               iv_language = iv_language ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_aff_type_mapping DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_aff_type_mapping.
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
    SELECT SINGLE category clsproxy AS proxy
           FROM vseointerf
           INTO (ls_data_aff-category, ls_data_aff-proxy)
           WHERE clsname = ls_data_abapgit-vseointerf-clsname AND version = '1' AND
                 langu = ls_data_aff-header-original_language.

    " get descriptions
    ls_data_aff-descriptions = lcl_aff_helper=>get_descriptions_compo_subco(
                              iv_language  = ls_data_aff-header-original_language
                              iv_clif_name = ls_data_abapgit-vseointerf-clsname ).

    es_data = ls_data_aff.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_paths_filter DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_ajson_filter.
    METHODS constructor
      IMPORTING
        is_aff_intf_v1 TYPE zif_abapgit_aff_intf_v1=>ty_main
      RAISING
        zcx_abapgit_ajson_error.
    TYPES:
      BEGIN OF ty_key_value,
        key   TYPE string,
        value TYPE string,
      END OF ty_key_value.

  PRIVATE SECTION.
    DATA mt_skip_paths TYPE STANDARD TABLE OF ty_key_value WITH Key key.
ENDCLASS.

CLASS lcl_paths_filter IMPLEMENTATION.

  METHOD zif_abapgit_ajson_filter~keep_node.

    DATA lv_path TYPE string.

    lv_path = is_node-path && is_node-name.

    IF line_exists( mt_skip_paths[ key = lv_path value = is_node-value ] )
      AND iv_visit = zif_abapgit_ajson_filter=>visit_type-value.
      RETURN abap_false.
    ELSEIF line_exists( mt_skip_paths[ key = lv_path ] )
      AND iv_visit = zif_abapgit_ajson_filter=>visit_type-value.
      RETURN abap_true.
    ENDIF.

    IF is_node-type = 'bool' AND is_node-value = 'false' AND iv_visit = zif_abapgit_ajson_filter=>visit_type-value.
      RETURN abap_false.
    ENDIF.


    IF NOT ( ( iv_visit = zif_abapgit_ajson_filter=>visit_type-value AND is_node-value IS NOT INITIAL ) OR
         ( iv_visit <> zif_abapgit_ajson_filter=>visit_type-value AND is_node-children > 0 ) ).
      RETURN abap_false.
    ENDIF.

    RETURN abap_true.

  ENDMETHOD.

  METHOD constructor.
    " extract annotations and build table for values to be skipped ( path/name | value )
    APPEND VALUE #( key = `/header/abapLanguageVersion` value = 'X' ) TO mt_skip_paths.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_aff_serialize_metadata DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS serialize
      IMPORTING is_intf          TYPE data
      RETURNING VALUE(rv_result) TYPE xstring
      RAISING   zcx_abapgit_exception.
ENDCLASS.

CLASS lcl_aff_serialize_metadata IMPLEMENTATION.

  METHOD serialize.
    DATA:
      ls_data_abapgit  TYPE zcl_abapgit_object_intf=>ty_intf,
      ls_data_aff      TYPE zif_abapgit_aff_intf_v1=>ty_main,
      lx_exception     TYPE REF TO cx_root,
      lo_ajson         TYPE REF TO zcl_abapgit_json_handler,
      lcl_paths_filter TYPE REF TO lcl_paths_filter,
      lo_aff_mapper    TYPE REF TO zif_abapgit_aff_type_mapping.

    ls_data_abapgit = is_intf.

    CREATE OBJECT lo_aff_mapper TYPE lcl_aff_type_mapping.
    lo_aff_mapper->to_aff( EXPORTING iv_data = ls_data_abapgit
                           IMPORTING es_data = ls_data_aff ).

    TRY.
        lcl_paths_filter = NEW lcl_paths_filter( ls_data_aff ).
      CATCH zcx_abapgit_ajson_error INTO DATA(exception).
        " todo: exception handling
    ENDTRY.

    CREATE OBJECT lo_ajson.
    TRY.
        rv_result = lo_ajson->serialize( iv_data = ls_data_aff
                                         io_filter = lcl_paths_filter ).
      CATCH cx_root INTO lx_exception.
        zcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
