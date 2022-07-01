*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_aff_helper DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      create_empty_class
        IMPORTING iv_class_name TYPE seoclsname
                  io_lock_handle   TYPE REF TO if_adt_lock_handle
        RAISING   cx_aff_root,
      create_empty_interface
        IMPORTING iv_intf_name   TYPE seoclsname
                  io_lock_handle TYPE REF TO if_adt_lock_handle
        RAISING   cx_aff_root,
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

  METHOD create_empty_class.
    DATA(empty_class) = VALUE vseoclass(
      clsname         = iv_class_name
      version         = seoc_version_active
      langu           = sy-langu
      descript        = space
      category        = seoc_category_general
      exposure        = seoc_exposure_public
      state           = seoc_state_implemented
      fixpt           = abap_true
      clsccincl       = abap_true
      with_unit_tests = abap_false ).

    CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
      EXPORTING
        version       = seoc_version_active
        suppress_corr = abap_true
        lock_handle   = io_lock_handle
      CHANGING
        class         = empty_class
      EXCEPTIONS
        OTHERS        = 1.
    IF sy-subrc <> 0.
      IF sy-msgid IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(class_error).
      ELSE.
        class_error = 'Internal error' ##NO_TEXT.
      ENDIF.
      RAISE EXCEPTION TYPE cx_aff_root MESSAGE e023(seo_aff) WITH iv_class_name class_error.
    ENDIF.
  ENDMETHOD.


  METHOD create_empty_interface.
    DATA(empty_interface) = VALUE vseointerf(
      clsname  = iv_intf_name
      version  = seoc_version_active
      langu    = sy-langu
      descript = space
      state    = seoc_state_implemented
      exposure = seoc_exposure_public ).

    CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
      EXPORTING
        version       = seoc_version_active
        suppress_corr = abap_true
        lock_handle   = io_lock_handle
      CHANGING
        interface     = empty_interface
      EXCEPTIONS
        OTHERS        = 1.
    IF sy-subrc <> 0.
      IF sy-msgid IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(interface_error).
      ELSE.
        interface_error = 'Internal error' ##NO_TEXT.
      ENDIF.
      RAISE EXCEPTION TYPE cx_aff_root MESSAGE e021(seo_aff) WITH iv_intf_name interface_error.
    ENDIF.
  ENDMETHOD.


  METHOD generate_class_pool.
    DATA(cifkey) = VALUE seoclskey( clsname = iv_class_name ).
    PERFORM set_wbinactive IN PROGRAM saplseok USING ' '.
    CALL FUNCTION 'SEO_WBINACTIVE_BROADCAST'
      EXPORTING
        wbia = ' '.
    CALL FUNCTION 'SEO_CLIF_SET_WBINACTIVE'
      EXPORTING
        wbia = ' '.

    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        cifkey  = cifkey
        version = seoc_version_active.

    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        cifkey  = cifkey
        version = seoc_version_inactive.

    CALL FUNCTION 'SEO_CLASS_GENERATE_CLASSPOOL'
      EXPORTING
        clskey        = cifkey
        suppress_corr = seox_true
      EXCEPTIONS
        OTHERS        = 1 ##FM_SUBRC_OK.
  ENDMETHOD.


  METHOD get_descr_comp_subc_w_exposure.
    SELECT FROM seocompo AS component LEFT OUTER JOIN seocompotx AS component_text
      ON component~cmpname = component_text~cmpname AND component~clsname = component_text~clsname AND
         component_text~langu = @iv_language
      INNER JOIN seocompodf AS df
      ON component~clsname = df~clsname AND
         component~cmpname = df~cmpname
      FIELDS component~cmpname, component_text~descript, component~cmptype, df~exposure AS visibility
      WHERE component~clsname = @iv_clif_name AND
            df~exposure       = @iv_exposure
      INTO TABLE @DATA(components).                    "#EC CI_BUFFJOIN

    SELECT FROM seosubco AS sub_component JOIN seosubcotx AS sub_component_text
      ON  sub_component~clsname = sub_component_text~clsname
          AND sub_component~cmpname = sub_component_text~cmpname
          AND sub_component~sconame = sub_component_text~sconame
      INNER JOIN seocompodf AS df
      ON sub_component~clsname = df~clsname AND
         sub_component~cmpname = df~cmpname
      FIELDS sub_component~cmpname, sub_component~sconame, sub_component_text~descript, sub_component~scotype, df~exposure AS visibility
      WHERE sub_component~clsname    = @iv_clif_name
        AND df~exposure              = @iv_exposure
        AND sub_component_text~langu = @iv_language
        AND sub_component_text~descript IS NOT INITIAL
     INTO TABLE @DATA(sub_components).                 "#EC CI_BUFFJOIN

    rs_properties-attributes = get_attributes( is_components = CORRESPONDING #( components ) ).
    rs_properties-methods = get_methods( is_components = CORRESPONDING #( components ) is_sub_components = CORRESPONDING #( sub_components ) ).
    rs_properties-events = get_events( is_components = CORRESPONDING #( components ) is_sub_components = CORRESPONDING #( sub_components ) ).
    rs_properties-types = get_types( is_components = CORRESPONDING #( components ) ).
  ENDMETHOD.


  METHOD get_descriptions_compo_subco.
    SELECT FROM seocompo AS component LEFT OUTER JOIN seocompotx AS component_text
      ON component~cmpname = component_text~cmpname AND component~clsname = component_text~clsname AND component_text~langu = @iv_language
      FIELDS component~cmpname, component_text~descript, component~cmptype
      WHERE component~clsname = @iv_clif_name
      INTO TABLE @DATA(components).                    "#EC CI_BUFFJOIN

    SELECT FROM seosubco AS sub_component JOIN seosubcotx AS sub_component_text
      ON  sub_component~clsname = sub_component_text~clsname
          AND sub_component~cmpname = sub_component_text~cmpname
          AND sub_component~sconame = sub_component_text~sconame
      FIELDS sub_component~cmpname, sub_component~sconame, sub_component_text~descript, sub_component~scotype
      WHERE sub_component~clsname    = @iv_clif_name
        AND sub_component_text~langu = @iv_language
        AND sub_component_text~descript IS NOT INITIAL
      INTO TABLE @DATA(sub_components).                "#EC CI_BUFFJOIN

    rs_properties-attributes = get_attributes( CORRESPONDING #( components ) ).
    rs_properties-methods = get_methods( is_components = CORRESPONDING #( components )  is_sub_components = sub_components ).
    rs_properties-events = get_events( is_components = CORRESPONDING #( components )  is_sub_components = sub_components ).
    rs_properties-types = get_types( CORRESPONDING #( components )  ).

  ENDMETHOD.


  METHOD get_attributes.
    LOOP AT is_components ASSIGNING FIELD-SYMBOL(<attribute>) WHERE cmptype = seoo_cmptype_attribute AND descript IS NOT INITIAL.
      INSERT VALUE #( name = <attribute>-cmpname description = <attribute>-descript ) INTO TABLE rs_result.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_methods.
    LOOP AT is_components ASSIGNING FIELD-SYMBOL(<method>) WHERE cmptype = seoo_cmptype_method.
      DATA(method) = VALUE zif_abapgit_aff_oo_types_v1=>ty_method( name = <method>-cmpname description = <method>-descript ).

      LOOP AT is_sub_components ASSIGNING FIELD-SYMBOL(<sub_component>) WHERE cmpname = <method>-cmpname.
        CASE <sub_component>-scotype.
          WHEN seos_scotype_parameter.
            INSERT VALUE #( name = <sub_component>-sconame description = <sub_component>-descript ) INTO TABLE method-parameters.
          WHEN seos_scotype_exception.
            INSERT VALUE #( name = <sub_component>-sconame description = <sub_component>-descript ) INTO TABLE method-exceptions.
        ENDCASE.
      ENDLOOP.

      IF method-description IS NOT INITIAL OR method-exceptions IS NOT INITIAL OR method-parameters IS NOT INITIAL.
        INSERT method INTO TABLE rs_result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_types.
    LOOP AT is_components ASSIGNING FIELD-SYMBOL(<types>) WHERE cmptype = seoo_cmptype_type AND descript IS NOT INITIAL.
      INSERT VALUE #( name = <types>-cmpname description = <types>-descript ) INTO TABLE rs_result.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_events.
    LOOP AT is_components ASSIGNING FIELD-SYMBOL(<event>) WHERE cmptype = seoo_cmptype_event.
      DATA(event) = VALUE if_oo_aff_types_v1=>ty_event( name = <event>-cmpname description = <event>-descript ).

      LOOP AT is_sub_components ASSIGNING FIELD-SYMBOL(<sub_component>) WHERE cmpname = <event>-cmpname.
        INSERT VALUE #( name = <sub_component>-sconame description = <sub_component>-descript ) INTO TABLE event-parameters.
      ENDLOOP.

      IF event-description IS NOT INITIAL OR event-parameters IS NOT INITIAL.
        INSERT event INTO TABLE rs_result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_attributes.
    LOOP AT is_properties-attributes ASSIGNING FIELD-SYMBOL(<attribute>).
      DATA(attribute) = VALUE seocompotx(
          clsname  = iv_clif_name
          cmpname  = <attribute>-name
          langu    = iv_language
          descript = <attribute>-description ).
      MODIFY seocompotx FROM attribute.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_methods.
    LOOP AT is_properties-methods ASSIGNING FIELD-SYMBOL(<method>).
      DATA(method) = VALUE seocompotx(
        clsname  = iv_clif_name
        cmpname  = <method>-name
        langu    = iv_language
        descript = <method>-description ).
      MODIFY seocompotx FROM method.

      LOOP AT <method>-parameters ASSIGNING FIELD-SYMBOL(<parameter>).
        DATA(method_parameter) = VALUE seosubcotx(
          clsname  = iv_clif_name
          cmpname  = <method>-name
          sconame  = <parameter>-name
          langu    = iv_language
          descript = <parameter>-description ).
        MODIFY seosubcotx FROM method_parameter.
      ENDLOOP.

      LOOP AT <method>-exceptions ASSIGNING FIELD-SYMBOL(<exception>).
        DATA(exception) = VALUE seosubcotx(
          clsname  = iv_clif_name
          cmpname  = <method>-name
          sconame  = <exception>-name
          langu    = iv_language
          descript = <exception>-description ).
        MODIFY seosubcotx FROM exception.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_events.
    LOOP AT is_properties-events ASSIGNING FIELD-SYMBOL(<event>).
      DATA(event) = VALUE seocompotx(
        clsname  = iv_clif_name
        cmpname  = <event>-name
        langu    = iv_language
        descript = <event>-description ).
      MODIFY seocompotx FROM event.

      LOOP AT <event>-parameters ASSIGNING FIELD-SYMBOL(<parameter>).
        DATA(evet_parameter) = VALUE seosubcotx(
          clsname  = iv_clif_name
          cmpname  = <event>-name
          sconame  = <parameter>-name
          langu    = iv_language
          descript = <parameter>-description ).
        MODIFY seosubcotx FROM evet_parameter.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_types.
    LOOP AT is_properties-types ASSIGNING FIELD-SYMBOL(<type>).
      DATA(type) = VALUE seocompotx(
        clsname  = iv_clif_name
        cmpname  = <type>-name
        langu    = iv_language
        descript = <type>-description ).
      MODIFY seocompotx FROM type.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_descriptions_compo_subco.
    set_attributes( is_properties = is_properties iv_clif_name = iv_clif_name iv_language = iv_language ).
    set_methods( is_properties = is_properties iv_clif_name = iv_clif_name iv_language = iv_language ).
    set_events( is_properties = is_properties iv_clif_name = iv_clif_name iv_language = iv_language ).
    set_types( is_properties = is_properties iv_clif_name = iv_clif_name iv_language = iv_language ).
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
      ls_data_abapgit TYPE zcl_abapgit_object_intf=>ty_intf,
      ls_data_aff     TYPE zif_abapgit_aff_intf_v1=>ty_main,
      lx_exception    TYPE REF TO cx_root,
      lo_ajson        TYPE REF TO zcl_abapgit_json_handler,
      lo_aff_mapper   TYPE REF TO zif_abapgit_aff_type_mapping.

    ls_data_abapgit = is_intf.

    CREATE OBJECT lo_aff_mapper TYPE lcl_aff_type_mapping.
    lo_aff_mapper->to_aff( EXPORTING iv_data = ls_data_abapgit
                           IMPORTING es_data = ls_data_aff ).
    CREATE OBJECT lo_ajson.
    TRY.
        rv_result = lo_ajson->serialize( ls_data_aff ).
      CATCH cx_root INTO lx_exception.
        zcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
