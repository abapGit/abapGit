CLASS zcl_abapgit_object_wdyn DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_longtext_id_wc TYPE dokil-id VALUE 'WC' ##NO_TEXT.
    CONSTANTS c_longtext_id_wd TYPE dokil-id VALUE 'WD' ##NO_TEXT.
    CONSTANTS c_longtext_name_wc TYPE string VALUE 'LONGTEXTS_WC' ##NO_TEXT.

    DATA:
      mt_components TYPE TABLE OF wdy_ctlr_compo_vrs,
      mt_sources    TYPE TABLE OF wdy_ctlr_compo_source_vrs.

    METHODS:
      get_limu_objects
        RETURNING VALUE(rt_objects) TYPE wdy_md_transport_keys,
      read
        RETURNING VALUE(rs_component) TYPE wdy_component_metadata
        RAISING   zcx_abapgit_exception,
      read_controller
        IMPORTING is_key               TYPE wdy_md_controller_key
        RETURNING VALUE(rs_controller) TYPE wdy_md_controller_meta_data
        RAISING   zcx_abapgit_exception,
      read_definition
        IMPORTING is_key               TYPE wdy_md_component_key
        RETURNING VALUE(rs_definition) TYPE wdy_md_component_meta_data
        RAISING   zcx_abapgit_exception,
      read_view
        IMPORTING is_key         TYPE wdy_md_view_key
        RETURNING VALUE(rs_view) TYPE wdy_md_view_meta_data
        RAISING   zcx_abapgit_exception,
      recover_controller
        IMPORTING is_controller TYPE wdy_md_controller_meta_data
        RAISING   zcx_abapgit_exception,
      recover_definition
        IMPORTING is_definition TYPE wdy_md_component_meta_data
                  iv_package    TYPE devclass
        RAISING   zcx_abapgit_exception,
      recover_view
        IMPORTING is_view TYPE wdy_md_view_meta_data
        RAISING   zcx_abapgit_exception,
      delta_controller
        IMPORTING is_controller   TYPE wdy_md_controller_meta_data
        RETURNING VALUE(rs_delta) TYPE svrs2_xversionable_object
        RAISING   zcx_abapgit_exception,
      delta_definition
        IMPORTING is_definition     TYPE wdy_md_component_meta_data
                  VALUE(iv_package) TYPE devclass
        RETURNING VALUE(rs_delta)   TYPE svrs2_xversionable_object
        RAISING   zcx_abapgit_exception,
      delta_view
        IMPORTING is_view         TYPE wdy_md_view_meta_data
        RETURNING VALUE(rs_delta) TYPE svrs2_xversionable_object
        RAISING   zcx_abapgit_exception,
      add_fm_param_exporting
        IMPORTING iv_name  TYPE string
                  ig_value TYPE any
        CHANGING  ct_param TYPE abap_func_parmbind_tab,
      add_fm_param_tables
        IMPORTING iv_name  TYPE string
        CHANGING  ct_value TYPE ANY TABLE
                  ct_param TYPE abap_func_parmbind_tab,
      add_fm_exception
        IMPORTING iv_name      TYPE string
                  iv_value     TYPE i
        CHANGING  ct_exception TYPE abap_func_excpbind_tab,
      add_with_inactive_parts
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_object_wdyn IMPLEMENTATION.


  METHOD add_fm_exception.

    DATA: ls_exception LIKE LINE OF ct_exception.

    ls_exception-name = iv_name.
    ls_exception-value = iv_value.

    INSERT ls_exception INTO TABLE ct_exception.

  ENDMETHOD.


  METHOD add_fm_param_exporting.

    DATA: ls_param LIKE LINE OF ct_param.

    ls_param-kind = abap_func_exporting.
    ls_param-name = iv_name.
    GET REFERENCE OF ig_value INTO ls_param-value.

    INSERT ls_param INTO TABLE ct_param.

  ENDMETHOD.


  METHOD add_fm_param_tables.

    DATA: ls_param LIKE LINE OF ct_param.

    ls_param-kind = abap_func_tables.
    ls_param-name = iv_name.
    GET REFERENCE OF ct_value INTO ls_param-value.

    INSERT ls_param INTO TABLE ct_param.

  ENDMETHOD.


  METHOD add_with_inactive_parts.

    DATA:
      lv_obj_name TYPE trobj_name,
      lv_object   TYPE trobjtype,
      lt_objects  TYPE dwinactiv_tab.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF lt_objects.

    lv_obj_name = ms_item-obj_name.
    lv_object = ms_item-obj_type.

    CALL FUNCTION 'RS_INACTIVE_OBJECTS_IN_OBJECT'
      EXPORTING
        obj_name         = lv_obj_name
        object           = lv_object
      TABLES
        inactive_objects = lt_objects
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    LOOP AT lt_objects ASSIGNING <ls_object>.
      zcl_abapgit_objects_activation=>add( iv_type = <ls_object>-object
                                           iv_name = <ls_object>-obj_name ).
    ENDLOOP.

  ENDMETHOD.


  METHOD delta_controller.

    DATA: li_controller TYPE REF TO if_wdy_md_controller,
          lx_error      TYPE REF TO cx_wdy_md_exception,
          lv_found      TYPE abap_bool,
          ls_key        TYPE wdy_md_controller_key,
          ls_obj_new    TYPE svrs2_versionable_object,
          ls_obj_old    TYPE svrs2_versionable_object.

    FIELD-SYMBOLS: <ls_component>            LIKE LINE OF mt_components,
                   <ls_source>               LIKE LINE OF mt_sources,
                   <lt_ctrl_exceptions>      TYPE ANY TABLE,
                   <lt_ctrl_exception_texts> TYPE ANY TABLE,
                   <lt_excp>                 TYPE ANY TABLE,
                   <lt_excpt>                TYPE ANY TABLE.


    ls_key-component_name = is_controller-definition-component_name.
    ls_key-controller_name = is_controller-definition-controller_name.

    lv_found = cl_wdy_md_controller=>check_existency(
          component_name  = ls_key-component_name
          controller_name = ls_key-controller_name ).
    IF lv_found = abap_false.
      TRY.
          li_controller ?= cl_wdy_md_controller=>create_complete(
                component_name  = ls_key-component_name
                controller_name = ls_key-controller_name
                controller_type = is_controller-definition-controller_type ).
          li_controller->save_to_database( ).
          li_controller->unlock( ).
        CATCH cx_wdy_md_exception INTO lx_error.
          zcx_abapgit_exception=>raise( |Error creating dummy controller: { lx_error->get_text( ) }| ).
      ENDTRY.
    ENDIF.

    ls_obj_new-objtype = wdyn_limu_component_controller.
    ls_obj_new-objname = ls_key.

    ls_obj_old-objtype = wdyn_limu_component_controller.
    ls_obj_old-objname = ls_key.

    APPEND is_controller-definition TO ls_obj_old-wdyc-defin.

    LOOP AT mt_components ASSIGNING <ls_component>
        WHERE component_name = ls_key-component_name
        AND controller_name = ls_key-controller_name.
      APPEND <ls_component> TO ls_obj_old-wdyc-ccomp.
    ENDLOOP.
    LOOP AT mt_sources ASSIGNING <ls_source>
        WHERE component_name = ls_key-component_name
        AND controller_name = ls_key-controller_name.
      APPEND <ls_source> TO ls_obj_old-wdyc-ccoms.
    ENDLOOP.

    ls_obj_old-wdyc-descr = is_controller-descriptions.
    ls_obj_old-wdyc-cusag = is_controller-controller_usages.
    ls_obj_old-wdyc-ccomt = is_controller-controller_component_texts.
    ls_obj_old-wdyc-cpara = is_controller-controller_parameters.
    ls_obj_old-wdyc-cpart = is_controller-controller_parameter_texts.
    ls_obj_old-wdyc-cnode = is_controller-context_nodes.
    ls_obj_old-wdyc-cattr = is_controller-context_attributes.
    ls_obj_old-wdyc-cmapp = is_controller-context_mappings.
*   Version 702 doesn't have these two attributes so we
*   use them dynamically for downward compatibility
    ASSIGN COMPONENT 'CONTROLLER_EXCEPTIONS' OF STRUCTURE is_controller
      TO <lt_ctrl_exceptions>.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'EXCP' OF STRUCTURE ls_obj_old-wdyc TO <lt_excp>.
      IF sy-subrc = 0.
        <lt_excp> = <lt_ctrl_exceptions>.
      ENDIF.
    ENDIF.
    ASSIGN COMPONENT 'CONTROLLER_EXCEPTIONS_TEXTS' OF STRUCTURE is_controller
      TO <lt_ctrl_exception_texts>.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'EXCPT' OF STRUCTURE ls_obj_old-wdyc TO <lt_excpt>.
      IF sy-subrc = 0.
        <lt_excpt> = <lt_ctrl_exception_texts>.
      ENDIF.
    ENDIF.
    ls_obj_old-wdyc-fgrps = is_controller-fieldgroups.

    CALL FUNCTION 'SVRS_MAKE_OBJECT_DELTA'
      EXPORTING
        obj_old              = ls_obj_new
        obj_new              = ls_obj_old
      CHANGING
        delta                = rs_delta
      EXCEPTIONS
        inconsistent_objects = 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SVRS_MAKE_OBJECT_DELTA' ).
    ENDIF.

  ENDMETHOD.


  METHOD delta_definition.

    DATA: ls_key       TYPE wdy_md_component_key,
          lv_found     TYPE abap_bool,
          ls_obj_new   TYPE svrs2_versionable_object,
          li_component TYPE REF TO if_wdy_md_component,
          lx_error     TYPE REF TO cx_wdy_md_exception,
          ls_obj_old   TYPE svrs2_versionable_object.


    ls_key-component_name = is_definition-definition-component_name.

    lv_found = cl_wdy_md_component=>check_existency( ls_key-component_name ).
    IF lv_found = abap_false.
      TRY.
          cl_wdy_md_component=>create_complete(
            EXPORTING
              name      = ls_key-component_name
            IMPORTING
              component = li_component
            CHANGING
              devclass  = iv_package ).
          li_component->save_to_database( ).
          li_component->unlock( ).
        CATCH cx_wdy_md_exception INTO lx_error.
          zcx_abapgit_exception=>raise( |Error creating dummy component: { lx_error->get_text( ) }| ).
      ENDTRY.
    ENDIF.

    ls_obj_new-objtype = wdyn_limu_component_definition.
    ls_obj_new-objname = ls_key-component_name.

    ls_obj_old-objtype = wdyn_limu_component_definition.
    ls_obj_old-objname = ls_key-component_name.

    APPEND is_definition-definition TO ls_obj_old-wdyd-defin.
    ls_obj_old-wdyd-descr = is_definition-descriptions.
    ls_obj_old-wdyd-cusag = is_definition-component_usages.
    ls_obj_old-wdyd-intrf = is_definition-interface_implementings.
    ls_obj_old-wdyd-libra = is_definition-library_usages.
    ls_obj_old-wdyd-ctuse = is_definition-ext_ctlr_usages.
    ls_obj_old-wdyd-ctmap = is_definition-ext_ctx_mappings.

    CALL FUNCTION 'SVRS_MAKE_OBJECT_DELTA'
      EXPORTING
        obj_old              = ls_obj_new
        obj_new              = ls_obj_old
      CHANGING
        delta                = rs_delta
      EXCEPTIONS
        inconsistent_objects = 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SVRS_MAKE_OBJECT_DELTA' ).
    ENDIF.

  ENDMETHOD.


  METHOD delta_view.

    DATA: ls_key     TYPE wdy_md_view_key,
          ls_obj_new TYPE svrs2_versionable_object,
          ls_obj_old TYPE svrs2_versionable_object,
          lv_found   TYPE abap_bool,
          lx_error   TYPE REF TO cx_wdy_md_exception,
          li_view    TYPE REF TO if_wdy_md_abstract_view.

    FIELD-SYMBOLS: <ls_def> LIKE LINE OF ls_obj_old-wdyv-defin.


    ls_key-component_name = is_view-definition-component_name.
    ls_key-view_name      = is_view-definition-view_name.

    lv_found = cl_wdy_md_abstract_view=>check_existency(
                 component_name = ls_key-component_name
                 name           = ls_key-view_name ).
    IF lv_found = abap_false.
      TRY.
          li_view = cl_wdy_md_abstract_view=>create(
                      component_name = is_view-definition-component_name
                      view_name      = is_view-definition-view_name
                      type           = is_view-definition-type ).
          li_view->save_to_database( ).
          li_view->unlock( ).
        CATCH cx_wdy_md_exception INTO lx_error.
          zcx_abapgit_exception=>raise( |Error creating dummy view: { lx_error->get_text( ) }| ).
      ENDTRY.
    ENDIF.

    ls_obj_new-objtype = wdyn_limu_component_view.
    ls_obj_new-objname = ls_key.

    ls_obj_old-objtype = wdyn_limu_component_view.
    ls_obj_old-objname = ls_key.

    APPEND INITIAL LINE TO ls_obj_old-wdyv-defin ASSIGNING <ls_def>.
    MOVE-CORRESPONDING is_view-definition TO <ls_def>.

    ls_obj_old-wdyv-descr = is_view-descriptions.
    ls_obj_old-wdyv-vcont = is_view-view_containers.
    ls_obj_old-wdyv-vcntt = is_view-view_container_texts.
    ls_obj_old-wdyv-ibplg = is_view-iobound_plugs.
    ls_obj_old-wdyv-ibplt = is_view-iobound_plug_texts.
    ls_obj_old-wdyv-plpar = is_view-plug_parameters.
    ls_obj_old-wdyv-plprt = is_view-plug_parameter_texts.
    ls_obj_old-wdyv-uiele = is_view-ui_elements.
    ls_obj_old-wdyv-uicon = is_view-ui_context_bindings.
    ls_obj_old-wdyv-uievt = is_view-ui_event_bindings.
    ls_obj_old-wdyv-uiddc = is_view-ui_ddic_bindings.
    ls_obj_old-wdyv-uiprp = is_view-ui_properties.
    ls_obj_old-wdyv-navil = is_view-navigation_links.
    ls_obj_old-wdyv-navit = is_view-navigation_target_refs.
    ls_obj_old-wdyv-vshno = is_view-vsh_nodes.
    ls_obj_old-wdyv-vshpl = is_view-vsh_placeholders.
    ls_obj_old-wdyv-views = is_view-viewset_properties.

    CALL FUNCTION 'SVRS_MAKE_OBJECT_DELTA'
      EXPORTING
        obj_old              = ls_obj_new
        obj_new              = ls_obj_old
      CHANGING
        delta                = rs_delta
      EXCEPTIONS
        inconsistent_objects = 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SVRS_MAKE_OBJECT_DELTA' ).
    ENDIF.

  ENDMETHOD.


  METHOD get_limu_objects.

    DATA: lv_name TYPE wdy_component_name.


    lv_name = ms_item-obj_name.
    CALL FUNCTION 'WDYN_GET_LIMU_OBJECTS'
      EXPORTING
        component_name = lv_name
      IMPORTING
        limu_objects   = rt_objects.

  ENDMETHOD.


  METHOD read.

    DATA: lt_objects        TYPE wdy_md_transport_keys,
          ls_controller_key TYPE wdy_md_controller_key,
          ls_component_key  TYPE wdy_md_component_key,
          ls_view_key       TYPE wdy_md_view_key.

    FIELD-SYMBOLS: <ls_object>               LIKE LINE OF lt_objects,
                   <ls_meta>                 LIKE LINE OF rs_component-ctlr_metadata,
                   <ls_view>                 LIKE LINE OF rs_component-view_metadata,
                   <lt_ctrl_exceptions>      TYPE ANY TABLE,
                   <lt_ctrl_exception_texts> TYPE ANY TABLE.

    CLEAR mt_components.
    CLEAR mt_sources.

    lt_objects = get_limu_objects( ).

    LOOP AT lt_objects ASSIGNING <ls_object>.
      CASE <ls_object>-sub_type.
        WHEN wdyn_limu_component_controller.
          ls_controller_key = <ls_object>-sub_name.
          APPEND read_controller( ls_controller_key ) TO rs_component-ctlr_metadata.
        WHEN wdyn_limu_component_definition.
          ls_component_key = <ls_object>-sub_name.
          rs_component-comp_metadata = read_definition( ls_component_key ).
        WHEN wdyn_limu_component_view.
          ls_view_key = <ls_object>-sub_name.
          APPEND read_view( ls_view_key ) TO rs_component-view_metadata.
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.
    ENDLOOP.

    SORT rs_component-ctlr_metadata BY
      definition-component_name ASCENDING
      definition-controller_name ASCENDING.

    LOOP AT rs_component-ctlr_metadata ASSIGNING <ls_meta>.
      SORT <ls_meta>-descriptions.
      SORT <ls_meta>-controller_usages.
      SORT <ls_meta>-controller_components.
      SORT <ls_meta>-controller_component_texts.
      SORT <ls_meta>-controller_parameters.
      SORT <ls_meta>-controller_parameter_texts.
      SORT <ls_meta>-context_nodes.
      SORT <ls_meta>-context_attributes.
      SORT <ls_meta>-context_mappings.
      SORT <ls_meta>-fieldgroups.
*     Version 702 doesn't have these two attributes so we
*     use them dynamically for downward compatibility
      ASSIGN COMPONENT 'CONTROLLER_EXCEPTIONS' OF STRUCTURE <ls_meta> TO <lt_ctrl_exceptions>.
      IF sy-subrc = 0.
        SORT <lt_ctrl_exceptions>.
      ENDIF.
      ASSIGN COMPONENT 'CONTROLLER_EXCEPTION_TEXTS' OF STRUCTURE <ls_meta> TO <lt_ctrl_exception_texts>.
      IF sy-subrc = 0.
        SORT <lt_ctrl_exception_texts>.
      ENDIF.
    ENDLOOP.

    SORT rs_component-view_metadata BY
      definition-component_name ASCENDING
      definition-view_name ASCENDING.

    LOOP AT rs_component-view_metadata ASSIGNING <ls_view>.
      SORT <ls_view>-descriptions.
      SORT <ls_view>-view_containers.
      SORT <ls_view>-view_container_texts.
      SORT <ls_view>-iobound_plugs.
      SORT <ls_view>-iobound_plug_texts.
      SORT <ls_view>-plug_parameters.
      SORT <ls_view>-plug_parameter_texts.
      SORT <ls_view>-ui_elements.
      SORT <ls_view>-ui_context_bindings.
      SORT <ls_view>-ui_event_bindings.
      SORT <ls_view>-ui_ddic_bindings.
      SORT <ls_view>-ui_properties.
      SORT <ls_view>-navigation_links.
      SORT <ls_view>-navigation_target_refs.
      SORT <ls_view>-vsh_nodes.
      SORT <ls_view>-vsh_placeholders.
      SORT <ls_view>-viewset_properties.
    ENDLOOP.

    SORT mt_components BY
      component_name ASCENDING
      controller_name ASCENDING
      cmpname ASCENDING.

    SORT mt_sources BY
      component_name ASCENDING
      controller_name ASCENDING
      cmpname ASCENDING
      line_number ASCENDING.

  ENDMETHOD.


  METHOD read_controller.

    DATA: lt_components   TYPE TABLE OF wdy_ctlr_compo_vrs,
          lt_sources      TYPE TABLE OF wdy_ctlr_compo_source_vrs,
          lt_definition   TYPE TABLE OF wdy_controller,
          lt_psmodilog    TYPE TABLE OF smodilog,
          lt_psmodisrc    TYPE TABLE OF smodisrc,
          lt_fm_param     TYPE abap_func_parmbind_tab,
          lt_fm_exception TYPE abap_func_excpbind_tab.

    FIELD-SYMBOLS: <lt_ctrl_exceptions>      TYPE ANY TABLE,
                   <lt_ctrl_exception_texts> TYPE ANY TABLE.

*   Calling FM dynamically because version 702 has less parameters

*   FM parameters
    add_fm_param_exporting( EXPORTING iv_name     = 'CONTROLLER_KEY'
                                      ig_value    = is_key
                            CHANGING  ct_param = lt_fm_param ).
    add_fm_param_exporting( EXPORTING iv_name     = 'GET_ALL_TRANSLATIONS'
                                      ig_value    = abap_false
                            CHANGING  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'DEFINITION'
                         CHANGING  ct_value = lt_definition
                                   ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'DESCRIPTIONS'
                         CHANGING ct_value = rs_controller-descriptions
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_USAGES'
                         CHANGING ct_value = rs_controller-controller_usages
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_COMPONENTS'
                         CHANGING ct_value = lt_components
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_COMPONENT_SOURCES'
                         CHANGING ct_value = lt_sources
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_COMPONENT_TEXTS'
                         CHANGING ct_value = rs_controller-controller_component_texts
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_PARAMETERS'
                         CHANGING ct_value = rs_controller-controller_parameters
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_PARAMETER_TEXTS'
                         CHANGING ct_value = rs_controller-controller_parameter_texts
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTEXT_NODES'
                         CHANGING ct_value = rs_controller-context_nodes
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTEXT_ATTRIBUTES'
                         CHANGING ct_value = rs_controller-context_attributes
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTEXT_MAPPINGS'
                         CHANGING ct_value = rs_controller-context_mappings
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'FIELDGROUPS'
                         CHANGING ct_value = rs_controller-fieldgroups
                                  ct_param = lt_fm_param ).
*   Version 702 doesn't have these two attributes so we
*   use them dynamically for downward compatibility
    ASSIGN COMPONENT 'CONTROLLER_EXCEPTIONS' OF STRUCTURE rs_controller TO <lt_ctrl_exceptions>.
    IF sy-subrc = 0.
      add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_EXCEPTIONS'
                           CHANGING ct_value = <lt_ctrl_exceptions>
                                    ct_param = lt_fm_param ).
    ENDIF.
    ASSIGN COMPONENT 'CONTROLLER_EXCEPTION_TEXTS' OF STRUCTURE rs_controller TO <lt_ctrl_exception_texts>.
    IF sy-subrc = 0.
      add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_EXCEPTION_TEXTS'
                           CHANGING ct_value = <lt_ctrl_exception_texts>
                                    ct_param = lt_fm_param ).
    ENDIF.
    add_fm_param_tables( EXPORTING iv_name = 'PSMODILOG'
                         CHANGING ct_value = lt_psmodilog
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'PSMODISRC'
                         CHANGING ct_value = lt_psmodisrc
                                  ct_param = lt_fm_param ).

*   FM exceptions
    add_fm_exception( EXPORTING iv_name = 'NOT_EXISTING'
                                iv_value = 1
                      CHANGING ct_exception = lt_fm_exception ).
    add_fm_exception( EXPORTING iv_name = 'OTHERS'
                                iv_value = 2
                      CHANGING ct_exception = lt_fm_exception ).

    CALL FUNCTION 'WDYC_GET_OBJECT'
      PARAMETER-TABLE
      lt_fm_param
      EXCEPTION-TABLE
      lt_fm_exception.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from WDYC_GET_OBJECT' ).
    ENDIF.

    APPEND LINES OF lt_components TO mt_components.
    APPEND LINES OF lt_sources TO mt_sources.

    READ TABLE lt_definition INDEX 1 INTO rs_controller-definition.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'WDYC, definition not found' ).
    ENDIF.

    CLEAR: rs_controller-definition-author,
           rs_controller-definition-createdon,
           rs_controller-definition-changedby,
           rs_controller-definition-changedon.

  ENDMETHOD.


  METHOD read_definition.

    DATA: lt_definition TYPE TABLE OF wdy_component,
          lt_psmodilog  TYPE TABLE OF smodilog,
          lt_psmodisrc  TYPE TABLE OF smodisrc.


    CALL FUNCTION 'WDYD_GET_OBJECT'
      EXPORTING
        component_key           = is_key
        get_all_translations    = abap_false
      TABLES
        definition              = lt_definition
        descriptions            = rs_definition-descriptions
        component_usages        = rs_definition-component_usages
        interface_implementings = rs_definition-interface_implementings
        library_usages          = rs_definition-library_usages
        ext_ctlr_usages         = rs_definition-ext_ctlr_usages
        ext_ctx_mappings        = rs_definition-ext_ctx_mappings
        psmodilog               = lt_psmodilog " not optional in all versions
        psmodisrc               = lt_psmodisrc " not optional in all versions
      EXCEPTIONS
        not_existing            = 1
        OTHERS                  = 2.
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from WDYD_GET_OBJECT' ).
    ENDIF.

    READ TABLE lt_definition INDEX 1 INTO rs_definition-definition.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'WDYD, definition not found' ).
    ENDIF.

    CLEAR: rs_definition-definition-author,
           rs_definition-definition-createdon,
           rs_definition-definition-changedby,
           rs_definition-definition-changedon,
           rs_definition-definition-gendate,
           rs_definition-definition-gentime.

  ENDMETHOD.


  METHOD read_view.

    DATA: lt_definition TYPE TABLE OF wdy_view_vrs,
          lt_psmodilog  TYPE TABLE OF smodilog,
          lt_psmodisrc  TYPE TABLE OF smodisrc.

    FIELD-SYMBOLS: <ls_definition> LIKE LINE OF lt_definition.


    CALL FUNCTION 'WDYV_GET_OBJECT'
      EXPORTING
        view_key               = is_key
        get_all_translations   = abap_false
      TABLES
        definition             = lt_definition
        descriptions           = rs_view-descriptions
        view_containers        = rs_view-view_containers
        view_container_texts   = rs_view-view_container_texts
        iobound_plugs          = rs_view-iobound_plugs
        iobound_plug_texts     = rs_view-iobound_plug_texts
        plug_parameters        = rs_view-plug_parameters
        plug_parameter_texts   = rs_view-plug_parameter_texts
        ui_elements            = rs_view-ui_elements
        ui_context_bindings    = rs_view-ui_context_bindings
        ui_event_bindings      = rs_view-ui_event_bindings
        ui_ddic_bindings       = rs_view-ui_ddic_bindings
        ui_properties          = rs_view-ui_properties
        navigation_links       = rs_view-navigation_links
        navigation_target_refs = rs_view-navigation_target_refs
        vsh_nodes              = rs_view-vsh_nodes
        vsh_placeholders       = rs_view-vsh_placeholders
        viewset_properties     = rs_view-viewset_properties
        psmodilog              = lt_psmodilog
        psmodisrc              = lt_psmodisrc
      EXCEPTIONS
        not_existing           = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from WDYV_GET_OBJECT' ).
    ENDIF.

    READ TABLE lt_definition INDEX 1 ASSIGNING <ls_definition>.
    ASSERT sy-subrc = 0.
    MOVE-CORRESPONDING <ls_definition> TO rs_view-definition.

    CLEAR: rs_view-definition-author,
           rs_view-definition-createdon,
           rs_view-definition-changedby,
           rs_view-definition-changedon.

  ENDMETHOD.


  METHOD recover_controller.

    DATA: ls_key    TYPE wdy_controller_key,
          lv_corrnr TYPE trkorr,
          lx_error  TYPE REF TO cx_wdy_md_exception,
          ls_delta  TYPE svrs2_xversionable_object.


    ls_delta = delta_controller( is_controller ).
    ls_key-component_name  = is_controller-definition-component_name.
    ls_key-controller_name = is_controller-definition-controller_name.

    TRY.
        cl_wdy_md_controller=>recover_version(
          EXPORTING
            controller_key = ls_key
            delta          = ls_delta-wdyc
          CHANGING
            corrnr         = lv_corrnr ).
      CATCH cx_wdy_md_exception INTO lx_error.
        zcx_abapgit_exception=>raise( |Error recovering version of controller: { lx_error->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD recover_definition.

    DATA: ls_key    TYPE wdy_md_component_key,
          lv_corrnr TYPE trkorr,
          lx_error  TYPE REF TO cx_wdy_md_exception,
          ls_delta  TYPE svrs2_xversionable_object.


    ls_delta = delta_definition(
      is_definition = is_definition
      iv_package    = iv_package ).

    ls_key-component_name = is_definition-definition-component_name.

    TRY.
        cl_wdy_md_component=>recover_version(
          EXPORTING
            component_key = ls_key
            delta         = ls_delta-wdyd
          CHANGING
            corrnr        = lv_corrnr ).
      CATCH cx_wdy_md_exception INTO lx_error.
        zcx_abapgit_exception=>raise( |Error recovering version of component: { lx_error->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD recover_view.

    DATA: ls_key    TYPE wdy_md_view_key,
          lv_corrnr TYPE trkorr,
          lx_error  TYPE REF TO cx_wdy_md_exception,
          ls_delta  TYPE svrs2_xversionable_object.


    ls_delta = delta_view( is_view ).
    ls_key-component_name = is_view-definition-component_name.
    ls_key-view_name      = is_view-definition-view_name.

    TRY.
        cl_wdy_md_abstract_view=>recover_version(
          EXPORTING
            view_key = ls_key
            delta    = ls_delta-wdyv
          CHANGING
            corrnr   = lv_corrnr ).
      CATCH cx_wdy_md_exception INTO lx_error.
        zcx_abapgit_exception=>raise( |Error recovering version of abstract view: { lx_error->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    SELECT SINGLE changedby FROM wdy_component INTO rv_user
      WHERE component_name = ms_item-obj_name AND version = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lo_component   TYPE REF TO cl_wdy_wb_component,
          lo_request     TYPE REF TO cl_wb_request,
          li_state       TYPE REF TO if_wb_program_state,
          lv_object_name TYPE seu_objkey.


    CREATE OBJECT lo_component.

    lv_object_name = ms_item-obj_name.
    CREATE OBJECT lo_request
      EXPORTING
        p_object_type = 'YC'
        p_object_name = lv_object_name
        p_operation   = swbm_c_op_delete_no_dialog.

    lo_component->if_wb_program~process_wb_request(
      p_wb_request       = lo_request
      p_wb_program_state = li_state ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_component   TYPE wdy_component_metadata,
          ls_description TYPE wdy_ext_ctx_map.

    FIELD-SYMBOLS: <ls_view>       LIKE LINE OF ls_component-view_metadata,
                   <ls_controller> LIKE LINE OF ls_component-ctlr_metadata.

    io_xml->read( EXPORTING iv_name = 'COMPONENT'
                  CHANGING cg_data = ls_component ).
    io_xml->read( EXPORTING iv_name  = 'COMPONENTS'
                  CHANGING cg_data = mt_components ).
    io_xml->read( EXPORTING iv_name  = 'SOURCES'
                  CHANGING cg_data = mt_sources ).

    ls_component-comp_metadata-definition-author = sy-uname.
    ls_component-comp_metadata-definition-createdon = sy-datum.
    recover_definition( is_definition = ls_component-comp_metadata
                        iv_package    = iv_package ).

    LOOP AT ls_component-ctlr_metadata ASSIGNING <ls_controller>.
      <ls_controller>-definition-author = sy-uname.
      <ls_controller>-definition-createdon = sy-datum.
      recover_controller( <ls_controller> ).
    ENDLOOP.
    LOOP AT ls_component-view_metadata ASSIGNING <ls_view>.
      <ls_view>-definition-author = sy-uname.
      <ls_view>-definition-createdon = sy-datum.
      recover_view( <ls_view> ).
    ENDLOOP.

    READ TABLE ls_component-comp_metadata-descriptions INTO ls_description INDEX 1.
    IF sy-subrc = 0.
      zcl_abapgit_sotr_handler=>create_sotr(
        iv_package = iv_package
        io_xml     = io_xml ).
    ENDIF.

    add_with_inactive_parts( ).

    deserialize_longtexts(
      ii_xml         = io_xml
      iv_longtext_id = c_longtext_id_wd ).

    deserialize_longtexts(
      ii_xml           = io_xml
      iv_longtext_id   = c_longtext_id_wc
      iv_longtext_name = c_longtext_name_wc ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_component_name TYPE wdy_component-component_name.


    SELECT SINGLE component_name FROM wdy_component
      INTO lv_component_name
      WHERE component_name = ms_item-obj_name.          "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
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
    rv_is_locked = abap_false.
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

    DATA: ls_component   TYPE wdy_component_metadata,
          ls_comp        TYPE wdy_ctlr_compo_vrs,
          lv_object      TYPE dokil-object,
          lt_object      TYPE STANDARD TABLE OF dokil-object WITH DEFAULT KEY,
          lt_dokil       TYPE STANDARD TABLE OF dokil WITH DEFAULT KEY,
          ls_description TYPE wdy_ext_ctx_map.

    ls_component = read( ).

    io_xml->add( iv_name = 'COMPONENT'
                 ig_data = ls_component ).
    io_xml->add( ig_data = mt_components
                 iv_name = 'COMPONENTS' ).
    io_xml->add( ig_data = mt_sources
                 iv_name = 'SOURCES' ).

    READ TABLE ls_component-comp_metadata-descriptions INTO ls_description INDEX 1.
    IF sy-subrc = 0.
      zcl_abapgit_sotr_handler=>read_sotr(
        iv_pgmid    = 'LIMU'
        iv_object   = 'WDYV'
        iv_obj_name = ms_item-obj_name
        io_xml      = io_xml ).
    ENDIF.

    serialize_longtexts(
      ii_xml         = io_xml
      iv_longtext_id = c_longtext_id_wd ).

    LOOP AT mt_components INTO ls_comp.
      lv_object    = ls_comp-component_name.
      lv_object+30 = ls_comp-controller_name.
      COLLECT lv_object INTO lt_object.
    ENDLOOP.

    IF lt_object IS NOT INITIAL.
      IF io_xml->i18n_params( )-main_language_only = abap_true.
        SELECT * FROM dokil INTO TABLE lt_dokil
          FOR ALL ENTRIES IN lt_object
          WHERE id = c_longtext_id_wc AND object = lt_object-table_line AND masterlang = abap_true
          ORDER BY PRIMARY KEY.
      ELSE.
        SELECT * FROM dokil INTO TABLE lt_dokil
          FOR ALL ENTRIES IN lt_object
          WHERE id = c_longtext_id_wc AND object = lt_object-table_line
          ORDER BY PRIMARY KEY.
      ENDIF.

      serialize_longtexts(
        ii_xml           = io_xml
        it_dokil         = lt_dokil
        iv_longtext_id   = c_longtext_id_wc
        iv_longtext_name = c_longtext_name_wc ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
