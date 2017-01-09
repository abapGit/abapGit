*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_WDYN
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_wdyn DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_object_wdyn definition inheriting from lcl_objects_super final.

  public section.
    interfaces lif_object.
    aliases mo_files for lif_object~mo_files.

  private section.

    data:
      mt_components type table of wdy_ctlr_compo_vrs,
      mt_sources    type table of wdy_ctlr_compo_source_vrs.

    methods:
      get_limu_objects
        returning value(rt_objects) type wdy_md_transport_keys,
      read
        returning value(rs_component) type wdy_component_metadata
        raising   lcx_exception,
      read_controller
        importing is_key               type wdy_md_controller_key
        returning value(rs_controller) type wdy_md_controller_meta_data
        raising   lcx_exception,
      read_definition
        importing is_key               type wdy_md_component_key
        returning value(rs_definition) type wdy_md_component_meta_data
        raising   lcx_exception,
      read_view
        importing is_key         type wdy_md_view_key
        returning value(rs_view) type wdy_md_view_meta_data
        raising   lcx_exception,
      recover_controller
        importing is_controller type wdy_md_controller_meta_data
        raising   lcx_exception,
      recover_definition
        importing is_definition type wdy_md_component_meta_data
        raising   lcx_exception,
      recover_view
        importing is_view type wdy_md_view_meta_data
        raising   lcx_exception,
      delta_controller
        importing is_controller   type wdy_md_controller_meta_data
        returning value(rs_delta) type svrs2_xversionable_object
        raising   lcx_exception,
      delta_definition
        importing is_definition   type wdy_md_component_meta_data
        returning value(rs_delta) type svrs2_xversionable_object
        raising   lcx_exception,
      delta_view
        importing is_view         type wdy_md_view_meta_data
        returning value(rs_delta) type svrs2_xversionable_object
        raising   lcx_exception,
      add_fm_param_exporting
        importing i_name          type string
                  i_value         type any
        changing  ct_param        type abap_func_parmbind_tab,
      add_fm_param_tables
        importing i_name          type string
        changing  ct_value        type any table
                  ct_param        type abap_func_parmbind_tab,
      add_fm_exception
        importing i_name          type string
                  i_value         type i
        changing  ct_exception    type abap_func_excpbind_tab.

endclass.                    "lcl_object_wdyn DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_wdyn IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_object_wdyn implementation.

  method lif_object~has_changed_since.
    rv_changed = abap_true.
  endmethod.  "lif_object~has_changed_since

  method lif_object~changed_by.
    rv_user = c_user_unknown. " todo
  endmethod.                    "lif_object~changed_by

  method lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  endmethod.                    "lif_object~get_metadata

  method lif_object~exists.

    data: lv_component_name type wdy_component-component_name.


    select single component_name from wdy_component
      into lv_component_name
      where component_name = ms_item-obj_name
      and version = 'A'.                                "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  endmethod.                    "lif_object~exists

  method delta_definition.

    data: ls_key       type wdy_md_component_key,
          lv_found     type abap_bool,
          ls_obj_new   type svrs2_versionable_object,
          li_component type ref to if_wdy_md_component,
          ls_obj_old   type svrs2_versionable_object.


    ls_key-component_name = is_definition-definition-component_name.

    lv_found = cl_wdy_md_component=>check_existency( ls_key-component_name ).
    if lv_found = abap_false.
      try.
          cl_wdy_md_component=>create_complete(
            exporting
              name      = ls_key-component_name
            importing
              component = li_component ).
          li_component->save_to_database( ).
          li_component->unlock( ).
        catch cx_wdy_md_exception.
          lcx_exception=>raise( 'error creating dummy component' ).
      endtry.
    endif.

    ls_obj_new-objtype = wdyn_limu_component_definition.
    ls_obj_new-objname = ls_key-component_name.

    ls_obj_old-objtype = wdyn_limu_component_definition.
    ls_obj_old-objname = ls_key-component_name.

    append is_definition-definition to ls_obj_old-wdyd-defin.
    ls_obj_old-wdyd-descr = is_definition-descriptions.
    ls_obj_old-wdyd-cusag = is_definition-component_usages.
    ls_obj_old-wdyd-intrf = is_definition-interface_implementings.
    ls_obj_old-wdyd-libra = is_definition-library_usages.
    ls_obj_old-wdyd-ctuse = is_definition-ext_ctlr_usages.
    ls_obj_old-wdyd-ctmap = is_definition-ext_ctx_mappings.

    call function 'SVRS_MAKE_OBJECT_DELTA'
      exporting
        obj_old              = ls_obj_new
        obj_new              = ls_obj_old
      changing
        delta                = rs_delta
      exceptions
        inconsistent_objects = 1.
    if sy-subrc <> 0.
      lcx_exception=>raise( 'error from SVRS_MAKE_OBJECT_DELTA' ).
    endif.

  endmethod.                    "delta_definition

  method delta_controller.

    data: li_controller type ref to if_wdy_md_controller,
          lv_found      type abap_bool,
          ls_key        type wdy_md_controller_key,
          ls_obj_new    type svrs2_versionable_object,
          ls_obj_old    type svrs2_versionable_object.

    field-symbols: <ls_component>            like line of mt_components,
                   <ls_source>               like line of mt_sources,
                   <lt_ctrl_exceptions>      type any table,
                   <lt_ctrl_exception_texts> type any table,
                   <excp>                    type any table,
                   <excpt>                   type any table.


    ls_key-component_name = is_controller-definition-component_name.
    ls_key-controller_name = is_controller-definition-controller_name.

    lv_found = cl_wdy_md_controller=>check_existency(
          component_name  = ls_key-component_name
          controller_name = ls_key-controller_name ).
    if lv_found = abap_false.
      try.
          li_controller ?= cl_wdy_md_controller=>create_complete(
                component_name  = ls_key-component_name
                controller_name = ls_key-controller_name
                controller_type = is_controller-definition-controller_type ).
          li_controller->save_to_database( ).
          li_controller->unlock( ).
        catch cx_wdy_md_exception.
          lcx_exception=>raise( 'error creating dummy controller' ).
      endtry.
    endif.

    ls_obj_new-objtype = wdyn_limu_component_controller.
    ls_obj_new-objname = ls_key.

    ls_obj_old-objtype = wdyn_limu_component_controller.
    ls_obj_old-objname = ls_key.

    append is_controller-definition to ls_obj_old-wdyc-defin.

    loop at mt_components assigning <ls_component>
        where component_name = ls_key-component_name
        and controller_name = ls_key-controller_name.
      append <ls_component> to ls_obj_old-wdyc-ccomp.
    endloop.
    loop at mt_sources assigning <ls_source>
        where component_name = ls_key-component_name
        and controller_name = ls_key-controller_name.
      append <ls_source> to ls_obj_old-wdyc-ccoms.
    endloop.

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
    assign component 'CONTROLLER_EXCEPTIONS' of structure is_controller to <lt_ctrl_exceptions>.
    if sy-subrc = 0.
      assign component 'EXCP' of structure ls_obj_old-wdyc to <excp>.
      if sy-subrc = 0.
        <excp> = <lt_ctrl_exceptions>.
      endif.
    endif.
    assign component 'CONTROLLER_EXCEPTIONS_TEXTS' of structure is_controller to <lt_ctrl_exception_texts>.
    if sy-subrc = 0.
      assign component 'EXCPT' of structure ls_obj_old-wdyc to <excpt>.
      if sy-subrc = 0.
        <excpt> = <lt_ctrl_exception_texts>.
      endif.
    endif.
    ls_obj_old-wdyc-fgrps = is_controller-fieldgroups.

    call function 'SVRS_MAKE_OBJECT_DELTA'
      exporting
        obj_old              = ls_obj_new
        obj_new              = ls_obj_old
      changing
        delta                = rs_delta
      exceptions
        inconsistent_objects = 1.
    if sy-subrc <> 0.
      lcx_exception=>raise( 'error from SVRS_MAKE_OBJECT_DELTA' ).
    endif.

  endmethod.                    "delta_controller

  method delta_view.

    data: ls_key     type wdy_md_view_key,
          ls_obj_new type svrs2_versionable_object,
          ls_obj_old type svrs2_versionable_object,
          lv_found   type abap_bool,
          li_view    type ref to if_wdy_md_abstract_view.

    field-symbols: <ls_def> like line of ls_obj_old-wdyv-defin.


    ls_key-component_name = is_view-definition-component_name.
    ls_key-view_name      = is_view-definition-view_name.

    lv_found = cl_wdy_md_abstract_view=>check_existency(
                 component_name = ls_key-component_name
                 name           = ls_key-view_name ).
    if lv_found = abap_false.
      try.
          li_view = cl_wdy_md_abstract_view=>create(
                      component_name = is_view-definition-component_name
                      view_name      = is_view-definition-view_name
                      type           = is_view-definition-type ).
          li_view->save_to_database( ).
          li_view->unlock( ).
        catch cx_wdy_md_exception.
          lcx_exception=>raise( 'error creating dummy view' ).
      endtry.
    endif.

    ls_obj_new-objtype = wdyn_limu_component_view.
    ls_obj_new-objname = ls_key.

    ls_obj_old-objtype = wdyn_limu_component_view.
    ls_obj_old-objname = ls_key.

    append initial line to ls_obj_old-wdyv-defin assigning <ls_def>.
    move-corresponding is_view-definition to <ls_def>.

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

    call function 'SVRS_MAKE_OBJECT_DELTA'
      exporting
        obj_old              = ls_obj_new
        obj_new              = ls_obj_old
      changing
        delta                = rs_delta
      exceptions
        inconsistent_objects = 1.
    if sy-subrc <> 0.
      lcx_exception=>raise( 'error from SVRS_MAKE_OBJECT_DELTA' ).
    endif.

  endmethod.                    "delta_view

  method recover_definition.

    data: ls_key    type wdy_md_component_key,
          lv_corrnr type trkorr,
          ls_delta  type svrs2_xversionable_object.


    ls_delta = delta_definition( is_definition ).
    ls_key-component_name = is_definition-definition-component_name.

    cl_wdy_md_component=>recover_version(
      exporting
        component_key = ls_key
        delta         = ls_delta-wdyd
      changing
        corrnr        = lv_corrnr ).

  endmethod.                    "recover_definition

  method recover_controller.

    data: ls_key    type wdy_controller_key,
          lv_corrnr type trkorr,
          ls_delta  type svrs2_xversionable_object.


    ls_delta = delta_controller( is_controller ).
    ls_key-component_name  = is_controller-definition-component_name.
    ls_key-controller_name = is_controller-definition-controller_name.

    cl_wdy_md_controller=>recover_version(
      exporting
        controller_key = ls_key
        delta          = ls_delta-wdyc
      changing
        corrnr         = lv_corrnr ).

  endmethod.                    "recover_controller

  method recover_view.

    data: ls_key    type wdy_md_view_key,
          lv_corrnr type trkorr,
          ls_delta  type svrs2_xversionable_object.


    ls_delta = delta_view( is_view ).
    ls_key-component_name = is_view-definition-component_name.
    ls_key-view_name      = is_view-definition-view_name.

    cl_wdy_md_abstract_view=>recover_version(
      exporting
        view_key = ls_key
        delta    = ls_delta-wdyv
      changing
        corrnr   = lv_corrnr ).

  endmethod.                    "recover_view

  method read_controller.

    data: lt_components   type table of wdy_ctlr_compo_vrs,
          lt_sources      type table of wdy_ctlr_compo_source_vrs,
          lt_definition   type table of wdy_controller,
          lt_psmodilog    type table of smodilog,
          lt_psmodisrc    type table of smodisrc,
          lt_fm_param     type abap_func_parmbind_tab,
          lt_fm_exception type abap_func_excpbind_tab.

    field-symbols: <lt_ctrl_exceptions>      type any table,
                   <lt_ctrl_exception_texts> type any table.

*   Calling FM dynamically because version 702 has less parameters

*   FM parameters
    add_fm_param_exporting( exporting i_name     = 'CONTROLLER_KEY'
                                      i_value    = is_key
                            changing  ct_param = lt_fm_param ).
    add_fm_param_exporting( exporting i_name     = 'GET_ALL_TRANSLATIONS'
                                      i_value    = abap_false
                            changing  ct_param = lt_fm_param ).
    add_fm_param_tables( exporting i_name = 'DEFINITION'
                         changing  ct_value = lt_definition
                                   ct_param = lt_fm_param ).
    add_fm_param_tables( exporting i_name = 'DESCRIPTIONS'
                         changing ct_value = rs_controller-descriptions
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( exporting i_name = 'CONTROLLER_USAGES'
                         changing ct_value = rs_controller-controller_usages
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( exporting i_name = 'CONTROLLER_COMPONENTS'
                         changing ct_value = lt_components
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( exporting i_name = 'CONTROLLER_COMPONENT_SOURCES'
                         changing ct_value = lt_sources
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( exporting i_name = 'CONTROLLER_COMPONENT_TEXTS'
                         changing ct_value = rs_controller-controller_component_texts
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( exporting i_name = 'CONTROLLER_PARAMETERS'
                         changing ct_value = rs_controller-controller_parameters
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( exporting i_name = 'CONTROLLER_PARAMETER_TEXTS'
                         changing ct_value = rs_controller-controller_parameter_texts
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( exporting i_name = 'CONTEXT_NODES'
                         changing ct_value = rs_controller-context_nodes
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( exporting i_name = 'CONTEXT_ATTRIBUTES'
                         changing ct_value = rs_controller-context_attributes
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( exporting i_name = 'CONTEXT_MAPPINGS'
                         changing ct_value = rs_controller-context_mappings
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( exporting i_name = 'FIELDGROUPS'
                         changing ct_value = rs_controller-fieldgroups
                                  ct_param = lt_fm_param ).
*   Version 702 doesn't have these two attributes so we
*   use them dynamically for downward compatibility
    assign component 'CONTROLLER_EXCEPTIONS' of structure rs_controller to <lt_ctrl_exceptions>.
    if sy-subrc = 0.
      add_fm_param_tables( exporting i_name = 'CONTROLLER_EXCEPTIONS'
                           changing ct_value = <lt_ctrl_exceptions>
                                    ct_param = lt_fm_param ).
    endif.
    assign component 'CONTROLLER_EXCEPTION_TEXTS' of structure rs_controller to <lt_ctrl_exception_texts>.
    if sy-subrc = 0.
      add_fm_param_tables( exporting i_name = 'CONTROLLER_EXCEPTION_TEXTS'
                           changing ct_value = <lt_ctrl_exception_texts>
                                    ct_param = lt_fm_param ).
    endif.
    add_fm_param_tables( exporting i_name = 'PSMODILOG'
                         changing ct_value = lt_psmodilog
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( exporting i_name = 'PSMODISRC'
                         changing ct_value = lt_psmodisrc
                                  ct_param = lt_fm_param ).

*   FM exceptions
    add_fm_exception( exporting i_name = 'NOT_EXISTING'
                                i_value = 1
                      changing ct_exception = lt_fm_exception ).
    add_fm_exception( exporting i_name = 'OTHERS'
                                i_value = 2
                      changing ct_exception = lt_fm_exception ).

    call function 'WDYC_GET_OBJECT'
      parameter-table
      lt_fm_param
      exception-table
      lt_fm_exception.
    if sy-subrc <> 0.
      lcx_exception=>raise( 'error from WDYC_GET_OBJECT' ).
    endif.

    append lines of lt_components to mt_components.
    append lines of lt_sources to mt_sources.

    read table lt_definition index 1 into rs_controller-definition.
    if sy-subrc <> 0.
      lcx_exception=>raise( 'WDYC, definition not found' ).
    endif.

    clear: rs_controller-definition-author,
           rs_controller-definition-createdon,
           rs_controller-definition-changedby,
           rs_controller-definition-changedon.

  endmethod.                    "read_controller

  method read_definition.

    data: lt_definition type table of wdy_component,
          lt_psmodilog  type table of smodilog,
          lt_psmodisrc  type table of smodisrc.


    call function 'WDYD_GET_OBJECT'
      exporting
        component_key           = is_key
        get_all_translations    = abap_false
      tables
        definition              = lt_definition
        descriptions            = rs_definition-descriptions
        component_usages        = rs_definition-component_usages
        interface_implementings = rs_definition-interface_implementings
        library_usages          = rs_definition-library_usages
        ext_ctlr_usages         = rs_definition-ext_ctlr_usages
        ext_ctx_mappings        = rs_definition-ext_ctx_mappings
        psmodilog               = lt_psmodilog " not optional in all versions
        psmodisrc               = lt_psmodisrc " not optional in all versions
      exceptions
        not_existing            = 1
        others                  = 2.
    if sy-subrc = 1.
      return.
    elseif sy-subrc <> 0.
      lcx_exception=>raise( 'error from WDYD_GET_OBJECT' ).
    endif.

    read table lt_definition index 1 into rs_definition-definition.
    if sy-subrc <> 0.
      lcx_exception=>raise( 'WDYD, definition not found' ).
    endif.

    clear: rs_definition-definition-author,
           rs_definition-definition-createdon,
           rs_definition-definition-changedby,
           rs_definition-definition-changedon,
           rs_definition-definition-gendate,
           rs_definition-definition-gentime.

  endmethod.                    "read_definition

  method read_view.

    data: lt_definition type table of wdy_view_vrs,
          lt_psmodilog  type table of smodilog,
          lt_psmodisrc  type table of smodisrc.

    field-symbols: <ls_definition> like line of lt_definition.


    call function 'WDYV_GET_OBJECT'
      exporting
        view_key               = is_key
        get_all_translations   = abap_false
      tables
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
      exceptions
        not_existing           = 1
        others                 = 2.
    if sy-subrc <> 0.
      lcx_exception=>raise( 'error from WDYV_GET_OBJECT' ).
    endif.

    read table lt_definition index 1 assigning <ls_definition>.
    assert sy-subrc = 0.
    move-corresponding <ls_definition> to rs_view-definition.

    clear: rs_view-definition-author,
           rs_view-definition-createdon,
           rs_view-definition-changedby,
           rs_view-definition-changedon.

  endmethod.                    "read_view

  method get_limu_objects.

    data: lv_name type wdy_component_name.


    lv_name = ms_item-obj_name.
    call function 'WDYN_GET_LIMU_OBJECTS'
      exporting
        component_name = lv_name
      importing
        limu_objects   = rt_objects.

  endmethod.                    "get_limu_objects

  method read.

    data: lt_objects        type wdy_md_transport_keys,
          ls_controller_key type wdy_md_controller_key,
          ls_component_key  type wdy_md_component_key,
          ls_view_key       type wdy_md_view_key.

    field-symbols: <ls_object>               like line of lt_objects,
                   <ls_meta>                 like line of rs_component-ctlr_metadata,
                   <lt_ctrl_exceptions>      type any table,
                   <lt_ctrl_exception_texts> type any table.

    clear mt_components.
    clear mt_sources.

    lt_objects = get_limu_objects( ).

    loop at lt_objects assigning <ls_object>.
      case <ls_object>-sub_type.
        when wdyn_limu_component_controller.
          ls_controller_key = <ls_object>-sub_name.
          append read_controller( ls_controller_key ) to rs_component-ctlr_metadata.
        when wdyn_limu_component_definition.
          ls_component_key = <ls_object>-sub_name.
          rs_component-comp_metadata = read_definition( ls_component_key ).
        when wdyn_limu_component_view.
          ls_view_key = <ls_object>-sub_name.
          append read_view( ls_view_key ) to rs_component-view_metadata.
        when others.
          assert 0 = 1.
      endcase.
    endloop.

    sort rs_component-ctlr_metadata by
      definition-component_name ascending
      definition-controller_name ascending.

    loop at rs_component-ctlr_metadata assigning <ls_meta>.
      sort <ls_meta>-descriptions.
      sort <ls_meta>-controller_usages.
      sort <ls_meta>-controller_components.
      sort <ls_meta>-controller_component_texts.
      sort <ls_meta>-controller_parameters.
      sort <ls_meta>-controller_parameter_texts.
      sort <ls_meta>-context_nodes.
      sort <ls_meta>-context_attributes.
      sort <ls_meta>-context_mappings.
      sort <ls_meta>-fieldgroups.
*     Version 702 doesn't have these two attributes so we
*     use them dynamically for downward compatibility
      assign component 'CONTROLLER_EXCEPTIONS' of structure <ls_meta> to <lt_ctrl_exceptions>.
      if sy-subrc = 0.
        sort <lt_ctrl_exceptions>.
      endif.
      assign component 'CONTROLLER_EXCEPTION_TEXTS' of structure <ls_meta> to <lt_ctrl_exception_texts>.
      if sy-subrc = 0.
        sort <lt_ctrl_exception_texts>.
      endif.
    endloop.

    sort mt_components by
      component_name ascending
      controller_name ascending
      cmpname ascending.

    sort mt_sources by
      component_name ascending
      controller_name ascending
      cmpname ascending
      line_number ascending.

  endmethod.                    "read

  method add_fm_param_exporting.

    data: ls_param like line of ct_param.

    ls_param-kind = abap_func_exporting.
    ls_param-name = i_name.
    get reference of i_value into ls_param-value.

    insert ls_param into table ct_param.

  endmethod.                    "add_fm_param_exporting

  method add_fm_param_tables.

    data: ls_param like line of ct_param.

    ls_param-kind = abap_func_tables.
    ls_param-name = i_name.
    get reference of ct_value into ls_param-value.

    insert ls_param into table ct_param.

  endmethod.                    "add_fm_param_tables

  method add_fm_exception.

    data: ls_exception like line of ct_exception.

    ls_exception-name = i_name.
    ls_exception-value = i_value.

    insert ls_exception into table ct_exception.

  endmethod.                    "add_fm_exception

  method lif_object~serialize.

    data: ls_component type wdy_component_metadata.


    ls_component = read( ).

    io_xml->add( iv_name = 'COMPONENT'
                 ig_data = ls_component ).
    io_xml->add( ig_data = mt_components
                 iv_name = 'COMPONENTS' ).
    io_xml->add( ig_data = mt_sources
                 iv_name = 'SOURCES' ).

  endmethod.                    "serialize

  method lif_object~deserialize.

    data: ls_component type wdy_component_metadata.

    field-symbols: <ls_view>       like line of ls_component-view_metadata,
                   <ls_controller> like line of ls_component-ctlr_metadata.


    io_xml->read( exporting iv_name = 'COMPONENT'
                  changing cg_data = ls_component ).
    io_xml->read( exporting iv_name  = 'COMPONENTS'
                  changing cg_data = mt_components ).
    io_xml->read( exporting iv_name  = 'SOURCES'
                  changing cg_data = mt_sources ).

    ls_component-comp_metadata-definition-author = sy-uname.
    ls_component-comp_metadata-definition-createdon = sy-datum.
    recover_definition( ls_component-comp_metadata ).

    loop at ls_component-ctlr_metadata assigning <ls_controller>.
      <ls_controller>-definition-author = sy-uname.
      <ls_controller>-definition-createdon = sy-datum.
      recover_controller( <ls_controller> ).
    endloop.
    loop at ls_component-view_metadata assigning <ls_view>.
      <ls_view>-definition-author = sy-uname.
      <ls_view>-definition-createdon = sy-datum.
      recover_view( <ls_view> ).
    endloop.

    lcl_objects_activation=>add_item( ms_item ).

  endmethod.                    "deserialize

  method lif_object~delete.

    data: lo_component   type ref to cl_wdy_wb_component,
          lo_request     type ref to cl_wb_request,
          li_state       type ref to if_wb_program_state,
          lv_object_name type seu_objkey.


    create object lo_component.

    lv_object_name = ms_item-obj_name.
    create object lo_request
      exporting
        p_object_type = 'YC'
        p_object_name = lv_object_name
        p_operation   = swbm_c_op_delete_no_dialog.

    lo_component->if_wb_program~process_wb_request(
      p_wb_request       = lo_request
      p_wb_program_state = li_state ).

  endmethod.                    "delete

  method lif_object~jump.

    call function 'RS_TOOL_ACCESS'
      exporting
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = ms_item-obj_type
        in_new_window = abap_true.

  endmethod.                    "jump

  method lif_object~compare_to_remote_version.
    create object ro_comparison_result type lcl_null_comparison_result.
  endmethod.                    "lif_object~compare_to_remote_version

endclass.                    "lcl_object_wdyn IMPLEMENTATION