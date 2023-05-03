CLASS zcl_abapgit_object_fdt0 DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS check_is_local
      RETURNING
        VALUE(rv_is_local) TYPE abap_bool .
    METHODS get_application_id
      RETURNING
        VALUE(rv_application_id) TYPE fdt_admn_0000s-application_id .
    METHODS before_xml_deserialize
      IMPORTING
        !iv_package  TYPE devclass
      EXPORTING
        !ev_create   TYPE abap_bool
        !ev_is_local TYPE abap_bool
      CHANGING
        !co_dom_tree TYPE REF TO if_ixml_document
      RAISING
        zcx_abapgit_exception .
    METHODS filter_xml_serialize
      CHANGING
        !co_ixml_element TYPE REF TO if_ixml_element
      RAISING
        zcx_abapgit_exception .
    METHODS set_field
      IMPORTING
        !iv_name         TYPE string
        !iv_value        TYPE string DEFAULT ''
      CHANGING
        !co_ixml_element TYPE REF TO if_ixml_element
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_object_fdt0 IMPLEMENTATION.


  METHOD before_xml_deserialize.

    DATA lv_application_id TYPE fdt_admn_0000s-application_id.
    DATA lv_timestamp TYPE timestamp.
    DATA lv_transport TYPE string.
    DATA lv_dlvunit TYPE tdevc-dlvunit.
    DATA lo_node_local TYPE REF TO if_ixml_element.
    DATA lo_node_package TYPE REF TO if_ixml_element.
    DATA lo_node_id TYPE REF TO if_ixml_element.
    DATA lo_xml_element TYPE REF TO if_ixml_element.
    DATA lv_count TYPE i.

    lo_node_local = co_dom_tree->find_from_name( name      = 'Local'
                                                 namespace = 'FDTNS' ).

    IF lo_node_local IS BOUND.
      ev_is_local = lo_node_local->get_value( ).
    ENDIF.

    lo_node_package = co_dom_tree->find_from_name(
      name      = 'DevelopmentPackage'
      namespace = 'FDTNS' ).
    IF lo_node_package IS BOUND.
      lo_node_package->set_value( value = |{ iv_package }| ).
    ENDIF.

    lo_node_id = co_dom_tree->find_from_name(
      name      = 'ApplicationId'
      namespace = 'FDTNS' ).
    IF lo_node_id IS BOUND.
      lv_application_id = lo_node_id->get_value( ).
      SELECT COUNT( * ) FROM fdt_admn_0000s INTO lv_count
        WHERE object_type = 'AP'
        AND id = lv_application_id
        AND deleted = ''.
      ev_create = boolc( lv_count = 0 ).
    ENDIF.

    " Fill in user/time/system-specific fields
    GET TIME STAMP FIELD lv_timestamp.
    lv_transport = |${ sy-sysid }0000000000000001|.

    lo_xml_element = co_dom_tree->get_root_element( ).

    IF ev_create = abap_true.
      set_field(
        EXPORTING
          iv_name         = 'CreationUser'
          iv_value        = |{ sy-uname }|
        CHANGING
          co_ixml_element = lo_xml_element ).

      set_field(
        EXPORTING
          iv_name         = 'CreationTimestamp'
          iv_value        = |{ lv_timestamp }|
        CHANGING
          co_ixml_element = lo_xml_element ).
    ENDIF.

    set_field(
      EXPORTING
        iv_name         = 'ChangeUser'
        iv_value        = |{ sy-uname }|
      CHANGING
        co_ixml_element = lo_xml_element ).

    set_field(
      EXPORTING
        iv_name         = 'ChangeTimestamp'
        iv_value        = |{ lv_timestamp }|
      CHANGING
        co_ixml_element = lo_xml_element ).

    set_field(
      EXPORTING
        iv_name         = 'User'
        iv_value        = |{ sy-uname }|
      CHANGING
        co_ixml_element = lo_xml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Timestamp'
        iv_value        = |{ lv_timestamp }|
      CHANGING
        co_ixml_element = lo_xml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Trrequest'
        iv_value        = lv_transport
      CHANGING
        co_ixml_element = lo_xml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Trversion'
        iv_value        = '000001'
      CHANGING
        co_ixml_element = lo_xml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Trtimestamp'
        iv_value        = |{ lv_timestamp }|
      CHANGING
        co_ixml_element = lo_xml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Trsysid'
        iv_value        = |{ sy-sysid }|
      CHANGING
        co_ixml_element = lo_xml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Trclient'
        iv_value        = |{ sy-mandt }|
      CHANGING
        co_ixml_element = lo_xml_element ).

    set_field(
      EXPORTING
        iv_name         = 'OversId'
        iv_value        = |{ lv_application_id }|
      CHANGING
        co_ixml_element = lo_xml_element ).

    SELECT SINGLE dlvunit FROM tdevc INTO lv_dlvunit WHERE devclass = iv_package.
    IF sy-subrc = 0.
      set_field(
        EXPORTING
          iv_name         = 'SoftwareComponent'
          iv_value        = |{ lv_dlvunit }|
        CHANGING
          co_ixml_element = lo_xml_element ).
    ENDIF.

    lo_xml_element->set_attribute(
      name  = 'Client'
      value = |{ sy-mandt }| ).
    lo_xml_element->set_attribute(
      name  = 'Date'
      value = |{ sy-datum }| ).
    lo_xml_element->set_attribute(
      name  = 'SAPRelease'
      value = |{ sy-saprl }| ).
    lo_xml_element->set_attribute(
      name  = 'Server'
      value = |{ sy-host }| ).
    lo_xml_element->set_attribute(
      name  = 'SourceExportReqID'
      value = lv_transport ).
    lo_xml_element->set_attribute(
      name  = 'SystemID'
      value = |{ sy-sysid }| ).
    lo_xml_element->set_attribute(
      name  = 'Time'
      value = |{ sy-uzeit }| ).
    lo_xml_element->set_attribute(
      name  = 'User'
      value = |{ sy-uname }| ).

  ENDMETHOD.


  METHOD check_is_local.

    SELECT SINGLE local_object FROM fdt_admn_0000s INTO rv_is_local
      WHERE object_type = 'AP'
      AND name = ms_item-obj_name.

  ENDMETHOD.


  METHOD filter_xml_serialize.

    DATA lo_components_node TYPE REF TO if_ixml_element.

    lo_components_node = co_ixml_element->find_from_name( name      = 'ComponentReleases'
                                                          namespace = 'FDTNS' ).
    IF lo_components_node IS BOUND.
      co_ixml_element->remove_child( old_child = lo_components_node ).
    ENDIF.

    " Clear user/time/system-specific fields
    set_field(
      EXPORTING
        iv_name         = 'CreationUser'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'CreationTimestamp'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'ChangeUser'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'ChangeTimestamp'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'User'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Timestamp'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Trrequest'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Trversion'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Trtimestamp'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Trsysid'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Trclient'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'OversId'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'SoftwareComponent'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'DevelopmentPackage'
      CHANGING
        co_ixml_element = co_ixml_element ).

    " Clear attributes of root FDTNS:Fdt node
    co_ixml_element->set_attribute(
      name  = 'Client'
      value = '' ).
    co_ixml_element->set_attribute(
      name  = 'Date'
      value = '' ).
    co_ixml_element->set_attribute(
      name  = 'SAPRelease'
      value = '' ).
    co_ixml_element->set_attribute(
      name  = 'Server'
      value = '' ).
    co_ixml_element->set_attribute(
      name  = 'SourceExportReqID'
      value = '' ).
    co_ixml_element->set_attribute(
      name  = 'SystemID'
      value = '' ).
    co_ixml_element->set_attribute(
      name  = 'Time'
      value = '' ).
    co_ixml_element->set_attribute(
      name  = 'User'
      value = '' ).

  ENDMETHOD.


  METHOD get_application_id.

    SELECT SINGLE application_id FROM fdt_admn_0000s INTO rv_application_id
      WHERE object_type = 'AP'
      AND name = ms_item-obj_name.

  ENDMETHOD.


  METHOD set_field.

    DATA:
      lo_node_collection TYPE REF TO if_ixml_node_collection,
      lo_node            TYPE REF TO if_ixml_node,
      lv_index           TYPE i.

    lo_node_collection = co_ixml_element->get_elements_by_tag_name(
      namespace = 'FDTNS'
      name      = iv_name ).

    lv_index = 0.
    WHILE lv_index < lo_node_collection->get_length( ).
      lo_node = lo_node_collection->get_item( lv_index ).
      lo_node->set_value( iv_value ).
      lv_index = lv_index + 1.
    ENDWHILE.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA lv_ch_user TYPE fdt_admn_0000s-ch_user.

    SELECT SINGLE ch_user FROM fdt_admn_0000s INTO lv_ch_user
      WHERE object_type = 'AP'
      AND name = ms_item-obj_name.

    rv_user = lv_ch_user.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA lv_is_local TYPE abap_bool.
    DATA lt_application_id TYPE TABLE OF fdt_admn_0000s-application_id.
    DATA ls_object_category_sel TYPE if_fdt_query=>s_object_category_sel.
    DATA lv_failure TYPE abap_bool.
    DATA lx_fdt_input TYPE REF TO cx_fdt_input.

    lv_is_local = check_is_local( ).

    SELECT application_id FROM fdt_admn_0000s INTO TABLE lt_application_id
      WHERE object_type = 'AP'
      AND name = ms_item-obj_name.

    ls_object_category_sel-system_objects = 'X'.

    TRY.
        IF lv_is_local = abap_true.

          cl_fdt_delete_handling=>mark_for_delete_via_job(
            EXPORTING
              is_object_category_sel     = ls_object_category_sel
              ita_application_id         = lt_application_id
              iv_background              = abap_true
              iv_local_option            = '1'
              iv_appl_transported_option = '2'
              iv_obj_transported_option  = '2'
            IMPORTING
              ev_failure                 = lv_failure ).
          IF lv_failure IS INITIAL.
            cl_fdt_delete_handling=>delete_logical_via_job(
              EXPORTING
                is_object_category_sel     = ls_object_category_sel
                ita_application_id         = lt_application_id
                iv_retention_time          = 0
                iv_background              = abap_true
                iv_local_option            = '1'
                iv_appl_transported_option = '2'
                iv_obj_transported_option  = '2'
              IMPORTING
                ev_failure                 = lv_failure ).
            IF lv_failure IS INITIAL.
              cl_fdt_delete_handling=>delete_physical_via_job(
                EXPORTING
                  is_object_category_sel     = ls_object_category_sel
                  ita_application_id         = lt_application_id
                  iv_retention_time          = 0
                  iv_background              = abap_true
                  iv_local_option            = '1'
                  iv_appl_transported_option = '2'
                IMPORTING
                  ev_failure                 = lv_failure ).
            ENDIF.
          ENDIF.

        ELSE.

          tadir_insert( iv_package ).

          corr_insert( iv_package ).

          cl_fdt_delete_handling=>mark_for_delete_via_job(
            EXPORTING
              is_object_category_sel     = ls_object_category_sel
              ita_application_id         = lt_application_id
              iv_background              = abap_true
              iv_local_option            = '2'
              iv_appl_transported_option = '1'
              iv_obj_transported_option  = '1'
            IMPORTING
              ev_failure                 = lv_failure ).
          IF lv_failure IS INITIAL.
            cl_fdt_delete_handling=>delete_logical_via_job(
              EXPORTING
                is_object_category_sel     = ls_object_category_sel
                ita_application_id         = lt_application_id
                iv_retention_time          = 0
                iv_background              = abap_true
                iv_local_option            = '2'
                iv_appl_transported_option = '1'
                iv_obj_transported_option  = '1'
              IMPORTING
                ev_failure                 = lv_failure ).
            IF lv_failure IS INITIAL.
              cl_fdt_delete_handling=>delete_physical_via_job(
                EXPORTING
                  is_object_category_sel     = ls_object_category_sel
                  ita_application_id         = lt_application_id
                  iv_retention_time          = 0
                  iv_background              = abap_true
                  iv_local_option            = '2'
                  iv_appl_transported_option = '1'
                IMPORTING
                  ev_failure                 = lv_failure ).
            ENDIF.
          ENDIF.

        ENDIF.

        IF lv_failure = abap_true.
          zcx_abapgit_exception=>raise( |Error deleting { ms_item-obj_type } { ms_item-obj_name }| ).
        ENDIF.

      CATCH cx_fdt_input INTO lx_fdt_input.
        zcx_abapgit_exception=>raise_with_text( lx_fdt_input ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA lo_dexc TYPE REF TO if_fdt_data_exchange.
    DATA lx_fdt_input TYPE REF TO cx_fdt_input.
    DATA lo_dom_tree TYPE REF TO if_ixml_document.
    DATA lv_transportable_package TYPE abap_bool.
    DATA lv_is_local TYPE abap_bool.
    DATA lt_message TYPE if_fdt_types=>t_message.
    DATA lv_create TYPE abap_bool.

    FIELD-SYMBOLS <ls_message> TYPE if_fdt_types=>s_message.

    lo_dom_tree = io_xml->get_raw( ).

    before_xml_deserialize(
      EXPORTING
        iv_package  = iv_package
      IMPORTING
        ev_create   = lv_create
        ev_is_local = lv_is_local
      CHANGING
        co_dom_tree = lo_dom_tree ).

    lv_transportable_package = zcl_abapgit_factory=>get_sap_package( iv_package )->are_changes_recorded_in_tr_req( ).

    IF lv_transportable_package = abap_true AND lv_is_local = abap_true.
      zcx_abapgit_exception=>raise( 'Local applications can only be imported into a local package' ).
    ELSEIF lv_transportable_package = abap_false AND lv_is_local = abap_false.
      zcx_abapgit_exception=>raise( 'Transportable application can only be imported into transportable package' ).
    ENDIF.

    lo_dexc = cl_fdt_factory=>if_fdt_factory~get_instance( )->get_data_exchange( ).

    TRY.

        IF lv_is_local = abap_true. "Local Object

          lo_dexc->import_xml(
            EXPORTING
              io_dom_tree = lo_dom_tree
              iv_create   = lv_create
              iv_activate = abap_true
              iv_simulate = abap_false
            IMPORTING
              et_message  = lt_message ).

        ELSE. "Transportable Object

          tadir_insert( iv_package ).

          corr_insert( iv_package ).

          lo_dexc->import_xml(
            EXPORTING
              io_dom_tree            = lo_dom_tree
              iv_create              = lv_create
              iv_activate            = abap_true
              iv_simulate            = abap_false
              iv_workbench_trrequest = iv_transport
            IMPORTING
              et_message             = lt_message ).

        ENDIF.

        LOOP AT lt_message ASSIGNING <ls_message>.
          ii_log->add(
            iv_msg  = <ls_message>-text
            iv_type = <ls_message>-msgty ).
        ENDLOOP.

      CATCH cx_fdt_input INTO lx_fdt_input.
        zcx_abapgit_exception=>raise_with_text( lx_fdt_input ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA lv_count TYPE i.

    SELECT COUNT( * ) FROM fdt_admn_0000s INTO lv_count
      WHERE object_type = 'AP'
      AND name = ms_item-obj_name
      AND deleted = ''.

    rv_bool = boolc( lv_count > 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.

    DATA lo_local_version_output TYPE REF TO zcl_abapgit_xml_output.
    DATA lo_local_version_input  TYPE REF TO zcl_abapgit_xml_input.

    CREATE OBJECT lo_local_version_output.
    zif_abapgit_object~serialize( lo_local_version_output ).

    CREATE OBJECT lo_local_version_input
      EXPORTING
        iv_xml = lo_local_version_output->zif_abapgit_xml_output~render( ).

    CREATE OBJECT ri_comparator TYPE zcl_abapgit_object_tabl_compar
      EXPORTING
        ii_local = lo_local_version_input.

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

    DATA lv_application_id TYPE fdt_admn_0000s-application_id.
    DATA lx_fdt_input TYPE REF TO cx_fdt_input.
    DATA lo_instance TYPE REF TO if_fdt_admin_data.
    DATA lt_version TYPE if_fdt_admin_data=>ts_version.
    DATA lv_index TYPE sy-tabix.

    FIELD-SYMBOLS <ls_version> LIKE LINE OF lt_version.

    lv_application_id = get_application_id( ).

    TRY.
        cl_fdt_factory=>get_instance_generic(
          EXPORTING
            iv_id       = lv_application_id
          IMPORTING
            eo_instance = lo_instance ).

      CATCH cx_fdt_input INTO lx_fdt_input.
        zcx_abapgit_exception=>raise_with_text( lx_fdt_input ).
    ENDTRY.

    lo_instance->get_versions( IMPORTING ets_version = lt_version ).
    lv_index = lines( lt_version ).
    READ TABLE lt_version ASSIGNING <ls_version> INDEX lv_index.

    rv_active = boolc( <ls_version>-state = 'A' ).

  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    DATA lv_application_id TYPE string.

    lv_application_id = get_application_id( ).

    rv_is_locked = exists_a_lock_entry_for(
      iv_lock_object = 'FDT_ENQUEUE_ID'
      iv_argument    = |$ST{ lv_application_id }| ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA lv_application_id TYPE fdt_admn_0000s-application_id.
    DATA lx_root TYPE REF TO cx_root.
    DATA lo_fdt_wd TYPE REF TO if_fdt_wd_factory.

    lv_application_id = get_application_id( ).

    IF lv_application_id IS NOT INITIAL.
      TRY.
          lo_fdt_wd = cl_fdt_wd_factory=>if_fdt_wd_factory~get_instance( ).
          lo_fdt_wd->get_ui_execution( )->execute_workbench( iv_id = lv_application_id ).
        CATCH cx_root INTO lx_root.
          zcx_abapgit_exception=>raise_with_text( lx_root ).
      ENDTRY.
    ELSE.
      zcx_abapgit_exception=>raise( 'Could not open BRF+ Workbench' ).
    ENDIF.

    rv_exit = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA lo_dexc TYPE REF TO if_fdt_data_exchange.
    DATA lv_application_id TYPE fdt_admn_0000s-application_id.
    DATA lx_fdt_input TYPE REF TO cx_fdt_input.
    DATA lv_xml_fdt0_application TYPE string.
    DATA lo_xml_document TYPE REF TO if_ixml_document.
    DATA lo_xml_element TYPE REF TO if_ixml_element.

    lv_application_id = get_application_id( ).

    lo_dexc = cl_fdt_factory=>if_fdt_factory~get_instance( )->get_data_exchange( ).

    TRY.
        lo_dexc->export_xml_application(
          EXPORTING
            iv_application_id = lv_application_id
            iv_schema         = if_fdt_data_exchange=>gc_xml_schema_type_external
            iv_xml_version    = if_fdt_data_exchange=>gc_xml_version
          IMPORTING
            ev_string         = lv_xml_fdt0_application ).

        lo_xml_document = cl_ixml_80_20=>parse_to_document( stream_string = lv_xml_fdt0_application ).
        lo_xml_element = lo_xml_document->get_root_element( ).

        filter_xml_serialize( CHANGING co_ixml_element = lo_xml_element ).

        io_xml->set_raw( lo_xml_element ).

      CATCH cx_fdt_input INTO lx_fdt_input.
        zcx_abapgit_exception=>raise_with_text( lx_fdt_input ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
