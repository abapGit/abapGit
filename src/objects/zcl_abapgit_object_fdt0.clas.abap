CLASS zcl_abapgit_object_fdt0 DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES zif_abapgit_object .
    ALIASES mo_files
      FOR zif_abapgit_object~mo_files .
  PROTECTED SECTION.

private section.

  methods CHECK_IS_LOCAL
    returning
      value(RV_IS_LOCAL) type ABAP_BOOL .
  methods GET_APPLICATION_ID
    returning
      value(RV_APPLICATION_ID) type FDT_ADMN_0000S-APPLICATION_ID .
  methods WB_REQUEST_CHOICE
    returning
      value(RS_REQUEST) type E070
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods BEFORE_XML_DESERIALIZE
    importing
      !IV_PACKAGE type DEVCLASS
    exporting
      !EV_CREATE type ABAP_BOOL
    changing
      !CO_DOM_TREE type ref to IF_IXML_DOCUMENT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods FILTER_XML_SERIALIZE
    changing
      !CO_IXML_ELEMENT type ref to IF_IXML_ELEMENT
    raising
      ZCX_ABAPGIT_EXCEPTION .
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_FDT0 IMPLEMENTATION.


  METHOD before_xml_deserialize.
    DATA lv_application_id TYPE fdt_admn_0000s-application_id.
    DATA lv_is_local TYPE abap_bool.
    DATA lv_package_xml_value TYPE string.
    DATA lo_node_local TYPE REF TO if_ixml_element.
    DATA lo_node_package TYPE REF TO if_ixml_element.
    DATA lo_node_id TYPE REF TO if_ixml_element.
    DATA lv_count TYPE i.
    DATA lv_create TYPE abap_bool.
    lo_node_local = co_dom_tree->find_from_name(
    name      = 'Local'
    namespace = 'FDTNS' ).

    IF lo_node_local IS BOUND.
      lv_is_local = lo_node_local->get_value( ).
    ENDIF.

    lo_node_package = co_dom_tree->find_from_name(
        name      = 'DevelopmentPackage'
        namespace = 'FDTNS' ).
    IF lo_node_package IS BOUND.
      lv_package_xml_value = iv_package.
      lo_node_package->set_value( value = lv_package_xml_value ).
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
      lv_create = boolc( lv_count = 0 ).
    ENDIF.

  ENDMETHOD.


  METHOD check_is_local.

    SELECT SINGLE local_object FROM fdt_admn_0000s INTO rv_is_local
          WHERE object_type = 'AP'
          AND name = ms_item-obj_name.

  ENDMETHOD.


  method FILTER_XML_SERIALIZE.
  endmethod.


  METHOD get_application_id.

    SELECT SINGLE application_id FROM fdt_admn_0000s INTO rv_application_id
            WHERE object_type = 'AP'
            AND name = ms_item-obj_name.

  ENDMETHOD.


  METHOD wb_request_choice.

    "Transport Request
    CALL FUNCTION 'OXT_REQUEST_CHOICE'
      EXPORTING
        iv_request_types     = 'K'    "Workbench-Request
      IMPORTING
        es_request           = rs_request
      EXCEPTIONS
        invalid_request      = 1
        invalid_request_type = 2
        user_not_owner       = 3
        no_objects_appended  = 4
        enqueue_error        = 5
        cancelled_by_user    = 6
        recursive_call       = 7
        OTHERS               = 8.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    DATA: lv_ch_user TYPE fdt_admn_0000s-ch_user.

    SELECT SINGLE ch_user FROM fdt_admn_0000s INTO lv_ch_user
       WHERE object_type = 'AP'
       AND name = ms_item-obj_name.

    rv_user = lv_ch_user.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA lv_is_local TYPE abap_bool.
    DATA ls_request TYPE e070.
    DATA lt_application_sel TYPE if_fdt_query=>ts_selection.
    DATA ls_application_sel TYPE if_fdt_query=>s_selection.
    DATA ls_object_category_sel TYPE if_fdt_query=>s_object_category_sel.
    DATA lv_failure TYPE abap_bool.

    IF zif_abapgit_object~exists( ) = abap_false.
      " Proxies e.g. delete on its own, nothing todo here then.
      RETURN.
    ENDIF.

    lv_is_local = check_is_local( ).
    IF lv_is_local = abap_false.
      ls_request = wb_request_choice( ).
    ENDIF.

    ls_application_sel-queryfield = 'NAME'.
    ls_application_sel-sign = 'I'.
    ls_application_sel-option = 'EQ'.
    ls_application_sel-low = ms_item-obj_name.
    INSERT ls_application_sel INTO TABLE lt_application_sel.

    ls_object_category_sel-system_objects = 'X'.

    TRY.

        IF lv_is_local = abap_true. "Local Object

          cl_fdt_delete_handling=>delete_logical_via_job(
            EXPORTING
                its_application_selection         = lt_application_sel
                iv_retention_time = 0
                iv_background = abap_true
                iv_local_option = '1'
                iv_appl_transported_option = '2'
                iv_obj_transported_option = '2'
                "iv_marked_option = '3'
                is_object_category_sel = ls_object_category_sel
            IMPORTING
                ev_failure = lv_failure ).


        ELSE. "Transportable Object

          cl_fdt_delete_handling=>delete_logical_via_job(
          EXPORTING
                its_application_selection         = lt_application_sel
                iv_retention_time = 0
                iv_background = abap_true
                iv_local_option = '2'
                iv_appl_transported_option = '1'
                iv_obj_transported_option = '1'
                iv_marked_option = '3'
                is_object_category_sel = ls_object_category_sel
                iv_transport_request_w  = ls_request-trkorr
          IMPORTING
                ev_failure = lv_failure ).

        ENDIF.

        IF lv_failure = abap_true.
          zcx_abapgit_exception=>raise( |Error deleting { ms_item-obj_type } { ms_item-obj_name }| ).
        ENDIF.

      CATCH cx_fdt_input.    "
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA lo_dexc TYPE REF TO if_fdt_data_exchange.
    DATA lx_root TYPE REF TO cx_root.
    DATA lo_dom_tree TYPE REF TO if_ixml_document.
    DATA lv_is_local TYPE abap_bool.
    DATA ls_request TYPE e070.
    DATA lt_message TYPE if_fdt_types=>t_message.
    DATA lv_create TYPE abap_bool.
    FIELD-SYMBOLS <ls_message> TYPE if_fdt_types=>s_message.

    lo_dom_tree = io_xml->get_raw( ).

    CALL METHOD me->before_xml_deserialize
      EXPORTING
        iv_package  = iv_package
      IMPORTING
        ev_create   = lv_create
      CHANGING
        co_dom_tree = lo_dom_tree.

    lo_dexc = cl_fdt_factory=>if_fdt_factory~get_instance( )->get_data_exchange( ).

    TRY.

        IF lv_is_local = abap_true. "Local Object

          lo_dexc->import_xml(
            EXPORTING
              io_dom_tree              = lo_dom_tree
              iv_create                = lv_create
              iv_activate              = abap_true
              iv_simulate              = abap_false
            IMPORTING
              et_message = lt_message   ).

        ELSE. "Transportable Object

          ls_request = wb_request_choice( ).

          lo_dexc->import_xml(
            EXPORTING
              io_dom_tree              = lo_dom_tree
              iv_create                = lv_create
              iv_activate              = abap_true
              iv_simulate              = abap_false
              iv_workbench_trrequest   = ls_request-trkorr
            IMPORTING
              et_message = lt_message ).

        ENDIF.

        LOOP AT lt_message ASSIGNING <ls_message>.
          ii_log->add(
              iv_msg  = <ls_message>-text
              iv_type = <ls_message>-msgty ).
        ENDLOOP.

      CATCH cx_fdt_input INTO lx_root.    "
        zcx_abapgit_exception=>raise( lx_root->get_text( ) ).
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

    DATA: lo_local_version_output TYPE REF TO zcl_abapgit_xml_output,
          lo_local_version_input  TYPE REF TO zcl_abapgit_xml_input.


    CREATE OBJECT lo_local_version_output.
    zif_abapgit_object~serialize( lo_local_version_output ).

    CREATE OBJECT lo_local_version_input
      EXPORTING
        iv_xml = lo_local_version_output->zif_abapgit_xml_output~render( ).

    CREATE OBJECT ri_comparator TYPE zcl_abapgit_object_tabl_compar
      EXPORTING
        ii_local = lo_local_version_input.

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
    DATA lv_index    LIKE sy-tabix.
    FIELD-SYMBOLS <ls_version> LIKE LINE OF lt_version.

    lv_application_id = get_application_id( ).

    TRY.
        cl_fdt_factory=>get_instance_generic( EXPORTING iv_id         = lv_application_id
                                            IMPORTING eo_instance   = lo_instance ).
      CATCH cx_fdt_input INTO lx_fdt_input.    "
        zcx_abapgit_exception=>raise( lx_fdt_input->get_text( ) ).
    ENDTRY.

    lo_instance->get_versions( IMPORTING ets_version = lt_version ).
    lv_index = lines( lt_version ).
    READ TABLE lt_version ASSIGNING <ls_version> INDEX lv_index.

    rv_active = boolc( <ls_version>-state = 'A' ).

  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    DATA lv_application_id TYPE string.

    lv_application_id = get_application_id( ).
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'FDT_ENQUEUE_ID'
                                            iv_argument    = |$ST{ lv_application_id }| ).
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA lv_application_id TYPE fdt_admn_0000s-application_id.
    DATA lx_root TYPE REF TO cx_root.

    lv_application_id = get_application_id( ).

    IF lv_application_id IS NOT INITIAL.
      TRY.
          cl_fdt_wd_factory=>if_fdt_wd_factory~get_instance( )->get_ui_execution( )->execute_workbench(
              iv_id           = lv_application_id ).
        CATCH cx_root INTO lx_root.
          zcx_abapgit_exception=>raise( lx_root->get_text( ) ).
      ENDTRY.
    ELSE.
      zcx_abapgit_exception=>raise( 'Could not open BRF+ Workbench' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA lo_dexc TYPE REF TO if_fdt_data_exchange.
    DATA lv_application_id TYPE fdt_admn_0000s-application_id.
    DATA lx_root TYPE REF TO cx_root.
    DATA lv_xml_fdt0_application TYPE string.
    DATA lo_xml_document TYPE REF TO if_ixml_document.
    DATA lo_xml_element TYPE REF TO if_ixml_element.

    lv_application_id = get_application_id( ).

    lo_dexc = cl_fdt_factory=>if_fdt_factory~get_instance( )->get_data_exchange( ).
    TRY.
        lo_dexc->export_xml_application(
          EXPORTING
            iv_application_id = lv_application_id
            iv_schema  = if_fdt_data_exchange=>gc_xml_schema_type_external
            iv_xml_version = if_fdt_data_exchange=>gc_xml_version
          IMPORTING
            ev_string         = lv_xml_fdt0_application ).

        lo_xml_document = cl_ixml_80_20=>parse_to_document( stream_string = lv_xml_fdt0_application ).
        lo_xml_element = lo_xml_document->get_root_element( ).

        filter_xml_serialize(
          CHANGING
            co_ixml_element = lo_xml_element                 " Element of an XML Document
        ).

        io_xml->set_raw( lo_xml_element ).

      CATCH cx_fdt_input INTO lx_root.    "
        zcx_abapgit_exception=>raise( lx_root->get_text( ) ).
    ENDTRY.


  ENDMETHOD.
ENDCLASS.
