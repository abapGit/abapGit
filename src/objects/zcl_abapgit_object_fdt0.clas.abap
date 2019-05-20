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

  PRIVATE SECTION.

    METHODS check_is_local
      RETURNING
        VALUE(rv_is_local) TYPE boole_d.
    METHODS get_application_id
      RETURNING
        VALUE(rv_application_id) TYPE fdt_admn_0000s-application_id.
    METHODS wb_request_choice
      RETURNING
        VALUE(rs_request) TYPE e070
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_FDT0 IMPLEMENTATION.


  METHOD check_is_local.

    SELECT SINGLE local_object FROM fdt_admn_0000s INTO rv_is_local
          WHERE object_type = 'AP'
          AND name = ms_item-obj_name.

  ENDMETHOD.


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

    DATA ls_transpkey TYPE trkey.
    DATA lv_is_local TYPE boole_d.
    DATA ls_request TYPE e070.
    DATA lt_application_id TYPE if_fdt_types=>t_object_id.
    DATA lv_application_id TYPE fdt_admn_0000s-application_id.

    IF zif_abapgit_object~exists( ) = abap_false.
      " Proxies e.g. delete on its own, nothing todo here then.
      RETURN.
    ENDIF.

    lv_is_local = check_is_local( ).
    ls_request = wb_request_choice( ).
    lv_application_id = get_application_id( ).

    INSERT lv_application_id INTO TABLE lt_application_id.

    TRY.

        IF lv_is_local = abap_true. "Local Object

          cl_fdt_delete_handling=>delete_logical_via_job(
            EXPORTING
                ita_application_id         = lt_application_id
                iv_background = abap_true
          ).

          cl_fdt_delete_handling=>delete_physical_via_job(
             EXPORTING
                ita_application_id         = lt_application_id
                iv_background = abap_true
          ).

        ELSE. "Transportable Object

          cl_fdt_delete_handling=>delete_logical_via_job(
            EXPORTING
                ita_application_id         = lt_application_id
                iv_transport_request_w  = ls_request-trkorr
                iv_background = abap_true
          ).

          cl_fdt_delete_handling=>delete_physical_via_job(
             EXPORTING
                ita_application_id         = lt_application_id
                iv_transport_request_w  = ls_request-trkorr
                iv_background = abap_true
          ).

        ENDIF.

      CATCH cx_fdt_input.    "
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA lo_dexc TYPE REF TO if_fdt_data_exchange.
    DATA lv_application_id TYPE fdt_admn_0000s-application_id.
    DATA lx_root TYPE REF TO cx_root.
    DATA lv_xml_fdt0_application TYPE string.
    DATA lo_xml_document TYPE REF TO if_ixml_document.
    DATA lo_dom_tree TYPE REF TO if_ixml_document.
    DATA lv_is_local TYPE boole_d.
    DATA lv_object TYPE string.
    DATA lv_objectclass TYPE string.
    DATA: ls_request TYPE e070.

    lo_dom_tree = io_xml->get_raw( ).

    lo_dexc = cl_fdt_factory=>get_instance( )->get_data_exchange( ).

    lv_is_local = check_is_local( ).

    TRY.

        IF lv_is_local = abap_true. "Local Object

          lo_dexc->import_xml(
            EXPORTING
              io_dom_tree              = lo_dom_tree
              iv_create                = abap_true
              iv_activate              = abap_true
              iv_simulate              = abap_false
          ).

        ELSE. "Transportable Object

          ls_request = wb_request_choice( ).

          lo_dexc->import_xml(
            EXPORTING
              io_dom_tree              = lo_dom_tree
              iv_create                = abap_true
              iv_activate              = abap_true
              iv_simulate              = abap_false
              iv_workbench_trrequest   = ls_request-trkorr
          ).

        ENDIF.

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

    IF lv_count > 0.
      rv_bool = abap_true.
    ELSE.
      rv_bool = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.

    DATA: lo_local_version_output TYPE REF TO zcl_abapgit_xml_output,
          lo_local_version_input  TYPE REF TO zcl_abapgit_xml_input.


    CREATE OBJECT lo_local_version_output.
    me->zif_abapgit_object~serialize( lo_local_version_output ).

    CREATE OBJECT lo_local_version_input
      EXPORTING
        iv_xml = lo_local_version_output->render( ).

    CREATE OBJECT ri_comparator TYPE zcl_abapgit_object_tabl_compar
      EXPORTING
        io_local = lo_local_version_input.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.

    DATA: ls_meta TYPE zif_abapgit_definitions=>ty_metadata.

    ls_meta = zif_abapgit_object~get_metadata( ).

    IF ls_meta-late_deser = abap_true.
      APPEND zif_abapgit_object=>gc_step_id-late TO rt_steps.
    ELSEIF ls_meta-ddic = abap_true.
      APPEND zif_abapgit_object=>gc_step_id-ddic TO rt_steps.
    ELSE.
      APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
    ENDIF.

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

    IF <ls_version>-state = 'A'. "Check if BRF+-Application is active
      rv_active = abap_true.
    ELSE.
      rv_active = abap_false.
    ENDIF.

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
            EXPORTING
              iv_id           = lv_application_id
          ).
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

    lv_application_id = get_application_id( ).

    lo_dexc = cl_fdt_factory=>get_instance( )->get_data_exchange( ).
    TRY.
        lo_dexc->export_xml_application(
          EXPORTING
            iv_application_id = lv_application_id
            iv_schema  = if_fdt_data_exchange=>gc_xml_schema_type_external
            iv_incl_deleted = abap_false
          IMPORTING
            ev_string         = lv_xml_fdt0_application
        ).

        lo_xml_document = cl_ixml_80_20=>parse_to_document( stream_string = lv_xml_fdt0_application ).
        "zcl_abapgit_object_sfpf=>fix_oref( li_document ).
        io_xml->set_raw( lo_xml_document->get_root_element( ) ).

      CATCH cx_fdt_input INTO lx_root.    "
        zcx_abapgit_exception=>raise( lx_root->get_text( ) ).
    ENDTRY.


  ENDMETHOD.
ENDCLASS.
