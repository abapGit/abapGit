"Internally, BOPF is BOBF. This nomenclature is followed here.
CLASS lcl_object_bobf DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.
    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras .
  PRIVATE SECTION.
    DATA:
      mv_key TYPE /bobf/obm_bo_key.
    METHODS:
      read_bobf_key
        RETURNING
          VALUE(rv_key) TYPE /bobf/obm_bo_key,
      read_details
        IMPORTING
          iv_key            TYPE /bobf/obm_bo_key
        RETURNING
          VALUE(rs_details) TYPE /bobf/s_conf_model_api_bo
        RAISING
          zcx_abapgit_exception,
      create_bcdata
        IMPORTING
          iv_program       TYPE bdc_prog OPTIONAL
          iv_dynpro        TYPE bdc_dynr OPTIONAL
          iv_dynbegin      TYPE bdc_start OPTIONAL
          iv_fnam          TYPE fnam_____4 OPTIONAL
          iv_fval          TYPE bdc_fval OPTIONAL
        RETURNING
          VALUE(rs_bcdata) TYPE bdcdata.
ENDCLASS.

CLASS lcl_object_bobf IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    mv_key = read_bobf_key( ).
  ENDMETHOD.

  METHOD lif_object~has_changed_since.
    DATA ls_details TYPE /bobf/s_conf_model_api_bo.

    ls_details = read_details( mv_key ).

    IF ls_details-change_time > iv_timestamp.
      rv_changed = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD lif_object~changed_by.
    DATA ls_details TYPE /bobf/s_conf_model_api_bo.

    ls_details = read_details( mv_key ).
    rv_user = ls_details-change_user.
  ENDMETHOD.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.

  METHOD lif_object~exists.
    IF mv_key IS NOT INITIAL.
      rv_bool = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD lif_object~jump.
    DATA:
      lv_field_value TYPE bdc_fval,
      lt_bcdata TYPE STANDARD TABLE OF bdcdata.

    APPEND create_bcdata(
      iv_program  = '/BOBF/CONF_UI'
      iv_dynpro   = '0100'
      iv_dynbegin = 'X'
    ) TO lt_bcdata.
    APPEND create_bcdata(
      iv_fnam = 'BDC_OKCODE'
      iv_fval = '=OPEN'
    ) TO lt_bcdata.
    APPEND create_bcdata(
      iv_fnam = 'BDC_SUBSCR'
      iv_fval = '/BOBF/CONF_UI'
    ) TO lt_bcdata.

    APPEND create_bcdata(
      iv_program  = '/BOBF/CONF_UI'
      iv_dynpro   = '0110'
      iv_dynbegin = 'X'
    ) TO lt_bcdata.
    APPEND create_bcdata(
      iv_fnam = 'BDC_CURSOR'
      iv_fval = '/BOBF/S_CONF_UI-BO_NAME'
    ) TO lt_bcdata.

    lv_field_value = ms_item-obj_name.
    APPEND create_bcdata(
      iv_fnam = '/BOBF/S_CONF_UI-BO_NAME'
      iv_fval = lv_field_value
    ) TO lt_bcdata.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode     = '/BOBF/CONF_UI'
        mode_val  = 'E'
      TABLES
        using_tab = lt_bcdata
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from ABAP4_CALL_TRANSACTION, BOBF' ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_object~delete.
    DATA lv_success TYPE abap_bool.
    lv_success = /bobf/cl_conf_model_api=>delete_business_object( mv_key ).
    IF lv_success = abap_false.
      zcx_abapgit_exception=>raise( `error when deleting BOBF ` && ms_item-obj_name ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_object~serialize.
    DATA:
      ls_details   TYPE /bobf/s_conf_model_api_bo,
      ls_root_node TYPE  /bobf/s_conf_model_api_node,
      lv_success   TYPE abap_bool.
    ls_details = read_details( mv_key ).

    io_xml->add( iv_name = 'DETAILS'
                 ig_data = ls_details ).

    /bobf/cl_conf_model_api=>get_node(
      EXPORTING
        iv_bo_key   = mv_key
        iv_node_key = ls_details-root_node_key
      IMPORTING
        es_node     = ls_root_node
        ev_success  = lv_success ).
    IF lv_success = abap_false.
      zcx_abapgit_exception=>raise( 'error from BOBF, GET_BO' ).
    ENDIF.

    io_xml->add( iv_name = 'ROOT_NODE'
                 ig_data = ls_root_node ).
  ENDMETHOD.

  METHOD lif_object~deserialize.
    DATA:
      ls_details   TYPE /bobf/s_conf_model_api_bo,
      ls_root_node TYPE /bobf/s_conf_model_api_node,
      lv_root_name TYPE /bobf/obm_name,
      lv_success   TYPE abap_bool.

    io_xml->read( EXPORTING iv_name = 'DETAILS'
                  CHANGING cg_data = ls_details ).
    io_xml->read( EXPORTING iv_name = 'ROOT_NODE'
                  CHANGING cg_data = ls_root_node ).

    lv_root_name = ls_root_node-node_name.
    /bobf/cl_conf_model_api=>create_business_object(
      EXPORTING
        iv_business_object_name  = ls_details-bo_name
        iv_description           = ls_details-description
        iv_constant_interface    = ls_details-const_interface
        iv_root_name             = lv_root_name
        iv_root_description      = ls_root_node-description
        iv_root_data_type        = ls_root_node-data_type
        iv_root_data_data_type   = ls_root_node-data_data_type
        iv_root_data_data_type_t = ls_root_node-data_data_type_t
        iv_root_data_table_type  = ls_root_node-data_table_type
        iv_root_database_table   = ls_root_node-database_table
      IMPORTING
        ev_bo_key                = mv_key
        ev_success               = lv_success ).
    IF lv_success = abap_false.
      zcx_abapgit_exception=>raise( `Error when deserializing BOBF ` && ms_item-obj_name ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_comparison_null.
  ENDMETHOD.

  METHOD read_bobf_key.
    DATA lv_bobf_name TYPE /bobf/obm_name.
    lv_bobf_name = ms_item-obj_name.
    rv_key = /bobf/cl_conf_model_api_reuse=>get_bo_key_by_name( lv_bobf_name ).
  ENDMETHOD.

  METHOD read_details.
    DATA lv_success TYPE abap_bool.
    /bobf/cl_conf_model_api=>get_bo(
      EXPORTING
        iv_bo_key  = iv_key
      IMPORTING
        es_bo      = rs_details
        ev_success = lv_success ).
    IF lv_success = abap_false.
      zcx_abapgit_exception=>raise( `Error from BOBF, GET_BO ` && ms_item-obj_name ).
    ENDIF.
  ENDMETHOD.

  METHOD create_bcdata.
    rs_bcdata-dynbegin = iv_dynbegin.
    rs_bcdata-dynpro   = iv_dynpro.
    rs_bcdata-fnam     = iv_fnam.
    rs_bcdata-fval     = iv_fval.
    rs_bcdata-program  = iv_program.
  ENDMETHOD.

ENDCLASS.
