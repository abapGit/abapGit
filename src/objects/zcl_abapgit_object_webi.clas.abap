CLASS zcl_abapgit_object_webi DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_webi,
             veptext         TYPE veptext,
             pvepheader      TYPE STANDARD TABLE OF vepheader WITH DEFAULT KEY,
             pvepfunction    TYPE STANDARD TABLE OF vepfunction WITH DEFAULT KEY,
             pvepfault       TYPE STANDARD TABLE OF vepfault WITH DEFAULT KEY,
             pvepparameter   TYPE STANDARD TABLE OF vepparameter WITH DEFAULT KEY,
             pveptype        TYPE STANDARD TABLE OF veptype WITH DEFAULT KEY,
             pvepelemtype    TYPE STANDARD TABLE OF vepelemtype WITH DEFAULT KEY,
             pveptabletype   TYPE STANDARD TABLE OF veptabletype WITH DEFAULT KEY,
             pvepstrutype    TYPE STANDARD TABLE OF vepstrutype WITH DEFAULT KEY,
             pveptypesoapext TYPE STANDARD TABLE OF veptypesoapext WITH DEFAULT KEY,
             pvepeletypsoap  TYPE STANDARD TABLE OF vepeletypsoap WITH DEFAULT KEY,
             pveptabtypsoap  TYPE STANDARD TABLE OF veptabtypsoap WITH DEFAULT KEY,
             pvepfuncsoapext TYPE STANDARD TABLE OF vepfuncsoapext WITH DEFAULT KEY,
             pvepfieldref    TYPE STANDARD TABLE OF vepfieldref WITH DEFAULT KEY,
             pvependpoint    TYPE STANDARD TABLE OF vependpoint WITH DEFAULT KEY,
             pvepvisoapext   TYPE STANDARD TABLE OF vepvisoapext WITH DEFAULT KEY,
             pvepparasoapext TYPE STANDARD TABLE OF vepparasoapext WITH DEFAULT KEY,
             pwsheader       TYPE STANDARD TABLE OF wsheader WITH DEFAULT KEY,
             pwssoapprop     TYPE STANDARD TABLE OF wssoapprop WITH DEFAULT KEY,
           END OF ty_webi.

    DATA: mi_vi TYPE REF TO if_ws_md_vif.

    METHODS:
      handle_endpoint
        IMPORTING is_webi TYPE ty_webi
        RAISING   zcx_abapgit_exception
                  cx_ws_md_exception,
      handle_types
        IMPORTING is_webi TYPE ty_webi
        RAISING   zcx_abapgit_exception
                  cx_ws_md_exception,
      handle_soap
        IMPORTING is_webi TYPE ty_webi
        RAISING   zcx_abapgit_exception
                  cx_ws_md_exception,
      handle_function
        IMPORTING is_webi TYPE ty_webi
        RAISING   zcx_abapgit_exception
                  cx_ws_md_exception.
    METHODS handle_single_parameter
      IMPORTING
        iv_parameter_type   TYPE vepparamtype
        iv_name             TYPE vepparameter-vepparam
        ii_function         TYPE REF TO if_ws_md_vif_func
      RETURNING
        VALUE(ri_parameter) TYPE REF TO if_ws_md_vif_param
      RAISING
        zcx_abapgit_exception
        cx_ws_md_exception.
    METHODS sort
      CHANGING
        cs_webi TYPE ty_webi.

ENDCLASS.



CLASS zcl_abapgit_object_webi IMPLEMENTATION.


  METHOD handle_endpoint.

    DATA: ls_endpoint LIKE LINE OF is_webi-pvependpoint,
          li_endpoint TYPE REF TO if_ws_md_vif_endpoint_ref.

    FIELD-SYMBOLS: <ls_function> LIKE LINE OF is_webi-pvepfunction.


    READ TABLE is_webi-pvependpoint INDEX 1 INTO ls_endpoint.
    ASSERT sy-subrc = 0.

    IF mi_vi->has_endpoint_reference( sews_c_vif_version-all ) = abap_true.
      RETURN.
    ENDIF.

    li_endpoint = mi_vi->create_endpoint_reference(
      endpoint_type          = ls_endpoint-endpointtype
      service_def_startpoint = ls_endpoint-def_start_pt
      auto_generated         = ls_endpoint-auto_generated
      i_is_srvv              = ls_endpoint-is_srvv ).

    IF ls_endpoint-endpointtype = 'BAPI'.
* it looks like some special handling is needed when calling
* set_data, and looking at the cluster data LS_ENDPOINT-CLUSTD
      zcx_abapgit_exception=>raise( 'todo, WEBI BAPI' ).
    ENDIF.

* field ls_endpoint-endpointname does not exist in 702
    READ TABLE is_webi-pvepfunction INDEX 1 ASSIGNING <ls_function>.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |WEBI { ms_item-obj_name }: couldn't detect endpoint name| ).
    ENDIF.

    li_endpoint->set_data(
      data_version = '1'
      data         = <ls_function>-function ).

  ENDMETHOD.


  METHOD handle_function.



    DATA: li_parameter TYPE REF TO if_ws_md_vif_param,
          li_soap      TYPE REF TO if_ws_md_soap_ext_func,
          li_fault     TYPE REF TO if_ws_md_vif_fault,
          li_function  TYPE REF TO if_ws_md_vif_func.

    FIELD-SYMBOLS: <ls_function>  LIKE LINE OF is_webi-pvepfunction,
                   <ls_soap>      LIKE LINE OF is_webi-pvepfuncsoapext,
                   <ls_fault>     LIKE LINE OF is_webi-pvepfault,
                   <ls_parameter> LIKE LINE OF is_webi-pvepparameter.


    LOOP AT is_webi-pvepfunction ASSIGNING <ls_function>.

      IF mi_vi->has_function( funcname = <ls_function>-function
                              version  = sews_c_vif_version-active ) = abap_true.
        CONTINUE.
      ENDIF.

      IF mi_vi->has_function( funcname = <ls_function>-function
                              version  = sews_c_vif_version-inactive ) = abap_true.

        li_function = mi_vi->get_function( funcname = <ls_function>-function
                                           version  = sews_c_vif_version-inactive ).

      ELSE.

        li_function = mi_vi->create_function( funcname    = <ls_function>-function
                                              mapped_name = <ls_function>-mappedname ).

      ENDIF.

      li_function->set_is_exposed( <ls_function>-is_exposed ).

      LOOP AT is_webi-pvepparameter ASSIGNING <ls_parameter>
          WHERE function = <ls_function>-function.

        li_parameter = handle_single_parameter( iv_name           = <ls_parameter>-vepparam
                                                ii_function       = li_function
                                                iv_parameter_type = <ls_parameter>-vepparamtype ).

        li_parameter->set_name_mapped_to( <ls_parameter>-mappedname ).
        li_parameter->set_is_exposed( <ls_parameter>-is_exposed ).
        li_parameter->set_is_optional( <ls_parameter>-is_optional ).
        li_parameter->set_default_value( <ls_parameter>-default_value ).
        li_parameter->set_initial( <ls_parameter>-is_initial ).
        li_parameter->set_type( <ls_parameter>-typename ).
      ENDLOOP.

      LOOP AT is_webi-pvepfuncsoapext ASSIGNING <ls_soap>
          WHERE function = <ls_function>-function.
        IF li_function->has_soap_extension_function( 'I' ) = abap_true.
          li_function->delete_soap_extension_function( ).
        ENDIF.
        li_soap = li_function->create_soap_extension_function( ).
        li_soap->set_soap_request_name( <ls_soap>-requestname ).
        li_soap->set_soap_response_name( <ls_soap>-responsename ).
        li_soap->set_namespace( <ls_soap>-namespace ).
      ENDLOOP.

      LOOP AT is_webi-pvepfault ASSIGNING <ls_fault>
          WHERE function = <ls_function>-function.
        li_fault = li_function->create_fault( <ls_fault>-fault ).
        li_fault->set_name_mapped_to( <ls_fault>-mappedname ).
        li_fault->set_detail( <ls_fault>-detail ).
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD handle_single_parameter.
    CONSTANTS:
      BEGIN OF lc_parameter_type,
        import TYPE vepparamtype VALUE 'I',
        export TYPE vepparamtype VALUE 'O',
      END OF lc_parameter_type.

    CASE iv_parameter_type.
      WHEN lc_parameter_type-import.
        ri_parameter = ii_function->get_incoming_parameter( parameter_name  = iv_name
                                                            version         = 'I' ).
        IF ri_parameter IS BOUND.
          ii_function->delete_incoming_parameter( ri_parameter ).
        ENDIF.
        ri_parameter = ii_function->create_incoming_parameter( iv_name ).

      WHEN lc_parameter_type-export.

        ri_parameter = ii_function->get_outgoing_parameter( parameter_name  = iv_name
                                                            version         = 'I' ).
        IF ri_parameter IS BOUND.
          ii_function->delete_outgoing_parameter( parameter = ri_parameter ).
        ENDIF.

        ri_parameter = ii_function->create_outgoing_parameter( iv_name ).

      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.


  METHOD handle_soap.

    DATA: li_soap TYPE REF TO if_ws_md_soap_ext_virtinfc,
          ls_soap LIKE LINE OF is_webi-pvepvisoapext.


    READ TABLE is_webi-pvepvisoapext INDEX 1 INTO ls_soap.
    ASSERT sy-subrc = 0.

    IF mi_vi->has_soap_extension_virtinfc( sews_c_vif_version-active ) = abap_true.
      RETURN.
    ENDIF.

    IF mi_vi->has_soap_extension_virtinfc( sews_c_vif_version-inactive ) = abap_true.
      li_soap = mi_vi->get_soap_extension_virtinfc( sews_c_vif_version-inactive ).
    ELSE.
      li_soap = mi_vi->create_soap_extension_virtinfc( ls_soap-soap_appl_uri ).
    ENDIF.

    li_soap->set_namespace( ls_soap-namespace ).

  ENDMETHOD.


  METHOD handle_types.

    DATA: lv_index TYPE i,
          li_soap  TYPE REF TO if_ws_md_soap_extension_type,
          li_struc TYPE REF TO if_ws_md_vif_struc_type,
          li_field TYPE REF TO if_ws_md_vif_field,
          li_table TYPE REF TO if_ws_md_vif_table_type,
          li_elem  TYPE REF TO if_ws_md_vif_elem_type.

    FIELD-SYMBOLS: <ls_elem>  LIKE LINE OF is_webi-pvepelemtype,
                   <ls_table> LIKE LINE OF is_webi-pveptabletype,
                   <ls_soap>  LIKE LINE OF is_webi-pveptypesoapext,
                   <ls_struc> LIKE LINE OF is_webi-pvepstrutype.


    LOOP AT is_webi-pvepelemtype ASSIGNING <ls_elem>.
      li_elem = mi_vi->create_type_as_elementary( <ls_elem>-typename ).
      li_elem->set_built_in_type( <ls_elem>-build_in_type ).
      li_elem->set_decimals( <ls_elem>-decimals ).
      li_elem->set_kind( <ls_elem>-kind ).
      li_elem->set_length( <ls_elem>-length ).
      li_elem->set_signed( <ls_elem>-signed ).
      li_elem->set_abaptype( <ls_elem>-abaptype ).

      IF li_elem->if_ws_md_vif_type~has_soap_extension_type( sews_c_vif_version-all ) = abap_false.
        READ TABLE is_webi-pveptypesoapext ASSIGNING <ls_soap>
          WITH KEY typename = <ls_elem>-typename.
        IF sy-subrc = 0.
          li_soap = li_elem->if_ws_md_vif_type~create_soap_extension_type( ).
          li_soap->set_namespace( <ls_soap>-namespace ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT is_webi-pvepstrutype ASSIGNING <ls_struc>.
      lv_index = sy-tabix.

      li_struc = mi_vi->create_type_as_structure( <ls_struc>-typename ).

      IF li_struc->has_field( field_pos = <ls_struc>-fieldpos
          version = sews_c_vif_version-active ) = abap_true.
        CONTINUE.
      ENDIF.

      li_field = li_struc->create_field(
        field_name = <ls_struc>-fieldname
        fieldpos = <ls_struc>-fieldpos ).
      li_field->set_type( mi_vi->get_type( typename = <ls_struc>-typeref
                                           version  = sews_c_vif_version-inactive ) ).

      IF lv_index = 1
          AND li_struc->if_ws_md_vif_type~has_soap_extension_type(
          sews_c_vif_version-all ) = abap_false.
        READ TABLE is_webi-pveptypesoapext ASSIGNING <ls_soap>
          WITH KEY typename = <ls_struc>-typename.
        IF sy-subrc = 0.
          li_soap = li_struc->if_ws_md_vif_type~create_soap_extension_type( ).
          li_soap->set_namespace( <ls_soap>-namespace ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT is_webi-pveptabletype ASSIGNING <ls_table>.
      li_table = mi_vi->create_type_as_table( <ls_table>-typename ).
      li_table->set_line_type( mi_vi->get_type( typename = <ls_table>-typeref
                                                version  = sews_c_vif_version-inactive ) ).

      IF li_table->if_ws_md_vif_type~has_soap_extension_type( sews_c_vif_version-all ) = abap_false.
        READ TABLE is_webi-pveptypesoapext ASSIGNING <ls_soap>
          WITH KEY typename = <ls_table>-typename.
        IF sy-subrc = 0.
          li_soap = li_table->if_ws_md_vif_type~create_soap_extension_type( ).
          li_soap->set_namespace( <ls_soap>-namespace ).
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD sort.
    SORT cs_webi-pvepheader BY vepname version.
    SORT cs_webi-pvepfunction BY vepname version function.
    SORT cs_webi-pvepfault BY vepname version function fault.
    SORT cs_webi-pvepparameter BY vepname version function vepparam vepparamtype.
    SORT cs_webi-pveptype BY vepname version typename.
    SORT cs_webi-pvepelemtype BY vepname version typename.
    SORT cs_webi-pveptabletype BY vepname version typename.
    SORT cs_webi-pvepstrutype BY vepname version typename fieldpos.
    SORT cs_webi-pveptypesoapext BY vepname version typename.
    SORT cs_webi-pvepeletypsoap BY vepname version typename assign_type assign_data1 assign_data2.
    SORT cs_webi-pveptabtypsoap BY vepname version typename.
    SORT cs_webi-pvepfuncsoapext BY vepname version function.
    SORT cs_webi-pvepfieldref BY vepname version function vepparam vepparamtype strucid fieldname.
    SORT cs_webi-pvependpoint BY relid vepname version sortfield.
    SORT cs_webi-pvepvisoapext BY vepname version.
    SORT cs_webi-pvepparasoapext BY vepname version function vepparam vepparamtype.
    SORT cs_webi-pwsheader BY wsname version.
    SORT cs_webi-pwssoapprop BY wsname version feature soapapp funcref propnum.
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE changedby FROM vepheader INTO rv_user
      WHERE vepname = ms_item-obj_name AND version = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_name TYPE vepname,
          lo_vif  TYPE REF TO cl_ws_md_vif_root.


    lv_name = ms_item-obj_name.

    CREATE OBJECT lo_vif.
    TRY.
        lo_vif->if_ws_md_vif_root~delete_virtual_interface( lv_name ).
      CATCH cx_ws_md_exception.
        zcx_abapgit_exception=>raise( 'error deleting WEBI' ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_webi     TYPE ty_webi,
          lv_name     TYPE vepname,
          ls_header   LIKE LINE OF ls_webi-pvepheader,
          lx_root     TYPE REF TO cx_root,
          lv_exists   TYPE abap_bool,
          li_root     TYPE REF TO if_ws_md_vif_root,
          ls_endpoint LIKE LINE OF ls_webi-pvependpoint.

    io_xml->read( EXPORTING iv_name = 'WEBI'
                  CHANGING  cg_data = ls_webi ).

    lv_name = ms_item-obj_name.

    READ TABLE ls_webi-pvependpoint INDEX 1 INTO ls_endpoint.
    ASSERT sy-subrc = 0.
    IF ls_endpoint-auto_generated = abap_true.
      " handled by SPRX.
      RETURN.
    ENDIF.

    READ TABLE ls_webi-pvepheader INDEX 1 INTO ls_header.
    ASSERT sy-subrc = 0.

    lv_exists = cl_ws_md_vif_root=>check_existence_by_vif_name(
      name      = lv_name
      i_version = sews_c_vif_version-all ).

    li_root = cl_ws_md_factory=>get_vif_root( ).
    TRY.
        IF lv_exists = abap_false.
          mi_vi = li_root->create_virtual_interface(
            name    = lv_name
            nameext = ls_header-vepnameext ).
        ELSE.
          mi_vi = li_root->get_virtual_interface( lv_name ).
          mi_vi->if_ws_md_lockable_object~lock( ).
        ENDIF.

        mi_vi->set_short_text( ls_webi-veptext ).

        handle_endpoint( ls_webi ).
        handle_types( ls_webi ).
        handle_function( ls_webi ).
        handle_soap( ls_webi ).

        tadir_insert( iv_package ).

        mi_vi->if_ws_md_lockable_object~save( ).
        mi_vi->if_ws_md_lockable_object~unlock( ).
      CATCH cx_ws_md_exception INTO lx_root.
        TRY.
            mi_vi->if_ws_md_lockable_object~unlock( ).
          CATCH cx_ws_md_exception ##NO_HANDLER.
        ENDTRY.
        zcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.

    zcl_abapgit_objects_activation=>add_item( ms_item ).

    zcl_abapgit_sotr_handler=>create_sotr(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_name TYPE vepname.
    DATA lv_generated TYPE abap_bool.

    lv_name = ms_item-obj_name.

    " Check if service is generated by proxy
    SELECT SINGLE auto_generated FROM vependpoint INTO lv_generated
      WHERE vepname = lv_name AND version = sews_c_vif_version-active.
    IF sy-subrc = 0 AND lv_generated = abap_true.
      RETURN.
    ENDIF.

    rv_bool = cl_ws_md_vif_root=>check_existence_by_vif_name(
      name      = lv_name
      i_version = sews_c_vif_version-all ).

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

    DATA: ls_webi    TYPE ty_webi,
          lx_error   TYPE REF TO cx_ws_md_exception,
          lt_modilog TYPE STANDARD TABLE OF smodilog WITH DEFAULT KEY,
          li_vi      TYPE REF TO if_ws_md_vif,
          lv_name    TYPE vepname.

    FIELD-SYMBOLS: <ls_vepheader>   LIKE LINE OF ls_webi-pvepheader,
                   <ls_vependpoint> LIKE LINE OF ls_webi-pvependpoint,
                   <ls_wsheader>    TYPE wsheader.

    CALL FUNCTION 'WEBI_GET_OBJECT'
      EXPORTING
        webiname          = ms_item-obj_name
      TABLES
        psmodilog         = lt_modilog
        pvepheader        = ls_webi-pvepheader
        pvepfunction      = ls_webi-pvepfunction
        pvepfault         = ls_webi-pvepfault
        pvepparameter     = ls_webi-pvepparameter
        pveptype          = ls_webi-pveptype
        pvepelemtype      = ls_webi-pvepelemtype
        pveptabletype     = ls_webi-pveptabletype
        pvepstrutype      = ls_webi-pvepstrutype
        pveptypesoapext   = ls_webi-pveptypesoapext
        pvepeletypsoap    = ls_webi-pvepeletypsoap
        pveptabtypsoap    = ls_webi-pveptabtypsoap
        pvepfuncsoapext   = ls_webi-pvepfuncsoapext
        pvepfieldref      = ls_webi-pvepfieldref
        pvependpoint      = ls_webi-pvependpoint
        pvepvisoapext     = ls_webi-pvepvisoapext
        pvepparasoapext   = ls_webi-pvepparasoapext
        pwsheader         = ls_webi-pwsheader
        pwssoapprop       = ls_webi-pwssoapprop
      EXCEPTIONS
        version_not_found = 1
        webi_not_exist    = 2
        OTHERS            = 3.
    IF sy-subrc = 1.
      " no active version
      RETURN.
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    sort( CHANGING cs_webi = ls_webi ).

    lv_name = ms_item-obj_name.
    TRY.
        li_vi = cl_ws_md_factory=>get_vif_root( )->get_virtual_interface( lv_name ).
        ls_webi-veptext = li_vi->get_short_text( sews_c_vif_version-active ).
      CATCH cx_ws_md_exception INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    LOOP AT ls_webi-pvepheader ASSIGNING <ls_vepheader>.
      CLEAR <ls_vepheader>-author.
      CLEAR <ls_vepheader>-createdon.
      CLEAR <ls_vepheader>-changedby.
      CLEAR <ls_vepheader>-changedon.
      CLEAR <ls_vepheader>-ctime.
      CLEAR <ls_vepheader>-text_id.
      CLEAR <ls_vepheader>-utime.
      CLEAR <ls_vepheader>-wsint_version.
    ENDLOOP.

    LOOP AT ls_webi-pvependpoint ASSIGNING <ls_vependpoint>.
      CLEAR <ls_vependpoint>-clustd.
    ENDLOOP.

    LOOP AT ls_webi-pwsheader ASSIGNING <ls_wsheader>.

      CLEAR:
        <ls_wsheader>-author,
        <ls_wsheader>-createdon,
        <ls_wsheader>-changedby,
        <ls_wsheader>-changedon,
        <ls_wsheader>-ctime,
        <ls_wsheader>-utime.

    ENDLOOP.

    io_xml->add( iv_name = 'WEBI'
                 ig_data = ls_webi ).

    zcl_abapgit_sotr_handler=>read_sotr(
      iv_pgmid    = 'R3TR'
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name
      io_xml      = io_xml ).

  ENDMETHOD.
ENDCLASS.
