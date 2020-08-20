
CLASS lth_input_xml DEFINITION
  INHERITING FROM zcl_abapgit_xml_input
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  "Using a subclass to add some of the output class's functions without affecting product code for now.

  PUBLIC SECTION.
    METHODS render IMPORTING iv_normalize  TYPE sap_bool DEFAULT abap_true
                             is_metadata   TYPE zif_abapgit_definitions=>ty_metadata OPTIONAL
                   RETURNING VALUE(rv_xml) TYPE string.

  PRIVATE SECTION.
    METHODS build_asx_node
      RETURNING
        VALUE(ri_element) TYPE REF TO if_ixml_element .

ENDCLASS.


CLASS lth_input_xml IMPLEMENTATION.

  METHOD render.

    "Copied from zcl_abapgit_xml_output

    DATA: li_git  TYPE REF TO if_ixml_element,
          li_abap TYPE REF TO if_ixml_element.

    li_abap ?= mi_xml_doc->get_root( )->get_first_child( ).
    mi_xml_doc->get_root( )->remove_child( li_abap ).
    IF li_abap IS INITIAL.
      li_abap = build_asx_node( ).
    ENDIF.

    li_git = mi_xml_doc->create_element( c_abapgit_tag ).
    li_git->set_attribute( name = c_attr_version
                           value = zif_abapgit_version=>gc_xml_version ).
    IF NOT is_metadata IS INITIAL.
      li_git->set_attribute( name  = c_attr_serializer
                             value = is_metadata-class ).
      li_git->set_attribute( name  = c_attr_serializer_version
                             value = is_metadata-version ).
    ENDIF.
    li_git->append_child( li_abap ).
    mi_xml_doc->get_root( )->append_child( li_git ).

    rv_xml = to_xml( iv_normalize ).

  ENDMETHOD.

  METHOD build_asx_node.

    DATA: li_attr TYPE REF TO if_ixml_attribute.


    ri_element = mi_xml_doc->create_element_ns(
      name   = 'abap'
      prefix = 'asx' ).

    li_attr = mi_xml_doc->create_attribute_ns( 'version' ).
    li_attr->if_ixml_node~set_value( '1.0' ).
    ri_element->set_attribute_node_ns( li_attr ).

    li_attr = mi_xml_doc->create_attribute_ns(
      name   = 'asx'
      prefix = 'xmlns' ).
    li_attr->if_ixml_node~set_value( 'http://www.sap.com/abapxml' ).
    ri_element->set_attribute_node_ns( li_attr ).

  ENDMETHOD.

ENDCLASS.


CLASS ltd_mock DEFINITION
  CREATE PUBLIC
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    CONSTANTS mc_task_id TYPE hrobjid VALUE '99999999'.

    METHODS create_input_xml RETURNING VALUE(ri_result) TYPE REF TO lth_input_xml  "zif_abapgit_xml_input
                             RAISING   zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_xml TYPE REF TO  zcl_abapgit_xml_input.
    DATA mv_xml TYPE string.

    METHODS generate.
    METHODS add_line IMPORTING iv_string TYPE string.
    METHODS verify_deserialization FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltd_mock IMPLEMENTATION.

  METHOD add_line.
    mv_xml = mv_xml && iv_string && cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD generate.
    add_line( |<?xml version="1.0" encoding="utf-8"?>| ).
    add_line( |<abapGit version="v1.0.0">| ).
    add_line( | <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">| ).
    add_line( |  <asx:values>| ).
    add_line( |   <PDTS>| ).
    add_line( |    <SHORT_TEXT>abapGitTest</SHORT_TEXT>| ).
    add_line( |    <PLVAR>01</PLVAR>| ).
    add_line( |    <WI_TEXT>abapGit Test</WI_TEXT>| ).
    add_line( |    <METHOD>| ).
    add_line( |     <OTYPE>TS</OTYPE>| ).
    add_line( |     <OBJID>{ mc_task_id }</OBJID>| ).
    add_line( |     <SWOTP>WFTS</SWOTP>| ).
    add_line( |     <SWMTD>EMPTYDIALOGMETHOD</SWMTD>| ).
    add_line( |     <METHTYPE>BO</METHTYPE>| ).
    add_line( |     <SYNCHRON>X</SYNCHRON>| ).
    add_line( |    </METHOD>| ).
    add_line( |   </PDTS>| ).
    add_line( |   <CONTAINER>| ).
    add_line( |    <CONTAINER>| ).
    add_line( |     <PROPERTIES>| ).
    add_line( |      <OWN_ID>| ).
    add_line( |       <INSTID>TS{ mc_task_id }</INSTID>| ).
    add_line( |       <TYPEID>CL_SWF_CNT_HRS_PERSISTENCE</TYPEID>| ).
    add_line( |       <CATID>CL</CATID>| ).
    add_line( |      </OWN_ID>| ).
    add_line( |      <PROPSTRING>23</PROPSTRING>| ).
    add_line( |      <XMLVERSION>0002</XMLVERSION>| ).
    add_line( |      <INTERNAL>X</INTERNAL>| ).
    add_line( |      <EXDEFINITN>| ).
    add_line( |       <NAME>_DEF_EXT</NAME>| ).
    add_line( |       <POR>| ).
    add_line( |        <INSTID>DIALOG_STEP_CONTAINER</INSTID>| ).
    add_line( |        <TYPEID>CL_SWF_CNT_PERSISTENCE_DEF_EXT</TYPEID>| ).
    add_line( |        <CATID>CL</CATID>| ).
    add_line( |       </POR>| ).
    add_line( |      </EXDEFINITN>| ).
    add_line( |     </PROPERTIES>| ).
    add_line( |     <ELEMENTS>| ).
    add_line( |      <A NAME="_ADHOC_OBJECTS:_Adhoc_Objects:" TYPE=":BO::h:0:0" PROPS="0C925A51" LTEXTS="EE014Ad Hoc ObjectsAd Hoc Objects of Workflow Instance"/>| ).
    add_line( |      <B NAME="_ATTACH_OBJECTS:_Attach_Objects:" TYPE="SOFM:BO::h:0:0" PROPS="0C925A51" LTEXTS="EE011AttachmentsAttachments of Workflow Instance"/>| ).
    add_line( |      <C NAME="_WI_ACTUAL_AGENT:_Wi_Actual_Agent:" TYPE="::WFSYST-AGENT:C:0:0" PROPS="0C002A11" LTEXTS="EE005AgentActual Agent of Workflow Activity"/>| ).
    add_line( |      <D NAME="_WI_GROUP_ID:_Wi_Group_ID:" TYPE=":BO::u:0:0" PROPS="0C921A11" LTEXTS="EE017Grouping Charact.Grouping Characteristic for Workflow Instances"/>| ).
    add_line( |      <E NAME="_WORKITEM:_Workitem:" TYPE="WORKINGWI:BO:::0:0" PROPS="0C921A11" LTEXTS="EE004StepStep Instance"/>| ).
    add_line( |      <F NAME="_WF_TICKET:_Wf_Ticket:" TYPE="::SWWTTICKET:h:0:0" PROPS="0C000271" LTEXTS="EE018Transaction TicketTransaction Ticket"/>| ).
    add_line( |      <G NAME="_RULE_RESULT:_Rule_Result:" TYPE="AAGENT:BO::u:0:0" PROPS="0C921A11" LTEXTS="EE006AgentsResult of Agent Determination"/>| ).
    add_line( |      <H NAME="_RFC_DESTINATION:_Rfc_Destination:" TYPE="::RFCDEST:C:0:0" PROPS="0C001231" LTEXTS="EE015RFC DestinationRFC Destination"/>| ).
    add_line( |      <I NAME="_ATTACH_COMMENT_OBJECTS:_Attach_Comment_Objects:" TYPE="SOFM:BO::h:0:0" PROPS="0C925A71" LTEXTS="EE007CommentComment"/>| ).
    add_line( |      <J NAME="_METHOD_OBJECTS:_Method_Objects:" TYPE=":BO::h:0:0" PROPS="0C925A51" LTEXTS="EE020Second.Method ObjectSecondary Method Objects of Workflow Activity"/>| ).
    add_line( |      <K NAME="_START_EVENT_IDENTIFIER:_Start_Event_Identifier:" TYPE="CL_SWF_UTL_EVT_IDENTIFIER:CL::h:0:0" PROPS="0CC20231" LTEXTS="EE017ID of Start EventID of Start Event"/>| ).
    add_line( |      <L NAME="_WF_TYPENAME_MAPPING:_WF_Typename_Mapping:" TYPE="::SWF_CNT_MAPPING_TAB:h:0:0" PROPS="0C120271" LTEXTS="EE022Relation of Type NamesRelation of Type Names (Original and Copy)"/>| ).
    add_line( |      <M NAME="ACTUALLYPROCBY:ActuallyProcBy:" TYPE="::WFSYST-ACT_AGENT:C:14:0" PROPS="0C003301"/>| ).
    add_line( |      <N NAME="_WI_OBJECT_ID:_Wi_Object_ID:" TYPE="WFTS:BO:::0:0" PROPS="0C921A11"/>| ).
    add_line( |     </ELEMENTS>| ).
    add_line( |    </CONTAINER>| ).
    add_line( |   </CONTAINER>| ).
    add_line( |  </asx:values>| ).
    add_line( | </asx:abap>| ).
    add_line( |</abapGit>| ).
  ENDMETHOD.

  METHOD create_input_xml.
    generate( ).
    CREATE OBJECT ri_result  " TYPE zcl_abapgit_xml_input
      EXPORTING
        iv_xml = mv_xml.
  ENDMETHOD.

  METHOD verify_deserialization.

    DATA: lo_xml      TYPE REF TO lth_input_xml,
          lv_rendered TYPE string.

    lo_xml = create_input_xml( ).
    lv_rendered = lo_xml->render( ).
    cl_abap_unit_assert=>assert_equals( act = lv_rendered
                                        exp = mv_xml ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_turnaround_test DEFINITION FINAL FOR TESTING
  DURATION LONG
  RISK LEVEL CRITICAL.

  PRIVATE SECTION.

    DATA mo_mock TYPE REF TO ltd_mock.
    DATA li_output_xml TYPE REF TO zcl_abapgit_xml_output.

    CLASS-METHODS class_setup.
    METHODS setup.

    METHODS create_task FOR TESTING RAISING cx_static_check.

ENDCLASS.



CLASS ltc_turnaround_test IMPLEMENTATION.

  METHOD class_setup.

    DATA lv_dummy TYPE abap_bool.

    SELECT SINGLE @abap_true
           FROM hrs1000
           WHERE otype = 'TS' AND
                 objid = @ltd_mock=>mc_task_id
           INTO @lv_dummy.

    cl_abap_unit_assert=>assert_subrc(
        exp              = 4
        act              = sy-subrc
        msg              = |Test task { ltd_mock=>mc_task_id } already exists|
        level            = if_aunit_constants=>fatal
        quit             = if_aunit_constants=>class ).

  ENDMETHOD.

  METHOD setup.

    CREATE OBJECT mo_mock.

  ENDMETHOD.

  METHOD create_task.

    DATA lo_input_xml TYPE REF TO lth_input_xml.

    DATA: lv_taskid TYPE hrobjid,
          lv_task   TYPE hrsobject,
          lo_cut    TYPE REF TO zif_abapgit_object,
          lo_cut2   TYPE REF TO zif_abapgit_object,
          ls_item   TYPE zif_abapgit_definitions=>ty_item,
          lv_step   TYPE zif_abapgit_definitions=>ty_deserialization_step,
          li_log    TYPE REF TO zif_abapgit_log.


    ls_item-obj_type = 'PDTS'.
    ls_item-obj_name = ltd_mock=>mc_task_id.

    CREATE OBJECT lo_cut TYPE zcl_abapgit_object_pdts
      EXPORTING
        is_item     = ls_item
        iv_language = sy-langu.

    lo_input_xml = mo_mock->create_input_xml( ).

    lo_cut->deserialize(
      EXPORTING
        iv_package = '$PDTS'
        io_xml     = CAST #( lo_input_xml )
        iv_step    = lv_step
        ii_log     = li_log ).

    CREATE OBJECT li_output_xml TYPE zcl_abapgit_xml_output.

    CREATE OBJECT lo_cut2 TYPE zcl_abapgit_object_pdts
      EXPORTING
        is_item     = ls_item
        iv_language = sy-langu.

    lo_cut2->serialize( io_xml = li_output_xml ).

    DATA(lv_input) = lo_input_xml->render( ).
    DATA(lv_output) = li_output_xml->render( ).

    cl_abap_unit_assert=>assert_equals(
                             act = lv_input
                             exp = lv_output ).

  ENDMETHOD.

ENDCLASS.

CLASS ltc_lock DEFINITION FINAL FOR TESTING
  DURATION MEDIUM
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS c_ts TYPE hr_sotype VALUE 'TS' ##NO_TEXT.
    METHODS:
      enqueue_is_detected FOR TESTING RAISING cx_static_check,
      get_any_customer_task
        RETURNING
          VALUE(rv_taskid) TYPE hrobjid,
      lock_task
        IMPORTING
          iv_taskid TYPE hrobjid.
ENDCLASS.


CLASS ltc_lock IMPLEMENTATION.

  METHOD enqueue_is_detected.

    DATA: lv_taskid TYPE hrobjid,
          lv_task   TYPE hrsobject,
          lo_cut    TYPE REF TO zif_abapgit_object,
          ls_item   TYPE zif_abapgit_definitions=>ty_item.

    lv_taskid = get_any_customer_task( ).
    lock_task( lv_taskid ).

    ls_item-obj_type = 'PDTS'.
    ls_item-obj_name = 'TS' && lv_taskid.

    CREATE OBJECT lo_cut TYPE zcl_abapgit_object_pdts
      EXPORTING
        is_item     = ls_item
        iv_language = sy-langu.

    cl_abap_unit_assert=>assert_true( lo_cut->is_locked( ) ).

    CALL FUNCTION 'DEQUEUE_HRSOBJECT'
      EXPORTING
        objid   = lv_taskid
        otype   = c_ts
        x_objid = ' '
        x_otype = ' '
        _scope  = '2'.

  ENDMETHOD.


  METHOD get_any_customer_task.

    SELECT SINGLE objid
           FROM hrs1000
           WHERE otype = @c_ts AND
                 objid LIKE '9%'
           INTO @rv_taskid.

    "A customer task must exist, else we assume WF customizing hasn't been done yet
    cl_abap_unit_assert=>assert_subrc( exp = 0
                                       act = sy-subrc ).

  ENDMETHOD.


  METHOD lock_task.

    CALL FUNCTION 'ENQUEUE_HRSOBJECT'
      EXPORTING
        objid          = iv_taskid
        otype          = c_ts
        x_objid        = ' '
        x_otype        = ' '
        _scope         = '2'
        _wait          = ' '
      EXCEPTIONS
        foreign_lock   = 01
        system_failure = 02.

    cl_abap_unit_assert=>assert_subrc( exp = 0
                                       act = sy-subrc ).

  ENDMETHOD.

ENDCLASS.
