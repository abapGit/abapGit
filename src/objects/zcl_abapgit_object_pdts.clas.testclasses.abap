CLASS ltd_mock DEFINITION
  CREATE PUBLIC
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    CONSTANTS mc_task_id TYPE hrobjid VALUE '99999999'.

    METHODS create_input_xml RETURNING VALUE(ri_result) TYPE REF TO zif_abapgit_xml_input
                             RAISING   zcx_abapgit_exception.
    METHODS get_input_xml RETURNING VALUE(r_result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_xml TYPE REF TO  zcl_abapgit_xml_input.
    DATA mv_xml TYPE string.

    METHODS generate.
    METHODS add_line IMPORTING iv_string TYPE string.

ENDCLASS.


CLASS ltd_mock IMPLEMENTATION.

  METHOD add_line.
    mv_xml = mv_xml && iv_string && cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD generate.

    "Note: When pasting updates from GitHub, change to UTF-16 and remove serializer attribute from abapGit tag

    add_line( |<?xml version="1.0" encoding="utf-16"?>| ).
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
    CREATE OBJECT ri_result TYPE zcl_abapgit_xml_input
      EXPORTING
        iv_xml = mv_xml.
  ENDMETHOD.

  METHOD get_input_xml.
    r_result = me->mv_xml.
  ENDMETHOD.

ENDCLASS.


CLASS ltc_turnaround_test DEFINITION FINAL FOR TESTING
  DURATION LONG
  RISK LEVEL CRITICAL.

  PRIVATE SECTION.

    DATA mo_mock TYPE REF TO ltd_mock.
    DATA li_output_xml TYPE REF TO zcl_abapgit_xml_output.
    DATA: mo_cut    TYPE REF TO zif_abapgit_object.

    CLASS-METHODS class_setup.
    CLASS-METHODS task_exists RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS setup.

    METHODS create_task RAISING zcx_abapgit_exception.
    METHODS serialize_task RETURNING VALUE(rv_result) TYPE string
                           RAISING   zcx_abapgit_exception.
    METHODS clean_xml IMPORTING iv_xml           TYPE string
                      RETURNING VALUE(rv_result) TYPE string.
    METHODS delete_task
      RAISING
        zcx_abapgit_exception.

    METHODS output_matches_input FOR TESTING RAISING cx_static_check.

ENDCLASS.



CLASS ltc_turnaround_test IMPLEMENTATION.


  METHOD class_setup.

    IF task_exists( ).
      cl_abap_unit_assert=>fail( msg   = |Test task { ltd_mock=>mc_task_id } already exists|
                                 level = if_aunit_constants=>fatal
                                 quit  = if_aunit_constants=>class ).
    ENDIF.

  ENDMETHOD.


  METHOD setup.
    DATA  ls_item   TYPE zif_abapgit_definitions=>ty_item.

    ls_item-obj_type = 'PDTS'.
    ls_item-obj_name = ltd_mock=>mc_task_id.

    CREATE OBJECT mo_cut TYPE zcl_abapgit_object_pdts
      EXPORTING
        is_item     = ls_item
        iv_language = sy-langu.

    CREATE OBJECT mo_mock.

  ENDMETHOD.


  METHOD output_matches_input.

    DATA lv_output TYPE string.

    create_task( ).
    cl_abap_unit_assert=>assert_equals( act = mo_cut->changed_by( )
                                        exp = sy-uname ).
    lv_output = serialize_task( ).
    lv_output = clean_xml( lv_output ).

    cl_abap_unit_assert=>assert_equals( act = lv_output
                                        exp = mo_mock->get_input_xml( ) ).
    delete_task( ).

  ENDMETHOD.


  METHOD clean_xml.
    rv_result = substring_from( val = iv_xml
                                sub = '<' ).
  ENDMETHOD.


  METHOD create_task.

    DATA lo_input_xml TYPE REF TO zif_abapgit_xml_input.

    DATA: lv_step TYPE zif_abapgit_definitions=>ty_deserialization_step,
          li_log  TYPE REF TO zif_abapgit_log.

    lo_input_xml = mo_mock->create_input_xml( ).

    mo_cut->deserialize(
      EXPORTING
        iv_package = '$TMP'
        io_xml     = CAST #( lo_input_xml )
        iv_step    = lv_step
        ii_log     = li_log ).

    cl_abap_unit_assert=>assert_true( task_exists( ) ).
    cl_abap_unit_assert=>assert_true( mo_cut->exists( ) ).

  ENDMETHOD.


  METHOD serialize_task.

    CREATE OBJECT li_output_xml TYPE zcl_abapgit_xml_output.
    mo_cut->serialize( io_xml = li_output_xml ).
    rv_result = li_output_xml->render( ).

  ENDMETHOD.


  METHOD delete_task.
    mo_cut->delete( iv_package = '$TMP' ).
    cl_abap_unit_assert=>assert_false( task_exists( ) ).
  ENDMETHOD.


  METHOD task_exists.

    SELECT SINGLE @abap_true
           FROM hrs1000
           WHERE otype = 'TS' AND
                 objid = @ltd_mock=>mc_task_id
           INTO @rv_result.

  ENDMETHOD.

ENDCLASS.


CLASS ltc_lock DEFINITION
  FINAL
  FOR TESTING
  DURATION MEDIUM
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    CONSTANTS c_ts TYPE hr_sotype VALUE 'TS' ##NO_TEXT.

    METHODS enqueue_is_detected FOR TESTING RAISING cx_static_check.
    METHODS get_any_task RETURNING VALUE(rv_taskid) TYPE hrobjid.
    METHODS lock_task IMPORTING iv_taskid TYPE hrobjid.

ENDCLASS.


CLASS ltc_lock IMPLEMENTATION.

  METHOD enqueue_is_detected.

    DATA: lv_taskid TYPE hrobjid,
          lv_task   TYPE hrsobject,
          lo_cut    TYPE REF TO zif_abapgit_object,
          ls_item   TYPE zif_abapgit_definitions=>ty_item.

    lv_taskid = get_any_task( ).
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


  METHOD get_any_task.

    SELECT SINGLE objid
           FROM hrs1000
           WHERE otype = @c_ts
           INTO @rv_taskid.

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

CLASS ltc_smoke_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA mo_cut TYPE REF TO zif_abapgit_object.

    METHODS setup.
    METHODS run_simple_methods FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_smoke_test IMPLEMENTATION.

  METHOD setup.
    DATA  ls_item   TYPE zif_abapgit_definitions=>ty_item.

    ls_item-obj_type = 'PDTS'.
    ls_item-obj_name = ltd_mock=>mc_task_id.

    CREATE OBJECT mo_cut TYPE zcl_abapgit_object_pdts
      EXPORTING
        is_item     = ls_item
        iv_language = sy-langu.

  ENDMETHOD.

  METHOD run_simple_methods.
    mo_cut->get_comparator( ).
    mo_cut->get_deserialize_steps( ).
    mo_cut->get_metadata( ).
    mo_cut->is_active( ).
  ENDMETHOD.

ENDCLASS.
