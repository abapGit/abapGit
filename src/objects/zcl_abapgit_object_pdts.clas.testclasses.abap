CLASS ltcl_mock DEFINITION
  CREATE PUBLIC
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    CONSTANTS mc_task_id TYPE hrobjid VALUE '99999999'.

    METHODS create_input_xml RETURNING VALUE(ri_result) TYPE REF TO zif_abapgit_xml_input
                             RAISING   zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_xml TYPE REF TO  zcl_abapgit_xml_input.
    DATA mv_xml TYPE string.

    METHODS generate.
    METHODS add_line IMPORTING iv_string TYPE string.

ENDCLASS.


CLASS ltcl_mock IMPLEMENTATION.

  METHOD add_line.
    mv_xml = mv_xml && iv_string && cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD generate.
    add_line( |<?xml version="1.0" encoding="utf-8"?>| ).
    add_line( |<abapGit version="v1.0.0" serializer="LCL_OBJECT_PDTS" serializer_version="v1.0.0">| ).
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
    ri_result = NEW zcl_abapgit_xml_input( mv_xml ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_turnaround_test DEFINITION FINAL FOR TESTING
  DURATION LONG
  RISK LEVEL CRITICAL.

  PRIVATE SECTION.

    DATA mo_mock TYPE REF TO ltcl_mock.

    METHODS setup.
    "Use unofficial naming hack to run this test first
    METHODS _task_must_not_exist FOR TESTING RAISING cx_static_check.
    METHODS create_task FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_turnaround_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_mock.
  ENDMETHOD.

  METHOD _task_must_not_exist.

    SELECT SINGLE @abap_true
        FROM hrs1000
        WHERE otype = 'TS' AND
              objid = @mo_mock->mc_task_id
        INTO @DATA(dummy).

    cl_abap_unit_assert=>assert_subrc(
        exp              = 4
        act              = sy-subrc
        msg              = |Test task { mo_mock->mc_task_id } already exists|
        level            = if_aunit_constants=>fatal
        quit             = if_aunit_constants=>class ).

  ENDMETHOD.

  METHOD create_task.
    DATA li_xml TYPE REF TO zif_abapgit_xml_input.
    li_xml = mo_mock->create_input_xml( ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_lock DEFINITION FINAL FOR TESTING
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


CLASS ltcl_lock IMPLEMENTATION.

  METHOD enqueue_is_detected.

    DATA: lv_taskid TYPE hrobjid,
          lv_task   TYPE hrsobject,
          lo_cut    TYPE REF TO zif_abapgit_object.

    lv_taskid = get_any_customer_task( ).
    lock_task( lv_taskid ).

    lo_cut = NEW zcl_abapgit_object_pdts(
      is_item     = VALUE #( obj_type = 'PDTS'
                             obj_name = 'TS' && lv_taskid )
      iv_language = sy-langu ).

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
