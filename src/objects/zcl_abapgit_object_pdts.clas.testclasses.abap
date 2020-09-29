CLASS ltd_mock DEFINITION
  FINAL
  CREATE PUBLIC
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    CONSTANTS mc_task_id TYPE hrobjid VALUE '99999999'.

    METHODS create_input_xml RETURNING VALUE(ri_result) TYPE REF TO zif_abapgit_xml_input
                             RAISING   zcx_abapgit_exception.
    METHODS get_input_xml RETURNING VALUE(rv_result) TYPE string.

  PRIVATE SECTION.
    DATA mv_xml TYPE string.

    METHODS generate.
    METHODS add_line IMPORTING iv_string TYPE string.
    METHODS add_pdts_segment.
    METHODS add_container_segment.

ENDCLASS.


CLASS ltd_mock IMPLEMENTATION.

  METHOD add_line.
    mv_xml = mv_xml && iv_string && cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD generate.

    "Todo: Automate GitHub updates, sort out XML discrepancies
    "
    "When pasting updates from GitHub, following changes are needed:
    "UTF-16, remove serializer attribute from abapGit tag, replace task ID with variable
    "
    "<?xml version="1.0" encoding="utf-16"?>| ).
    "<abapGit version="v1.0.0">| ).
    "...
    "Then replace all instances of task ID (e.g. 90000005), such as:
    "     <OBJID>{ mc_task_id }</OBJID>| ).
    "       <INSTID>TS{ mc_task_id }</INSTID>| ).


    add_line( |<?xml version="1.0" encoding="utf-16"?>| ).
    add_line( |<abapGit version="v1.0.0">| ).
    add_line( | <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">| ).
    add_line( |  <asx:values>| ).

    add_pdts_segment( ).
    add_container_segment( ).

    add_line( |  </asx:values>| ).
    add_line( | </asx:abap>| ).
    add_line( |</abapGit>| ).

  ENDMETHOD.


  METHOD add_pdts_segment.

    add_line( |   <PDTS>| ).
    add_line( |    <SHORT_TEXT>abapGitTest</SHORT_TEXT>| ).
    add_line( |    <PLVAR>01</PLVAR>| ).
    add_line( |    <WI_TEXT>abapGit Test &amp;_WI_OBJECT_ID.BUSINESSPARTNER&amp;</WI_TEXT>| ).
    add_line( |    <METHOD>| ).
    add_line( |     <OTYPE>TS</OTYPE>| ).
    add_line( |     <OBJID>{ mc_task_id }</OBJID>| ).
    add_line( |     <SWOTP>BUS1006</SWOTP>| ).
    add_line( |     <SWMTD>DISPLAY</SWMTD>| ).
    add_line( |     <METHTYPE>BO</METHTYPE>| ).
    add_line( |     <SYNCHRON>X</SYNCHRON>| ).
    add_line( |    </METHOD>| ).
    add_line( |    <STARTING_EVENTS>| ).
    add_line( |     <HRSEVENTS>| ).
    add_line( |      <SUBTY>0001</SUBTY>| ).
    add_line( |      <CLSTYP>BO</CLSTYP>| ).
    add_line( |      <OBJTYP>BUS1006</OBJTYP>| ).
    add_line( |      <EVENT>CREATED</EVENT>| ).
    add_line( |     </HRSEVENTS>| ).
    add_line( |    </STARTING_EVENTS>| ).
    add_line( |    <STARTING_EVENTS_BINDING>| ).
    add_line( |     <HRS1212>| ).
    add_line( |      <OTYPE>TS</OTYPE>| ).
    add_line( |      <OBJID>{ mc_task_id }</OBJID>| ).
    add_line( |      <SUBTY>0001</SUBTY>| ).
    add_line( |      <CLSTYP>BO</CLSTYP>| ).
    add_line( |      <OBJTYP>BUS1006</OBJTYP>| ).
    add_line( |      <EVENT>CREATED</EVENT>| ).
    add_line( |      <DATAFLOW>E</DATAFLOW>| ).
    add_line( |      <TAB_INDEX>000001</TAB_INDEX>| ).
    add_line( |      <EXPR>&amp;_EVT_OBJECT&amp;</EXPR>| ).
    add_line( |      <SOURCETYPE>S</SOURCETYPE>| ).
    add_line( |      <TARGETTYPE>S</TARGETTYPE>| ).
    add_line( |      <TARGETEXPR>&amp;_WI_OBJECT_ID&amp;</TARGETEXPR>| ).
    add_line( |      <OPERATION>ASN</OPERATION>| ).
    add_line( |      <OPCLASS>E</OPCLASS>| ).
    add_line( |     </HRS1212>| ).
    add_line( |    </STARTING_EVENTS_BINDING>| ).
    add_line( |    <TERMINATING_EVENTS>| ).
    add_line( |     <HRSEVTERM>| ).
    add_line( |      <SUBTY>0002</SUBTY>| ).
    add_line( |      <CLSTYP>BO</CLSTYP>| ).
    add_line( |      <OBJTYP>BUS1006</OBJTYP>| ).
    add_line( |      <EVENT>CHANGED</EVENT>| ).
    add_line( |      <EV_ELEMENT>_WI_OBJECT_ID</EV_ELEMENT>| ).
    add_line( |     </HRSEVTERM>| ).
    add_line( |    </TERMINATING_EVENTS>| ).
    add_line( |    <TERMINATING_EVENTS_BINDING>| ).
    add_line( |     <HRS1212>| ).
    add_line( |      <OTYPE>TS</OTYPE>| ).
    add_line( |      <OBJID>{ mc_task_id }</OBJID>| ).
    add_line( |      <SUBTY>0002</SUBTY>| ).
    add_line( |      <CLSTYP>BO</CLSTYP>| ).
    add_line( |      <OBJTYP>BUS1006</OBJTYP>| ).
    add_line( |      <EVENT>CHANGED</EVENT>| ).
    add_line( |      <DATAFLOW>E</DATAFLOW>| ).
    add_line( |      <TAB_INDEX>000001</TAB_INDEX>| ).
    add_line( |      <EV_ELEMENT>_WI_OBJECT_ID</EV_ELEMENT>| ).
    add_line( |      <EXPR>&amp;_EVT_OBJECT&amp;</EXPR>| ).
    add_line( |      <SOURCETYPE>S</SOURCETYPE>| ).
    add_line( |      <TARGETTYPE>S</TARGETTYPE>| ).
    add_line( |      <TARGETEXPR>&amp;_WI_OBJECT_ID&amp;</TARGETEXPR>| ).
    add_line( |      <OPERATION>ASN</OPERATION>| ).
    add_line( |      <OPCLASS>E</OPCLASS>| ).
    add_line( |     </HRS1212>| ).
    add_line( |    </TERMINATING_EVENTS_BINDING>| ).
    add_line( |   </PDTS>| ).

  ENDMETHOD.


  METHOD add_container_segment.

    add_line( |   <CONTAINER>| ).
    add_line( |    <CONTAINER>| ).
    add_line( |     <PROPERTIES>| ).
    add_line( |      <OWN_ID>| ).
    add_line( |       <INSTID>TS{ mc_task_id }</INSTID>| ).
    add_line( |       <TYPEID>CL_SWF_CNT_HRS_PERSISTENCE</TYPEID>| ).
    add_line( |       <CATID>CL</CATID>| ).
    add_line( |      </OWN_ID>| ).
    add_line( |      <PROPSTRING>23000000000008</PROPSTRING>| ).
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
    add_line( |      <M NAME="NEWINTERNALMODE:NewInternalMode:" TYPE="::BAPIBUS1006_HEAD-NEWINTERNALMODE:C:1:0" PROPS="0C003301"/>| ).
    add_line( |      <N NAME="FOO:Foo:" TYPE="::CHAR10:h:10:0" PROPS="0C005241"/>| ).
    add_line( |     </ELEMENTS>| ).
    add_line( |    </CONTAINER>| ).
    add_line( |   </CONTAINER>| ).

  ENDMETHOD.


  METHOD create_input_xml.
    generate( ).
    CREATE OBJECT ri_result TYPE zcl_abapgit_xml_input
      EXPORTING
        iv_xml = mv_xml.
  ENDMETHOD.


  METHOD get_input_xml.
    rv_result = me->mv_xml.
  ENDMETHOD.

ENDCLASS.


CLASS ltc_turnaround_test DEFINITION FINAL FOR TESTING
  DURATION LONG
  RISK LEVEL CRITICAL.

  PRIVATE SECTION.

    DATA mo_mock TYPE REF TO ltd_mock.
    DATA mi_output_xml TYPE REF TO zif_abapgit_xml_output.
    DATA mo_cut    TYPE REF TO zif_abapgit_object.

    CLASS-METHODS class_setup.
    CLASS-METHODS task_exists RETURNING VALUE(rv_result) TYPE abap_bool.
    CLASS-METHODS check_critical_tests_enabled.
    CLASS-METHODS check_task_does_not_exist.

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
    check_critical_tests_enabled( ).
    check_task_does_not_exist( ).
  ENDMETHOD.


  METHOD check_critical_tests_enabled.

    "Objects will be created and deleted, do not run in customer system!
    "These tests may fail if you are locking the entries (e.g. the ZABAPGIT transaction is open)
    IF zcl_abapgit_persist_settings=>get_instance( )->read( )->get_run_critical_tests( ) = abap_false.
      cl_abap_unit_assert=>fail(
        msg   = 'Cancelled. You can enable these tests at the Settings page'
        level = if_aunit_constants=>tolerable ).
    ENDIF.

  ENDMETHOD.


  METHOD check_task_does_not_exist.
    IF task_exists( ) = abap_true.
      cl_abap_unit_assert=>fail( msg   = |Test task { ltd_mock=>mc_task_id } already exists|
                                 level = if_aunit_constants=>fatal
                                 quit  = if_aunit_constants=>class ).
    ENDIF.
  ENDMETHOD.


  METHOD setup.
    DATA  ls_item   TYPE zif_abapgit_definitions=>ty_item.

    ls_item-obj_type = 'PDTS'.
    ls_item-obj_name = ltd_mock=>mc_task_id.

    TRY.
        CREATE OBJECT mo_cut TYPE zcl_abapgit_object_pdts
          EXPORTING
            is_item     = ls_item
            iv_language = sy-langu.
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

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
          li_log  TYPE REF TO zif_abapgit_log.              "#EC NEEDED

    lo_input_xml = mo_mock->create_input_xml( ).

    mo_cut->deserialize( iv_package = '$TMP'
                         io_xml     = lo_input_xml
                         iv_step    = lv_step
                         ii_log     = li_log  ).

    cl_abap_unit_assert=>assert_true( task_exists( ) ).
    cl_abap_unit_assert=>assert_true( mo_cut->exists( ) ).

  ENDMETHOD.


  METHOD serialize_task.

    CREATE OBJECT mi_output_xml TYPE zcl_abapgit_xml_output.
    mo_cut->serialize( io_xml = mi_output_xml ).
    rv_result = mi_output_xml->render( ).

  ENDMETHOD.


  METHOD delete_task.
    mo_cut->delete( iv_package = '$TMP' ).
    cl_abap_unit_assert=>assert_false( task_exists( ) ).
  ENDMETHOD.


  METHOD task_exists.

    DATA lv_dummy TYPE hr_sobjid.

    SELECT SINGLE objid
           INTO lv_dummy
           FROM hrs1000
           WHERE otype = 'TS' AND
                 objid = ltd_mock=>mc_task_id  ##WARN_OK.

    IF sy-subrc = 0.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


CLASS ltc_lock DEFINITION
  FINAL
  FOR TESTING
  DURATION MEDIUM
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    CONSTANTS c_ts TYPE hr_sotype VALUE 'TS'.

    METHODS enqueue_is_detected FOR TESTING RAISING cx_static_check.
    METHODS get_any_task RETURNING VALUE(rv_taskid) TYPE hrobjid.
    METHODS lock_task IMPORTING iv_taskid TYPE hrobjid.

ENDCLASS.


CLASS ltc_lock IMPLEMENTATION.

  METHOD enqueue_is_detected.

    DATA: lv_taskid TYPE hrobjid,
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
           INTO rv_taskid
           FROM hrs1000
           WHERE otype = c_ts  ##WARN_OK. "#EC CI_NOORDER #EC CI_SGLSELECT

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

    TRY.
        CREATE OBJECT mo_cut TYPE zcl_abapgit_object_pdts
          EXPORTING
            is_item     = ls_item
            iv_language = sy-langu.
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

  METHOD run_simple_methods.
    mo_cut->get_comparator( ).
    mo_cut->get_deserialize_steps( ).
    mo_cut->get_metadata( ).
    mo_cut->is_active( ).
  ENDMETHOD.

ENDCLASS.
