*----------------------------------------------------------------------*
*       CLASS lc_Zsaplink_Test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lc_zsaplink_test DEFINITION FOR TESTING
  " DURATION SHORT
  " RISK LEVEL HARMLESS
  "#AU Duration Medium
  "#AU Risk_Level Harmless
.
*?<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>lc_Zsaplink_Test
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZSAPLINK
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
* ================
    DATA:
      f_cut TYPE REF TO zsaplink.  "class under test

    METHODS: convertixmldoctostring FOR TESTING.
ENDCLASS.       "lc_Zsaplink_Test


*----------------------------------------------------------------------*
*       CLASS lc_Zsaplink_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lc_zsaplink_test IMPLEMENTATION.
* ======================================

  METHOD convertixmldoctostring.
    CONSTANTS value TYPE string VALUE 'German - Umlaute: öäü ÖÄÜ ß, Special characters - slowenian: ##### #####'.
    DATA ixml     TYPE REF TO if_ixml.
    DATA ixmldoc  TYPE REF TO if_ixml_document.
    DATA rootnode TYPE REF TO if_ixml_element.
    DATA xmlstring TYPE string.
    DATA value_from_xml TYPE string.

    " Instanciate ixml
    ixml = cl_ixml=>create( ).
    ixmldoc = ixml->create_document( ).
    " Add content
    rootnode = ixmldoc->create_element( 'test' ).
    rootnode->set_value( value ).
    ixmldoc->append_child( rootNode ).

    " Convert to String
    xmlstring = zsaplink=>convertixmldoctostring( ixmldoc ).

    " Convert back
    FREE: ixmldoc, rootnode.
    ixmldoc = zsaplink=>convertstringtoixmldoc( xmlstring ).

    rootnode = ixmldoc->get_root_element( ).
    value_from_xml = rootnode->get_value( ).
    cl_aunit_assert=>assert_equals(
      EXPORTING
        exp                  = value          " Data Object with Expected Type
        act                  = value_from_xml " Data Object with Current Value
        msg                  = 'XML Input and Output not equal'    " Message in Case of Error
    ).
  ENDMETHOD.       "convertixmldoctostring




ENDCLASS.       "lc_Zsaplink_Test