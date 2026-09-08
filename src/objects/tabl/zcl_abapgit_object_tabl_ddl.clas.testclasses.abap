CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_abapgit_object_tabl_ddl DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS test
      IMPORTING
        iv_ddl TYPE string
        iv_xml TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS escape_string FOR TESTING RAISING cx_static_check.
    METHODS test1 FOR TESTING RAISING cx_static_check.
    METHODS test2 FOR TESTING RAISING cx_static_check.
    METHODS test_includes_and_value_help FOR TESTING RAISING cx_static_check.
    METHODS invalid_ddl FOR TESTING RAISING cx_static_check.
    METHODS annotations_and_types FOR TESTING RAISING cx_static_check.
    METHODS builtin_types FOR TESTING RAISING cx_static_check.
    METHODS foreign_key_cardinalities FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD escape_string.

    DATA lv_text   TYPE c LENGTH 20.
    DATA lo_cut    TYPE REF TO zcl_abapgit_object_tabl_ddl.
    DATA lv_result TYPE string.

    lv_text = |hello ' world|.
    CREATE OBJECT lo_cut.
    lv_result = lo_cut->escape_string( lv_text ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = |'hello '' world'| ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->unescape_string( lv_result )
      exp = lv_text ).

  ENDMETHOD.

  METHOD test.

    DATA lo_format       TYPE REF TO zcl_abapgit_object_tabl_ddl.
    DATA ls_data         TYPE zif_abapgit_object_tabl=>ty_internal.
    DATA ls_deserialized TYPE zif_abapgit_object_tabl=>ty_internal.
    DATA ls_expected_dd03p LIKE LINE OF ls_data-dd03p.
    DATA ls_actual_dd03p   LIKE LINE OF ls_data-dd03p.
    DATA ls_expected_dd08v LIKE LINE OF ls_data-dd08v.
    DATA ls_actual_dd08v   LIKE LINE OF ls_data-dd08v.
    DATA ls_expected_dd05m LIKE LINE OF ls_data-dd05m.
    DATA ls_actual_dd05m   LIKE LINE OF ls_data-dd05m.
    DATA lv_ddl          TYPE string.
    DATA lv_roundtrip    TYPE string.


    CREATE OBJECT lo_format.

    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML iv_xml
      RESULT
      dd02v       = ls_data-dd02v
      dd03p_table = ls_data-dd03p
      dd05m_table = ls_data-dd05m
      dd08v_table = ls_data-dd08v.

    lv_ddl = lo_format->serialize( ls_data ).

    cl_abap_unit_assert=>assert_equals(
      exp = iv_ddl
      act = lv_ddl ).

    ls_deserialized = lo_format->deserialize( lv_ddl ).
    lv_roundtrip = lo_format->serialize( ls_deserialized ).
    cl_abap_unit_assert=>assert_equals(
      exp = iv_ddl
      act = lv_roundtrip ).

    cl_abap_unit_assert=>assert_equals(
      exp = ls_data-dd02v-tabname
      act = ls_deserialized-dd02v-tabname ).
    cl_abap_unit_assert=>assert_equals(
      exp = ls_data-dd02v-ddtext
      act = ls_deserialized-dd02v-ddtext ).
    cl_abap_unit_assert=>assert_equals(
      exp = ls_data-dd02v-exclass
      act = ls_deserialized-dd02v-exclass ).
    cl_abap_unit_assert=>assert_equals(
      exp = ls_data-dd02v-tabclass
      act = ls_deserialized-dd02v-tabclass ).
    cl_abap_unit_assert=>assert_equals(
      exp = ls_data-dd02v-contflag
      act = ls_deserialized-dd02v-contflag ).
    cl_abap_unit_assert=>assert_equals(
      exp = ls_data-dd02v-mainflag
      act = ls_deserialized-dd02v-mainflag ).

    cl_abap_unit_assert=>assert_equals(
      exp = lines( ls_data-dd03p )
      act = lines( ls_deserialized-dd03p ) ).
    LOOP AT ls_data-dd03p INTO ls_expected_dd03p.
      READ TABLE ls_deserialized-dd03p INTO ls_actual_dd03p
        WITH KEY fieldname = ls_expected_dd03p-fieldname.
      cl_abap_unit_assert=>assert_equals( exp = 0
                                          act = sy-subrc ).
      cl_abap_unit_assert=>assert_equals(
        exp = ls_expected_dd03p-keyflag
        act = ls_actual_dd03p-keyflag ).
      cl_abap_unit_assert=>assert_equals(
        exp = ls_expected_dd03p-notnull
        act = ls_actual_dd03p-notnull ).
      cl_abap_unit_assert=>assert_equals(
        exp = ls_expected_dd03p-rollname
        act = ls_actual_dd03p-rollname ).
      cl_abap_unit_assert=>assert_equals(
        exp = ls_expected_dd03p-datatype
        act = ls_actual_dd03p-datatype ).
      cl_abap_unit_assert=>assert_equals(
        exp = ls_expected_dd03p-leng
        act = ls_actual_dd03p-leng ).
      cl_abap_unit_assert=>assert_equals(
        exp = ls_expected_dd03p-decimals
        act = ls_actual_dd03p-decimals ).
    ENDLOOP.

    cl_abap_unit_assert=>assert_equals(
      exp = lines( ls_data-dd08v )
      act = lines( ls_deserialized-dd08v ) ).
    LOOP AT ls_data-dd08v INTO ls_expected_dd08v.
      READ TABLE ls_deserialized-dd08v INTO ls_actual_dd08v
        WITH KEY fieldname = ls_expected_dd08v-fieldname.
      cl_abap_unit_assert=>assert_equals( exp = 0
                                          act = sy-subrc ).
      cl_abap_unit_assert=>assert_equals(
        exp = ls_expected_dd08v-checktable
        act = ls_actual_dd08v-checktable ).
      cl_abap_unit_assert=>assert_equals(
        exp = ls_expected_dd08v-frkart
        act = ls_actual_dd08v-frkart ).
      cl_abap_unit_assert=>assert_equals(
        exp = ls_expected_dd08v-card
        act = ls_actual_dd08v-card ).
      cl_abap_unit_assert=>assert_equals(
        exp = ls_expected_dd08v-cardleft
        act = ls_actual_dd08v-cardleft ).
      cl_abap_unit_assert=>assert_equals(
        exp = ls_expected_dd08v-ddtext
        act = ls_actual_dd08v-ddtext ).
    ENDLOOP.

    cl_abap_unit_assert=>assert_equals(
      exp = lines( ls_data-dd05m )
      act = lines( ls_deserialized-dd05m ) ).
    LOOP AT ls_data-dd05m INTO ls_expected_dd05m.
      READ TABLE ls_deserialized-dd05m INTO ls_actual_dd05m
        WITH KEY fieldname = ls_expected_dd05m-fieldname
                 primpos   = ls_expected_dd05m-primpos.
      cl_abap_unit_assert=>assert_equals(
        exp = 0
        act = sy-subrc ).
      cl_abap_unit_assert=>assert_equals(
        exp = ls_expected_dd05m-checkfield
        act = ls_actual_dd05m-checkfield ).
      cl_abap_unit_assert=>assert_equals(
        exp = ls_expected_dd05m-fortable
        act = ls_actual_dd05m-fortable ).
      cl_abap_unit_assert=>assert_equals(
        exp = ls_expected_dd05m-forkey
        act = ls_actual_dd05m-forkey ).
      cl_abap_unit_assert=>assert_equals(
        exp = ls_expected_dd05m-checktable
        act = ls_actual_dd05m-checktable ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test1.

    DATA lv_ddl TYPE string.
    DATA lv_xml TYPE string.

    lv_ddl =
      `@EndUserText.label : 'Generated by abapGit'` && |\n| &&
      `@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE` && |\n| &&
      `@AbapCatalog.tableCategory : #TRANSPARENT` && |\n| &&
      `@AbapCatalog.deliveryClass : #L` && |\n| &&
      `@AbapCatalog.dataMaintenance : #RESTRICTED` && |\n| &&
      `define table zabapgit {` && |\n| &&
      `` && |\n| &&
      `  key type  : abap.char(12) not null;` && |\n| &&
      `  key value : abap.char(12) not null;` && |\n| &&
      `  data_str  : abap.string(0);` && |\n| &&
      `` && |\n| &&
      `}`.

    lv_xml =
      `<?xml version="1.0" encoding="utf-8"?>` && |\n| &&
      ` <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">` && |\n| &&
      `  <asx:values>` && |\n| &&
      `   <DD02V>` && |\n| &&
      `    <TABNAME>ZABAPGIT</TABNAME>` && |\n| &&
      `    <DDLANGUAGE>E</DDLANGUAGE>` && |\n| &&
      `    <TABCLASS>TRANSP</TABCLASS>` && |\n| &&
      `    <DDTEXT>Generated by abapGit</DDTEXT>` && |\n| &&
      `    <MASTERLANG>E</MASTERLANG>` && |\n| &&
      `    <CONTFLAG>L</CONTFLAG>` && |\n| &&
      `    <EXCLASS>1</EXCLASS>` && |\n| &&
      `   </DD02V>` && |\n| &&
      `   <DD09L>` && |\n| &&
      `    <TABNAME>ZABAPGIT</TABNAME>` && |\n| &&
      `    <AS4LOCAL>A</AS4LOCAL>` && |\n| &&
      `    <TABKAT>1</TABKAT>` && |\n| &&
      `    <TABART>APPL1</TABART>` && |\n| &&
      `    <BUFALLOW>N</BUFALLOW>` && |\n| &&
      `   </DD09L>` && |\n| &&
      `   <DD03P_TABLE>` && |\n| &&
      `    <DD03P>` && |\n| &&
      `     <FIELDNAME>TYPE</FIELDNAME>` && |\n| &&
      `     <KEYFLAG>X</KEYFLAG>` && |\n| &&
      `     <ADMINFIELD>0</ADMINFIELD>` && |\n| &&
      `     <INTTYPE>C</INTTYPE>` && |\n| &&
      `     <INTLEN>000024</INTLEN>` && |\n| &&
      `     <NOTNULL>X</NOTNULL>` && |\n| &&
      `     <DATATYPE>CHAR</DATATYPE>` && |\n| &&
      `     <LENG>000012</LENG>` && |\n| &&
      `     <MASK>  CHAR</MASK>` && |\n| &&
      `    </DD03P>` && |\n| &&
      `    <DD03P>` && |\n| &&
      `     <FIELDNAME>VALUE</FIELDNAME>` && |\n| &&
      `     <KEYFLAG>X</KEYFLAG>` && |\n| &&
      `     <ADMINFIELD>0</ADMINFIELD>` && |\n| &&
      `     <INTTYPE>C</INTTYPE>` && |\n| &&
      `     <INTLEN>000024</INTLEN>` && |\n| &&
      `     <NOTNULL>X</NOTNULL>` && |\n| &&
      `     <DATATYPE>CHAR</DATATYPE>` && |\n| &&
      `     <LENG>000012</LENG>` && |\n| &&
      `     <MASK>  CHAR</MASK>` && |\n| &&
      `    </DD03P>` && |\n| &&
      `    <DD03P>` && |\n| &&
      `     <FIELDNAME>DATA_STR</FIELDNAME>` && |\n| &&
      `     <ADMINFIELD>0</ADMINFIELD>` && |\n| &&
      `     <INTTYPE>g</INTTYPE>` && |\n| &&
      `     <INTLEN>000008</INTLEN>` && |\n| &&
      `     <DATATYPE>STRG</DATATYPE>` && |\n| &&
      `     <MASK>  STRG</MASK>` && |\n| &&
      `    </DD03P>` && |\n| &&
      `   </DD03P_TABLE>` && |\n| &&
      `  </asx:values>` && |\n| &&
      ` </asx:abap>`.

    test( iv_xml = lv_xml
          iv_ddl = lv_ddl ).

  ENDMETHOD.

  METHOD test2.

    DATA lv_ddl TYPE string.
    DATA lv_xml TYPE string.

    lv_ddl =
      `@EndUserText.label : 'Clients'` && |\n| &&
      `@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE` && |\n| &&
      `@AbapCatalog.tableCategory : #TRANSPARENT` && |\n| &&
      `@AbapCatalog.deliveryClass : #C` && |\n| &&
      `@AbapCatalog.dataMaintenance : #ALLOWED` && |\n| &&
      `define table t000 {` && |\n| &&
      `` && |\n| &&
      `  key mandt  : mandt not null;` && |\n| &&
      `  mtext      : mtext_d not null;` && |\n| &&
      `  ort01      : ort01 not null;` && |\n| &&
      `  @AbapCatalog.foreignKey.keyType : #KEY` && |\n| &&
      `  @AbapCatalog.foreignKey.screenCheck : true` && |\n| &&
      `  mwaer      : mwaer not null` && |\n| &&
      `    with foreign key [1..*,1] tcurc` && |\n| &&
      `      where mandt = t000.mandt` && |\n| &&
      `        and waers = t000.mwaer;` && |\n| &&
      `  adrnr      : char10 not null;` && |\n| &&
      `  cccategory : cccategory not null;` && |\n| &&
      `  cccoractiv : cccoractiv not null;` && |\n| &&
      `  ccnocliind : ccnocliind not null;` && |\n| &&
      `  cccopylock : cccopylock not null;` && |\n| &&
      `  ccnocascad : ccnocascad not null;` && |\n| &&
      `  ccsoftlock : ccsoftlock not null;` && |\n| &&
      `  ccorigcont : ccorigcont not null;` && |\n| &&
      `  ccimaildis : ccimaildis not null;` && |\n| &&
      `  cctemplock : cctemplock not null;` && |\n| &&
      `  changeuser : as4user not null;` && |\n| &&
      `  changedate : as4date not null;` && |\n| &&
      `  @AbapCatalog.foreignKey.label : 'Logical system'` && |\n| &&
      `  @AbapCatalog.foreignKey.keyType : #KEY` && |\n| &&
      `  @AbapCatalog.foreignKey.screenCheck : true` && |\n| &&
      `  logsys     : logsys not null` && |\n| &&
      `    with foreign key [1,0..1] tbdls` && |\n| &&
      `      where logsys = t000.logsys;` && |\n| &&
      `` && |\n| &&
      `}`.

    lv_xml =
      `<?xml version="1.0" encoding="utf-8"?>` && |\n| &&
      ` <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">` && |\n| &&
      `  <asx:values>` && |\n| &&
      `   <DD02V>` && |\n| &&
      `    <TABNAME>T000</TABNAME>` && |\n| &&
      `    <DDLANGUAGE>E</DDLANGUAGE>` && |\n| &&
      `    <TABCLASS>TRANSP</TABCLASS>` && |\n| &&
      `    <BUFFERED>E</BUFFERED>` && |\n| &&
      `    <DDTEXT>Clients</DDTEXT>` && |\n| &&
      `    <APPLCLASS>SAP</APPLCLASS>` && |\n| &&
      `    <MASTERLANG>D</MASTERLANG>` && |\n| &&
      `    <MAINFLAG>X</MAINFLAG>` && |\n| &&
      `    <CONTFLAG>C</CONTFLAG>` && |\n| &&
      `    <SHLPEXI>X</SHLPEXI>` && |\n| &&
      `    <EXCLASS>1</EXCLASS>` && |\n| &&
      `   </DD02V>` && |\n| &&
      `   <DD09L>` && |\n| &&
      `    <TABNAME>T000</TABNAME>` && |\n| &&
      `    <AS4LOCAL>A</AS4LOCAL>` && |\n| &&
      `    <TABKAT>0</TABKAT>` && |\n| &&
      `    <TABART>APPL2</TABART>` && |\n| &&
      `    <PUFFERUNG>X</PUFFERUNG>` && |\n| &&
      `    <PROTOKOLL>X</PROTOKOLL>` && |\n| &&
      `    <TRANSPFLAG>X</TRANSPFLAG>` && |\n| &&
      `    <BUFALLOW>X</BUFALLOW>` && |\n| &&
      `   </DD09L>` && |\n| &&
      `   <DD03P_TABLE>` && |\n| &&
      `    <DD03P>` && |\n| &&
      `     <FIELDNAME>MANDT</FIELDNAME>` && |\n| &&
      `     <KEYFLAG>X</KEYFLAG>` && |\n| &&
      `     <ROLLNAME>MANDT</ROLLNAME>` && |\n| &&
      `     <ADMINFIELD>0</ADMINFIELD>` && |\n| &&
      `     <NOTNULL>X</NOTNULL>` && |\n| &&
      `     <COMPTYPE>E</COMPTYPE>` && |\n| &&
      `    </DD03P>` && |\n| &&
      `    <DD03P>` && |\n| &&
      `     <FIELDNAME>MTEXT</FIELDNAME>` && |\n| &&
      `     <ROLLNAME>MTEXT_D</ROLLNAME>` && |\n| &&
      `     <ADMINFIELD>0</ADMINFIELD>` && |\n| &&
      `     <NOTNULL>X</NOTNULL>` && |\n| &&
      `     <COMPTYPE>E</COMPTYPE>` && |\n| &&
      `    </DD03P>` && |\n| &&
      `    <DD03P>` && |\n| &&
      `     <FIELDNAME>ORT01</FIELDNAME>` && |\n| &&
      `     <ROLLNAME>ORT01</ROLLNAME>` && |\n| &&
      `     <ADMINFIELD>0</ADMINFIELD>` && |\n| &&
      `     <NOTNULL>X</NOTNULL>` && |\n| &&
      `     <COMPTYPE>E</COMPTYPE>` && |\n| &&
      `    </DD03P>` && |\n| &&
      `    <DD03P>` && |\n| &&
      `     <FIELDNAME>MWAER</FIELDNAME>` && |\n| &&
      `     <ROLLNAME>MWAER</ROLLNAME>` && |\n| &&
      `     <CHECKTABLE>TCURC</CHECKTABLE>` && |\n| &&
      `     <ADMINFIELD>0</ADMINFIELD>` && |\n| &&
      `     <NOTNULL>X</NOTNULL>` && |\n| &&
      `     <SHLPORIGIN>P</SHLPORIGIN>` && |\n| &&
      `     <COMPTYPE>E</COMPTYPE>` && |\n| &&
      `    </DD03P>` && |\n| &&
      `    <DD03P>` && |\n| &&
      `     <FIELDNAME>ADRNR</FIELDNAME>` && |\n| &&
      `     <ROLLNAME>CHAR10</ROLLNAME>` && |\n| &&
      `     <ADMINFIELD>0</ADMINFIELD>` && |\n| &&
      `     <NOTNULL>X</NOTNULL>` && |\n| &&
      `     <COMPTYPE>E</COMPTYPE>` && |\n| &&
      `    </DD03P>` && |\n| &&
      `    <DD03P>` && |\n| &&
      `     <FIELDNAME>CCCATEGORY</FIELDNAME>` && |\n| &&
      `     <ROLLNAME>CCCATEGORY</ROLLNAME>` && |\n| &&
      `     <ADMINFIELD>0</ADMINFIELD>` && |\n| &&
      `     <NOTNULL>X</NOTNULL>` && |\n| &&
      `     <VALEXI>X</VALEXI>` && |\n| &&
      `     <SHLPORIGIN>F</SHLPORIGIN>` && |\n| &&
      `     <COMPTYPE>E</COMPTYPE>` && |\n| &&
      `    </DD03P>` && |\n| &&
      `    <DD03P>` && |\n| &&
      `     <FIELDNAME>CCCORACTIV</FIELDNAME>` && |\n| &&
      `     <ROLLNAME>CCCORACTIV</ROLLNAME>` && |\n| &&
      `     <ADMINFIELD>0</ADMINFIELD>` && |\n| &&
      `     <NOTNULL>X</NOTNULL>` && |\n| &&
      `     <VALEXI>X</VALEXI>` && |\n| &&
      `     <SHLPORIGIN>F</SHLPORIGIN>` && |\n| &&
      `     <COMPTYPE>E</COMPTYPE>` && |\n| &&
      `    </DD03P>` && |\n| &&
      `    <DD03P>` && |\n| &&
      `     <FIELDNAME>CCNOCLIIND</FIELDNAME>` && |\n| &&
      `     <ROLLNAME>CCNOCLIIND</ROLLNAME>` && |\n| &&
      `     <ADMINFIELD>0</ADMINFIELD>` && |\n| &&
      `     <NOTNULL>X</NOTNULL>` && |\n| &&
      `     <VALEXI>X</VALEXI>` && |\n| &&
      `     <SHLPORIGIN>F</SHLPORIGIN>` && |\n| &&
      `     <COMPTYPE>E</COMPTYPE>` && |\n| &&
      `    </DD03P>` && |\n| &&
      `    <DD03P>` && |\n| &&
      `     <FIELDNAME>CCCOPYLOCK</FIELDNAME>` && |\n| &&
      `     <ROLLNAME>CCCOPYLOCK</ROLLNAME>` && |\n| &&
      `     <ADMINFIELD>0</ADMINFIELD>` && |\n| &&
      `     <NOTNULL>X</NOTNULL>` && |\n| &&
      `     <VALEXI>X</VALEXI>` && |\n| &&
      `     <SHLPORIGIN>F</SHLPORIGIN>` && |\n| &&
      `     <COMPTYPE>E</COMPTYPE>` && |\n| &&
      `    </DD03P>` && |\n| &&
      `    <DD03P>` && |\n| &&
      `     <FIELDNAME>CCNOCASCAD</FIELDNAME>` && |\n| &&
      `     <ROLLNAME>CCNOCASCAD</ROLLNAME>` && |\n| &&
      `     <ADMINFIELD>0</ADMINFIELD>` && |\n| &&
      `     <NOTNULL>X</NOTNULL>` && |\n| &&
      `     <VALEXI>X</VALEXI>` && |\n| &&
      `     <SHLPORIGIN>F</SHLPORIGIN>` && |\n| &&
      `     <COMPTYPE>E</COMPTYPE>` && |\n| &&
      `    </DD03P>` && |\n| &&
      `    <DD03P>` && |\n| &&
      `     <FIELDNAME>CCSOFTLOCK</FIELDNAME>` && |\n| &&
      `     <ROLLNAME>CCSOFTLOCK</ROLLNAME>` && |\n| &&
      `     <ADMINFIELD>0</ADMINFIELD>` && |\n| &&
      `     <NOTNULL>X</NOTNULL>` && |\n| &&
      `     <VALEXI>X</VALEXI>` && |\n| &&
      `     <SHLPORIGIN>F</SHLPORIGIN>` && |\n| &&
      `     <COMPTYPE>E</COMPTYPE>` && |\n| &&
      `    </DD03P>` && |\n| &&
      `    <DD03P>` && |\n| &&
      `     <FIELDNAME>CCORIGCONT</FIELDNAME>` && |\n| &&
      `     <ROLLNAME>CCORIGCONT</ROLLNAME>` && |\n| &&
      `     <ADMINFIELD>0</ADMINFIELD>` && |\n| &&
      `     <NOTNULL>X</NOTNULL>` && |\n| &&
      `     <COMPTYPE>E</COMPTYPE>` && |\n| &&
      `    </DD03P>` && |\n| &&
      `    <DD03P>` && |\n| &&
      `     <FIELDNAME>CCIMAILDIS</FIELDNAME>` && |\n| &&
      `     <ROLLNAME>CCIMAILDIS</ROLLNAME>` && |\n| &&
      `     <ADMINFIELD>0</ADMINFIELD>` && |\n| &&
      `     <NOTNULL>X</NOTNULL>` && |\n| &&
      `     <VALEXI>X</VALEXI>` && |\n| &&
      `     <SHLPORIGIN>F</SHLPORIGIN>` && |\n| &&
      `     <COMPTYPE>E</COMPTYPE>` && |\n| &&
      `    </DD03P>` && |\n| &&
      `    <DD03P>` && |\n| &&
      `     <FIELDNAME>CCTEMPLOCK</FIELDNAME>` && |\n| &&
      `     <ROLLNAME>CCTEMPLOCK</ROLLNAME>` && |\n| &&
      `     <ADMINFIELD>0</ADMINFIELD>` && |\n| &&
      `     <NOTNULL>X</NOTNULL>` && |\n| &&
      `     <VALEXI>X</VALEXI>` && |\n| &&
      `     <SHLPORIGIN>F</SHLPORIGIN>` && |\n| &&
      `     <COMPTYPE>E</COMPTYPE>` && |\n| &&
      `    </DD03P>` && |\n| &&
      `    <DD03P>` && |\n| &&
      `     <FIELDNAME>CHANGEUSER</FIELDNAME>` && |\n| &&
      `     <ROLLNAME>AS4USER</ROLLNAME>` && |\n| &&
      `     <ADMINFIELD>0</ADMINFIELD>` && |\n| &&
      `     <NOTNULL>X</NOTNULL>` && |\n| &&
      `     <COMPTYPE>E</COMPTYPE>` && |\n| &&
      `     <ANONYMOUS>X</ANONYMOUS>` && |\n| &&
      `    </DD03P>` && |\n| &&
      `    <DD03P>` && |\n| &&
      `     <FIELDNAME>CHANGEDATE</FIELDNAME>` && |\n| &&
      `     <ROLLNAME>AS4DATE</ROLLNAME>` && |\n| &&
      `     <ADMINFIELD>0</ADMINFIELD>` && |\n| &&
      `     <NOTNULL>X</NOTNULL>` && |\n| &&
      `     <SHLPORIGIN>T</SHLPORIGIN>` && |\n| &&
      `     <COMPTYPE>E</COMPTYPE>` && |\n| &&
      `    </DD03P>` && |\n| &&
      `    <DD03P>` && |\n| &&
      `     <FIELDNAME>LOGSYS</FIELDNAME>` && |\n| &&
      `     <ROLLNAME>LOGSYS</ROLLNAME>` && |\n| &&
      `     <CHECKTABLE>TBDLS</CHECKTABLE>` && |\n| &&
      `     <ADMINFIELD>0</ADMINFIELD>` && |\n| &&
      `     <NOTNULL>X</NOTNULL>` && |\n| &&
      `     <SHLPORIGIN>P</SHLPORIGIN>` && |\n| &&
      `     <COMPTYPE>E</COMPTYPE>` && |\n| &&
      `    </DD03P>` && |\n| &&
      `   </DD03P_TABLE>` && |\n| &&
      `   <DD05M_TABLE>` && |\n| &&
      `    <DD05M>` && |\n| &&
      `     <FIELDNAME>LOGSYS</FIELDNAME>` && |\n| &&
      `     <FORTABLE>T000</FORTABLE>` && |\n| &&
      `     <FORKEY>LOGSYS</FORKEY>` && |\n| &&
      `     <CHECKTABLE>TBDLS</CHECKTABLE>` && |\n| &&
      `     <CHECKFIELD>LOGSYS</CHECKFIELD>` && |\n| &&
      `     <PRIMPOS>0001</PRIMPOS>` && |\n| &&
      `     <DOMNAME>LOGSYS</DOMNAME>` && |\n| &&
      `     <DATATYPE>CHAR</DATATYPE>` && |\n| &&
      `    </DD05M>` && |\n| &&
      `    <DD05M>` && |\n| &&
      `     <FIELDNAME>MWAER</FIELDNAME>` && |\n| &&
      `     <FORTABLE>T000</FORTABLE>` && |\n| &&
      `     <FORKEY>MANDT</FORKEY>` && |\n| &&
      `     <CHECKTABLE>TCURC</CHECKTABLE>` && |\n| &&
      `     <CHECKFIELD>MANDT</CHECKFIELD>` && |\n| &&
      `     <PRIMPOS>0001</PRIMPOS>` && |\n| &&
      `     <DOMNAME>MANDT</DOMNAME>` && |\n| &&
      `     <DATATYPE>CLNT</DATATYPE>` && |\n| &&
      `    </DD05M>` && |\n| &&
      `    <DD05M>` && |\n| &&
      `     <FIELDNAME>MWAER</FIELDNAME>` && |\n| &&
      `     <FORTABLE>T000</FORTABLE>` && |\n| &&
      `     <FORKEY>MWAER</FORKEY>` && |\n| &&
      `     <CHECKTABLE>TCURC</CHECKTABLE>` && |\n| &&
      `     <CHECKFIELD>WAERS</CHECKFIELD>` && |\n| &&
      `     <PRIMPOS>0002</PRIMPOS>` && |\n| &&
      `     <DOMNAME>WAERS</DOMNAME>` && |\n| &&
      `     <DATATYPE>CUKY</DATATYPE>` && |\n| &&
      `    </DD05M>` && |\n| &&
      `   </DD05M_TABLE>` && |\n| &&
      `   <DD08V_TABLE>` && |\n| &&
      `    <DD08V>` && |\n| &&
      `     <FIELDNAME>LOGSYS</FIELDNAME>` && |\n| &&
      `     <CHECKTABLE>TBDLS</CHECKTABLE>` && |\n| &&
      `     <FRKART>KEY</FRKART>` && |\n| &&
      `     <CARD>1</CARD>` && |\n| &&
      `     <DDTEXT>Logical system</DDTEXT>` && |\n| &&
      `     <CARDLEFT>C</CARDLEFT>` && |\n| &&
      `    </DD08V>` && |\n| &&
      `    <DD08V>` && |\n| &&
      `     <FIELDNAME>MWAER</FIELDNAME>` && |\n| &&
      `     <CHECKTABLE>TCURC</CHECKTABLE>` && |\n| &&
      `     <FRKART>KEY</FRKART>` && |\n| &&
      `     <CARD>N</CARD>` && |\n| &&
      `     <CARDLEFT>1</CARDLEFT>` && |\n| &&
      `    </DD08V>` && |\n| &&
      `   </DD08V_TABLE>` && |\n| &&
      `   <DD35V_TALE>` && |\n| &&
      `    <DD35V>` && |\n| &&
      `     <SHLPNAME>H_T000</SHLPNAME>` && |\n| &&
      `    </DD35V>` && |\n| &&
      `   </DD35V_TALE>` && |\n| &&
      `   <DD36M>` && |\n| &&
      `    <DD36M>` && |\n| &&
      `     <SHLPNAME>H_T000</SHLPNAME>` && |\n| &&
      `     <SHLPFIELD>MANDT</SHLPFIELD>` && |\n| &&
      `     <FLPOSITION>0001</FLPOSITION>` && |\n| &&
      `     <SHTABLE>T000</SHTABLE>` && |\n| &&
      `     <SHFIELD>MANDT</SHFIELD>` && |\n| &&
      `     <SHLPINPUT>X</SHLPINPUT>` && |\n| &&
      `     <SHLPOUTPUT>X</SHLPOUTPUT>` && |\n| &&
      `     <ROLLNAME>MANDT</ROLLNAME>` && |\n| &&
      `     <DOMNAME>MANDT</DOMNAME>` && |\n| &&
      `     <DATATYPE>CLNT</DATATYPE>` && |\n| &&
      `     <LENG>000003</LENG>` && |\n| &&
      `    </DD36M>` && |\n| &&
      `    <DD36M>` && |\n| &&
      `     <SHLPNAME>H_T000</SHLPNAME>` && |\n| &&
      `     <SHLPFIELD>MTEXT</SHLPFIELD>` && |\n| &&
      `     <FLPOSITION>0002</FLPOSITION>` && |\n| &&
      `     <SHTYPE>G</SHTYPE>` && |\n| &&
      `     <SHLPOUTPUT>X</SHLPOUTPUT>` && |\n| &&
      `     <ROLLNAME>MTEXT_D</ROLLNAME>` && |\n| &&
      `     <DOMNAME>TEXT25</DOMNAME>` && |\n| &&
      `     <DATATYPE>CHAR</DATATYPE>` && |\n| &&
      `     <LENG>000025</LENG>` && |\n| &&
      `    </DD36M>` && |\n| &&
      `    <DD36M>` && |\n| &&
      `     <SHLPNAME>H_T000</SHLPNAME>` && |\n| &&
      `     <SHLPFIELD>ORT01</SHLPFIELD>` && |\n| &&
      `     <FLPOSITION>0003</FLPOSITION>` && |\n| &&
      `     <SHTYPE>G</SHTYPE>` && |\n| &&
      `     <SHLPOUTPUT>X</SHLPOUTPUT>` && |\n| &&
      `     <ROLLNAME>ORT01</ROLLNAME>` && |\n| &&
      `     <DOMNAME>TEXT25</DOMNAME>` && |\n| &&
      `     <DATATYPE>CHAR</DATATYPE>` && |\n| &&
      `     <LENG>000025</LENG>` && |\n| &&
      `    </DD36M>` && |\n| &&
      `   </DD36M>` && |\n| &&
      `   <TABL_EXTRAS>` && |\n| &&
      `    <TDDAT>` && |\n| &&
      `     <TABNAME>T000</TABNAME>` && |\n| &&
      `     <CCLASS>SS</CCLASS>` && |\n| &&
      `    </TDDAT>` && |\n| &&
      `   </TABL_EXTRAS>` && |\n| &&
      `  </asx:values>` && |\n| &&
      ` </asx:abap>`.

    test( iv_xml = lv_xml
          iv_ddl = lv_ddl ).

  ENDMETHOD.

  METHOD test_includes_and_value_help.

    DATA lo_format TYPE REF TO zcl_abapgit_object_tabl_ddl.
    DATA ls_data TYPE zif_abapgit_object_tabl=>ty_internal.
    DATA ls_field LIKE LINE OF ls_data-dd03p.
    DATA ls_value_help LIKE LINE OF ls_data-dd35v.
    DATA ls_value_condition LIKE LINE OF ls_data-dd36m.
    DATA ls_extension_key LIKE LINE OF ls_data-dd08v.
    DATA lv_ddl TYPE string.
    DATA lv_expected TYPE string.

    lv_ddl =
      `@EndUserText.label : 'Two  spaces:; and ''quotes'''` && |\n| &&
      `@AbapCatalog.enhancementCategory : #NOT_EXTENSIBLE` && |\n| &&
      `@AbapCatalog.tableCategory : #TRANSPARENT` && |\n| &&
      `@AbapCatalog.deliveryClass : #C` && |\n| &&
      `@AbapCatalog.dataMaintenance : #RESTRICTED` && |\n| &&
      `define table zinclude {` && |\n| &&
      `` && |\n| &&
      `  include zcommon not null;` && |\n| &&
      `  named : include znamed with suffix foo not null;` && |\n| &&
      `  @AbapCatalog.foreignKey.label : 'Extension key'` && |\n| &&
      `  extend key :` && |\n| &&
      `    with foreign key [1,1] zref` && |\n| &&
      `      where code = zsource.code;` && |\n| &&
      `  code : abap.char(4)` &&
      `    with value help zhelp` &&
      `      where code = zsource.code;` && |\n| &&
      `  key : abap.char(1);` && |\n| &&
      `}`.

    CREATE OBJECT lo_format.
    ls_data = lo_format->deserialize( lv_ddl ).

    cl_abap_unit_assert=>assert_equals(
      exp = `Two  spaces:; and 'quotes'`
      act = ls_data-dd02v-ddtext ).
    cl_abap_unit_assert=>assert_equals(
      exp = 5
      act = lines( ls_data-dd03p ) ).

    READ TABLE ls_data-dd03p INTO ls_field
      WITH KEY fieldname = 'KEY' adminfield = '0'.
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = sy-subrc ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'CHAR'
      act = ls_field-datatype ).

    READ TABLE ls_data-dd03p INTO ls_field WITH KEY precfield = 'ZCOMMON'.
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = sy-subrc ).
    cl_abap_unit_assert=>assert_equals(
      exp = '.INCLU'
      act = ls_field-fieldname ).
    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = ls_field-notnull ).

    READ TABLE ls_data-dd03p INTO ls_field WITH KEY precfield = 'ZNAMED'.
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = sy-subrc ).
    cl_abap_unit_assert=>assert_equals(
      exp = '.INCLU-FOO'
      act = ls_field-fieldname ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'NAMED'
      act = ls_field-groupname ).
    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = ls_field-notnull ).

    READ TABLE ls_data-dd35v INTO ls_value_help WITH KEY fieldname = 'CODE'.
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = sy-subrc ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'ZHELP'
      act = ls_value_help-shlpname ).

    READ TABLE ls_data-dd36m INTO ls_value_condition WITH KEY fieldname = 'CODE'.
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = sy-subrc ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'CODE'
      act = ls_value_condition-shlpfield ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'ZSOURCE'
      act = ls_value_condition-shtable ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'CODE'
      act = ls_value_condition-shfield ).

    READ TABLE ls_data-dd08v INTO ls_extension_key
      WITH KEY fieldname = 'KEY'.
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = sy-subrc ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'ZREF'
      act = ls_extension_key-checktable ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'Extension key'
      act = ls_extension_key-ddtext ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'Y'
      act = ls_extension_key-noinherit ).

    lv_ddl = lo_format->serialize( ls_data ).
    lv_expected = |include zcommon not null;\n  named : include|.
    FIND lv_expected IN lv_ddl.
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = sy-subrc ).
    lv_expected = |where code = zsource.code;\n  code  :|.
    FIND lv_expected IN lv_ddl.
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = sy-subrc ).
    ls_data = lo_format->deserialize( lv_ddl ).
    cl_abap_unit_assert=>assert_equals(
      exp = 5
      act = lines( ls_data-dd03p ) ).
    READ TABLE ls_data-dd08v INTO ls_extension_key
      WITH KEY fieldname = 'KEY'.
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = sy-subrc ).

    CLEAR ls_data-dd36m.
    ls_value_condition-fieldname = 'CODE'.
    ls_value_condition-shlpname = 'ZHELP'.
    ls_value_condition-shtype = 'F'.
    ls_value_condition-shtable = 'ZSOURCE'.
    ls_value_condition-shlpfield = 'VAR'.
    ls_value_condition-shfield = 'VAR'.
    ls_value_condition-flposition = 1.
    APPEND ls_value_condition TO ls_data-dd36m.
    ls_value_condition-shlpfield = 'VTEXT'.
    ls_value_condition-shfield = 'VTEXT'.
    ls_value_condition-flposition = 2.
    APPEND ls_value_condition TO ls_data-dd36m.
    ls_value_condition-shlpfield = 'ENAME'.
    ls_value_condition-shfield = 'ENAME'.
    ls_value_condition-flposition = 3.
    APPEND ls_value_condition TO ls_data-dd36m.
    lv_ddl = lo_format->serialize( ls_data ).
    lv_expected = |where ename = zsource.ename\n        and var = zsource.var\n        and vtext = zsource.vtext|.
    FIND lv_expected IN lv_ddl.
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = sy-subrc ).

  ENDMETHOD.

  METHOD annotations_and_types.

    DATA lo_format TYPE REF TO zcl_abapgit_object_tabl_ddl.
    DATA ls_data TYPE zif_abapgit_object_tabl=>ty_internal.
    DATA ls_field LIKE LINE OF ls_data-dd03p.
    DATA ls_foreign_key LIKE LINE OF ls_data-dd08v.
    DATA ls_condition LIKE LINE OF ls_data-dd05m.
    DATA lv_ddl TYPE string.
    DATA lv_roundtrip TYPE string.
    DATA lv_exclass TYPE c LENGTH 1.
    FIELD-SYMBOLS <lv_is_gtt> TYPE abap_bool.
    FIELD-SYMBOLS <lv_invhash> TYPE c.
    FIELD-SYMBOLS <lv_outputstyle> TYPE zif_abapgit_aff_doma_v1=>ty_output_style.

    lv_ddl =
      `// comments and blank lines are ignored` && |\n| &&
      `@EndUserText.label : 'Temporary table'` && |\n| &&
      `@AbapCatalog.enhancement.category : #EXTENSIBLE_ANY` && |\n| &&
      `@AbapCatalog.tableCategory : #GLOBAL_TEMPORARY` && |\n| &&
      `@AbapCatalog.activationType : #NAMETAB_GENERATION_OFFLINE` && |\n| &&
      `@AbapCatalog.deliveryClass : #C` && |\n| &&
      `@AbapCatalog.dataMaintenance : #NOT_ALLOWED` && |\n| &&
      `@AbapCatalog.primaryKey.invertedHashIndex : true` && |\n| &&
      `define table zannotations {` && |\n| &&
      `  @EndUserText.label : 'Amount  field'` && |\n| &&
      `  @Semantics.amount.currencyCode : 'zannotations.cuky'` && |\n| &&
      `  amount : abap.curr(10,2);` && |\n| &&
      `  @Semantics.quantity.unitOfMeasure : 'zannotations.unit'` && |\n| &&
      `  quantity : abap.quan(10,3);` && |\n| &&
      `  @AbapCatalog.textLanguage` && |\n| &&
      `  text : abap.char(2);` && |\n| &&
      `  @AbapCatalog.decfloat.outputStyle : #NORMAL` && |\n| &&
      `  decimal : abap.df16_dec(16,3);` && |\n| &&
      `  @AbapCatalog.foreignKey.label : 'Foreign key'` && |\n| &&
      `  @AbapCatalog.foreignKey.keyType : #TEXT_KEY` && |\n| &&
      `  @AbapCatalog.foreignKey.screenCheck : false` && |\n| &&
      `  @AbapCatalog.foreignKey.messageClass : 'ZMSG'` && |\n| &&
      `  @AbapCatalog.foreignKey.messageNumber : '001'` && |\n| &&
      `  foreign_field : abap.char(1)` && |\n| &&
      `    with foreign key [1,1] zcheck` && |\n| &&
      `      where code = 'X';` && |\n| &&
      `}`.

    CREATE OBJECT lo_format.
    ls_data = lo_format->deserialize( lv_ddl ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'ZANNOTATIONS'
      act = ls_data-dd02v-tabname ).
    cl_abap_unit_assert=>assert_equals(
      exp = '4'
      act = ls_data-dd02v-exclass ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'TRANSP'
      act = ls_data-dd02v-tabclass ).
    cl_abap_unit_assert=>assert_equals(
      exp = '01'
      act = ls_data-dd02v-authclass ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'C'
      act = ls_data-dd02v-contflag ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'N'
      act = ls_data-dd02v-mainflag ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'Temporary table'
      act = ls_data-dd02v-ddtext ).

    ASSIGN COMPONENT 'IS_GTT' OF STRUCTURE ls_data-dd02v TO <lv_is_gtt>.
    IF sy-subrc = 0.
      cl_abap_unit_assert=>assert_equals(
        exp = abap_true
        act = <lv_is_gtt> ).
    ENDIF.
    ASSIGN COMPONENT 'PK_IS_INVHASH' OF STRUCTURE ls_data-dd02v TO <lv_invhash>.
    IF sy-subrc = 0.
      cl_abap_unit_assert=>assert_equals(
        exp = abap_true
        act = <lv_invhash> ).
    ENDIF.

    READ TABLE ls_data-dd03p INTO ls_field WITH KEY fieldname = 'AMOUNT'.
    cl_abap_unit_assert=>assert_equals(
      exp = 'ZANNOTATIONS'
      act = ls_field-reftable ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'CUKY'
      act = ls_field-reffield ).
    READ TABLE ls_data-dd03p INTO ls_field WITH KEY fieldname = 'QUANTITY'.
    cl_abap_unit_assert=>assert_equals(
      exp = 'ZANNOTATIONS'
      act = ls_field-reftable ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'UNIT'
      act = ls_field-reffield ).
    READ TABLE ls_data-dd03p INTO ls_field WITH KEY fieldname = 'TEXT'.
    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = ls_field-languflag ).
    READ TABLE ls_data-dd03p INTO ls_field WITH KEY fieldname = 'DECIMAL'.
    ASSIGN COMPONENT 'OUTPUTSTYLE' OF STRUCTURE ls_field TO <lv_outputstyle>.
    IF sy-subrc = 0.
      cl_abap_unit_assert=>assert_equals(
        exp = '00'
        act = <lv_outputstyle> ).
    ENDIF.

    READ TABLE ls_data-dd08v INTO ls_foreign_key
      WITH KEY fieldname = 'FOREIGN_FIELD'.
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = sy-subrc ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'TEXT'
      act = ls_foreign_key-frkart ).
    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = ls_foreign_key-checkflag ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'ZMSG'
      act = ls_foreign_key-arbgb ).
    cl_abap_unit_assert=>assert_equals(
      exp = '001'
      act = ls_foreign_key-msgnr ).
    READ TABLE ls_data-dd05m INTO ls_condition
      WITH KEY fieldname = 'FOREIGN_FIELD'.
    cl_abap_unit_assert=>assert_equals(
      exp = |'X'|
      act = ls_condition-fortable ).

    ls_data-dd02v-ddtext = 'Temporary table'.
    lv_roundtrip = lo_format->serialize( ls_data ).
    ls_data = lo_format->deserialize( lv_roundtrip ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'ZANNOTATIONS'
      act = ls_data-dd02v-tabname ).
    cl_abap_unit_assert=>assert_equals(
      exp = 5
      act = lines( ls_data-dd03p ) ).

    DO 5 TIMES.
      CLEAR ls_data.
      lv_exclass = sy-index - 1.
      ls_data-dd02v-tabname = 'ZTOP'.
      ls_data-dd02v-ddtext = 'Top-level'.
      ls_data-dd02v-exclass = lv_exclass.
      ls_data-dd02v-tabclass = 'TRANSP'.
      ls_data-dd02v-contflag = 'C'.
      lv_roundtrip = lo_format->serialize( ls_data ).
      ls_data = lo_format->deserialize( lv_roundtrip ).
      cl_abap_unit_assert=>assert_equals(
        exp = lv_exclass
        act = ls_data-dd02v-exclass ).
    ENDDO.

    CLEAR ls_data.
    ls_data-dd02v-tabname = 'ZTOP'.
    ls_data-dd02v-ddtext = 'Top-level'.
    ls_data-dd02v-exclass = '0'.
    ls_data-dd02v-tabclass = 'TRANSP'.
    ls_data-dd02v-contflag = 'C'.
    ls_data-dd02v-authclass = '02'.
    lv_roundtrip = lo_format->serialize( ls_data ).
    ls_data = lo_format->deserialize( lv_roundtrip ).
    cl_abap_unit_assert=>assert_equals(
      exp = '02'
      act = ls_data-dd02v-authclass ).

    lv_ddl =
      `@EndUserText.label : 'Legacy data maintenance'` && |\n| &&
      `@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE` && |\n| &&
      `@AbapCatalog.tableCategory : #TRANSPARENT` && |\n| &&
      `@AbapCatalog.deliveryClass : #C` && |\n| &&
      `@AbapCatalog.dataMaintenance : #LIMITED` && |\n| &&
      `define table zlegacy {` && |\n| &&
      `  value : abap.char(1);` && |\n| &&
      `}`.
    CLEAR ls_data.
    ls_data = lo_format->deserialize( lv_ddl ).
    cl_abap_unit_assert=>assert_initial( ls_data-dd02v-mainflag ).

  ENDMETHOD.

  METHOD builtin_types.

    DATA lo_format TYPE REF TO zcl_abapgit_object_tabl_ddl.
    DATA ls_data TYPE zif_abapgit_object_tabl=>ty_internal.
    DATA ls_roundtrip TYPE zif_abapgit_object_tabl=>ty_internal.
    DATA ls_field LIKE LINE OF ls_data-dd03p.
    DATA lt_specs TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA lv_spec TYPE string.
    DATA lv_name TYPE string.
    DATA lv_datatype TYPE string.
    DATA lv_length TYPE string.
    DATA lv_decimals TYPE string.
    DATA lv_expected_length TYPE i.
    DATA lv_expected_decimals TYPE i.
    DATA lv_ddl TYPE string.
    DATA lv_roundtrip TYPE string.

    lv_ddl =
      `define table ztypes {` && |\n| &&
      `  char_field : abap.char(4);` && |\n| &&
      `  numc_field : abap.numc(4);` && |\n| &&
      `  raw_field : abap.raw(4);` && |\n| &&
      `  string_field : abap.string(0);` && |\n| &&
      `  rawstring_field : abap.rawstring(0);` && |\n| &&
      `  sstring_field : abap.sstring(4);` && |\n| &&
      `  dec_field : abap.dec(10,2);` && |\n| &&
      `  curr_field : abap.curr(10,2);` && |\n| &&
      `  quan_field : abap.quan(10,3);` && |\n| &&
      `  unit_field : abap.unit(3);` && |\n| &&
      `  df16_dec_field : abap.df16_dec(16,3);` && |\n| &&
      `  df34_dec_field : abap.df34_dec(34,3);` && |\n| &&
      `  int1_field : abap.int1;` && |\n| &&
      `  int2_field : abap.int2;` && |\n| &&
      `  int4_field : abap.int4;` && |\n| &&
      `  int8_field : abap.int8;` && |\n| &&
      `  fltp_field : abap.fltp;` && |\n| &&
      `  accp_field : abap.accp;` && |\n| &&
      `  lang_field : abap.lang;` && |\n| &&
      `  datn_field : abap.datn;` && |\n| &&
      `  timn_field : abap.timn;` && |\n| &&
      `  utcl_field : abap.utcl;` && |\n| &&
      `  d16n_field : abap.d16n;` && |\n| &&
      `  d34n_field : abap.d34n;` && |\n| &&
      `  cuky_field : abap.cuky;` && |\n| &&
      `  dats_field : abap.dats;` && |\n| &&
      `  tims_field : abap.tims;` && |\n| &&
      `  df16_raw_field : abap.df16_raw;` && |\n| &&
      `  df16_scl_field : abap.df16_scl;` && |\n| &&
      `  df34_raw_field : abap.df34_raw;` && |\n| &&
      `  df34_scl_field : abap.df34_scl;` && |\n| &&
      `  key : abap.char(1);` && |\n| &&
      `  key key_field : abap.char(1) not null;` && |\n| &&
      `}`.

    APPEND `CHAR_FIELD;CHAR;4;` TO lt_specs.
    APPEND `NUMC_FIELD;NUMC;4;` TO lt_specs.
    APPEND `RAW_FIELD;RAW;4;` TO lt_specs.
    APPEND `STRING_FIELD;STRG;-;-` TO lt_specs.
    APPEND `RAWSTRING_FIELD;RSTR;-;-` TO lt_specs.
    APPEND `SSTRING_FIELD;SSTR;4;-` TO lt_specs.
    APPEND `DEC_FIELD;DEC;10;2` TO lt_specs.
    APPEND `CURR_FIELD;CURR;10;2` TO lt_specs.
    APPEND `QUAN_FIELD;QUAN;10;3` TO lt_specs.
    APPEND `UNIT_FIELD;UNIT;3;-` TO lt_specs.
    APPEND `DF16_DEC_FIELD;D16D;16;3` TO lt_specs.
    APPEND `DF34_DEC_FIELD;D34D;34;3` TO lt_specs.
    APPEND `INT1_FIELD;INT1;-;-` TO lt_specs.
    APPEND `INT2_FIELD;INT2;-;-` TO lt_specs.
    APPEND `INT4_FIELD;INT4;-;-` TO lt_specs.
    APPEND `INT8_FIELD;INT8;-;-` TO lt_specs.
    APPEND `FLTP_FIELD;FLTP;-;-` TO lt_specs.
    APPEND `ACCP_FIELD;ACCP;6;-` TO lt_specs.
    APPEND `LANG_FIELD;LANG;1;-` TO lt_specs.
    APPEND `DATN_FIELD;DATN;8;-` TO lt_specs.
    APPEND `TIMN_FIELD;TIMN;6;-` TO lt_specs.
    APPEND `UTCL_FIELD;UTCL;-;-` TO lt_specs.
    APPEND `D16N_FIELD;D16N;-;-` TO lt_specs.
    APPEND `D34N_FIELD;D34N;-;-` TO lt_specs.
    APPEND `CUKY_FIELD;CUKY;5;-` TO lt_specs.
    APPEND `DATS_FIELD;DATS;8;-` TO lt_specs.
    APPEND `TIMS_FIELD;TIMS;6;-` TO lt_specs.
    APPEND `DF16_RAW_FIELD;D16R;-;-` TO lt_specs.
    APPEND `DF16_SCL_FIELD;D16S;-;-` TO lt_specs.
    APPEND `DF34_RAW_FIELD;D34R;-;-` TO lt_specs.
    APPEND `DF34_SCL_FIELD;D34S;-;-` TO lt_specs.
    APPEND `KEY;CHAR;1;` TO lt_specs.
    APPEND `KEY_FIELD;CHAR;1;` TO lt_specs.

    CREATE OBJECT lo_format.
    ls_data = lo_format->deserialize( lv_ddl ).
    cl_abap_unit_assert=>assert_equals(
      exp = lines( lt_specs )
      act = lines( ls_data-dd03p ) ).
    LOOP AT lt_specs INTO lv_spec.
      SPLIT lv_spec AT ';' INTO lv_name lv_datatype lv_length lv_decimals.
      READ TABLE ls_data-dd03p INTO ls_field WITH KEY fieldname = lv_name.
      cl_abap_unit_assert=>assert_equals(
        exp = 0
        act = sy-subrc ).
      cl_abap_unit_assert=>assert_equals(
        exp = lv_datatype
        act = ls_field-datatype ).
      IF lv_length <> '-'.
        lv_expected_length = lv_length.
        cl_abap_unit_assert=>assert_equals(
          exp = lv_expected_length
          act = ls_field-leng ).
      ENDIF.
      IF lv_decimals <> '-'.
        lv_expected_decimals = lv_decimals.
        cl_abap_unit_assert=>assert_equals(
          exp = lv_expected_decimals
          act = ls_field-decimals ).
      ENDIF.
    ENDLOOP.

    ls_data-dd02v-tabname = 'ZTYPES'.
    ls_data-dd02v-exclass = '0'.
    ls_data-dd02v-tabclass = 'TRANSP'.
    ls_data-dd02v-contflag = 'C'.
    lv_roundtrip = lo_format->serialize( ls_data ).
    ls_roundtrip = lo_format->deserialize( lv_roundtrip ).
    cl_abap_unit_assert=>assert_equals(
      exp = lines( lt_specs )
      act = lines( ls_roundtrip-dd03p ) ).

  ENDMETHOD.

  METHOD foreign_key_cardinalities.

    DATA lo_format TYPE REF TO zcl_abapgit_object_tabl_ddl.
    DATA ls_data TYPE zif_abapgit_object_tabl=>ty_internal.
    DATA ls_roundtrip TYPE zif_abapgit_object_tabl=>ty_internal.
    DATA ls_field LIKE LINE OF ls_data-dd03p.
    DATA ls_foreign_key LIKE LINE OF ls_data-dd08v.
    DATA lt_cards TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA lv_card TYPE string.
    DATA lv_token TYPE string.
    DATA lv_left TYPE string.
    DATA lv_right TYPE string.
    DATA lv_ddl TYPE string.

    APPEND `[1,0..1];C;1` TO lt_cards.
    APPEND `[0..1,1];1;C` TO lt_cards.
    APPEND `[1,1];1;1` TO lt_cards.
    APPEND `[1..*,1];1;N` TO lt_cards.
    APPEND `[0..*,1];1;CN` TO lt_cards.
    APPEND `[0..*,0..1];C;CN` TO lt_cards.
    APPEND `[0..1,0..1];C;C` TO lt_cards.
    APPEND `[1..*,];N;N` TO lt_cards.
    APPEND `[1..*,0..1];C;N` TO lt_cards.

    CREATE OBJECT lo_format.
    LOOP AT lt_cards INTO lv_card.
      SPLIT lv_card AT ';' INTO lv_token lv_left lv_right.
      CLEAR ls_foreign_key.
      lo_format->parse_cardinality(
        EXPORTING
          iv_token = lv_token
          iv_offset = 0
        CHANGING
          cs_dd08v = ls_foreign_key ).
      cl_abap_unit_assert=>assert_equals(
        exp = lv_left
        act = ls_foreign_key-cardleft ).
      cl_abap_unit_assert=>assert_equals(
        exp = lv_right
        act = ls_foreign_key-card ).

      CLEAR: ls_data, ls_field, ls_foreign_key.
      ls_data-dd02v-tabname = 'ZCARDINALITY'.
      ls_data-dd02v-exclass = '0'.
      ls_data-dd02v-tabclass = 'TRANSP'.
      ls_data-dd02v-contflag = 'C'.
      ls_field-fieldname = 'FIELD'.
      ls_field-adminfield = '0'.
      ls_field-datatype = 'CHAR'.
      ls_field-leng = 1.
      ls_field-inttype = 'C'.
      ls_field-intlen = 2.
      APPEND ls_field TO ls_data-dd03p.
      ls_foreign_key-fieldname = 'FIELD'.
      ls_foreign_key-checktable = 'ZCHECK'.
      ls_foreign_key-cardleft = lv_left.
      ls_foreign_key-card = lv_right.
      APPEND ls_foreign_key TO ls_data-dd08v.
      lv_ddl = lo_format->serialize( ls_data ).
      ls_roundtrip = lo_format->deserialize( lv_ddl ).
      READ TABLE ls_roundtrip-dd08v INTO ls_foreign_key
        WITH KEY fieldname = 'FIELD'.
      cl_abap_unit_assert=>assert_equals(
        exp = 0
        act = sy-subrc ).
      cl_abap_unit_assert=>assert_equals(
        exp = lv_left
        act = ls_foreign_key-cardleft ).
      cl_abap_unit_assert=>assert_equals(
        exp = lv_right
        act = ls_foreign_key-card ).
    ENDLOOP.

    CLEAR: ls_data, ls_field, ls_foreign_key.
    ls_data-dd02v-tabname = 'ZCARDINALITY'.
    ls_data-dd02v-exclass = '0'.
    ls_data-dd02v-tabclass = 'TRANSP'.
    ls_data-dd02v-contflag = 'C'.
    ls_field-fieldname = 'FIELD'.
    ls_field-adminfield = '0'.
    ls_field-datatype = 'CHAR'.
    ls_field-leng = 1.
    ls_field-inttype = 'C'.
    ls_field-intlen = 2.
    APPEND ls_field TO ls_data-dd03p.
    ls_foreign_key-fieldname = 'FIELD'.
    ls_foreign_key-checktable = 'ZCHECK'.
    ls_foreign_key-cardleft = '1'.
    APPEND ls_foreign_key TO ls_data-dd08v.
    lv_ddl = lo_format->serialize( ls_data ).
    cl_abap_unit_assert=>assert_not_initial( lv_ddl ).
    ls_roundtrip = lo_format->deserialize( lv_ddl ).
    READ TABLE ls_roundtrip-dd08v INTO ls_foreign_key
      WITH KEY fieldname = 'FIELD'.
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = sy-subrc ).
    cl_abap_unit_assert=>assert_initial( ls_foreign_key-cardleft ).
    cl_abap_unit_assert=>assert_initial( ls_foreign_key-card ).

    CLEAR ls_data-dd08v.
    APPEND ls_foreign_key TO ls_data-dd08v.
    lv_ddl = lo_format->serialize( ls_data ).
    ls_roundtrip = lo_format->deserialize( lv_ddl ).
    READ TABLE ls_roundtrip-dd08v INTO ls_foreign_key
      WITH KEY fieldname = 'FIELD'.
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = sy-subrc ).

  ENDMETHOD.

  METHOD invalid_ddl.

    DATA lo_format TYPE REF TO zcl_abapgit_object_tabl_ddl.
    DATA lx_error TYPE REF TO zcx_abapgit_exception.

    CREATE OBJECT lo_format.

    TRY.
        lo_format->deserialize( `define view zbad { }` ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>assert_bound( lx_error ).
        cl_abap_unit_assert=>assert_text_matches(
          pattern = `TABL DDL parse error.*expected TABLE`
          text = lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
