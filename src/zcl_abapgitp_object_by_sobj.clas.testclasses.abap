CLASS lcl_test_bridge DEFINITION INHERITING FROM lcl_tlogo_bridge.
  PUBLIC SECTION.
    METHODS expose_where_clause
      IMPORTING iv_table_name           TYPE sobj_name
      RETURNING VALUE(rv_where_on_keys) TYPE string.

    METHODS constructor
      IMPORTING iv_object      TYPE trobjtype
                iv_object_name LIKE mv_object_name
      RAISING   lcx_obj_exception.

ENDCLASS.

CLASS lcl_test_bridge IMPLEMENTATION.

  METHOD expose_where_clause.
    rv_where_on_keys = get_where_clause( iv_table_name ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor(
        iv_object         = iv_object
        iv_object_name    = iv_object_name ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_catalog DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS build FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_catalog IMPLEMENTATION.

  METHOD build.

    CONSTANTS: lc_tab               TYPE objsl-tobj_name VALUE '/IWBEP/I_SBD_GA',
               lc_type_p_max_length TYPE i VALUE 16.

    DATA: lt_catalog TYPE lif_external_object_container=>ty_t_component.

    FIELD-SYMBOLS: <ls_catalog> LIKE LINE OF lt_catalog.


* make sure the database table exists in system
    cl_abap_structdescr=>describe_by_name(
      EXPORTING
        p_name         = lc_tab
      EXCEPTIONS
        type_not_found = 1 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lt_catalog = lcl_catalog=>build( lc_tab ).

    LOOP AT lt_catalog ASSIGNING <ls_catalog> WHERE type_kind = cl_abap_typedescr=>typekind_packed.
      cl_abap_unit_assert=>assert_number_between(
        lower  = 1
        upper  = lc_type_p_max_length
        number = <ls_catalog>-length ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_acgr DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS setup RAISING cx_static_check.

    METHODS where_clause_client FOR TESTING RAISING cx_static_check.
    METHODS where_clause_language FOR TESTING RAISING cx_static_check.

    DATA mo_bridge TYPE REF TO lcl_test_bridge.
ENDCLASS.

CLASS ltcl_acgr IMPLEMENTATION.

  METHOD where_clause_client.
    cl_abap_unit_assert=>assert_equals(
      msg = 'Where clause not expected'
      exp = |MANDT = '{ sy-mandt }' AND AGR_NAME = 'ZOBJUT_TEST_DUMMY'|
      act = mo_bridge->expose_where_clause( 'AGR_DEFINE' ) ).
  ENDMETHOD.

  METHOD where_clause_language.
    cl_abap_unit_assert=>assert_equals(
      msg = 'Where clause not expected'
      exp = |MANDT = '{ sy-mandt }' AND AGR_NAME = 'ZOBJUT_TEST_DUMMY' AND SPRAS = '{ sy-langu }'|
      act = mo_bridge->expose_where_clause( 'AGR_HIERT' ) ).
  ENDMETHOD.

  METHOD setup.
    CREATE OBJECT mo_bridge
      EXPORTING
        iv_object      = 'ACGR'
        iv_object_name = 'ZOBJUT_TEST_DUMMY'.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_adso DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS where_clause_one_value_field    FOR TESTING RAISING cx_static_check.
    METHODS where_clause_2_value_fields_st  FOR TESTING RAISING cx_static_check.
    METHODS setup RAISING cx_static_check.

    DATA mo_bridge TYPE REF TO lcl_test_bridge.
ENDCLASS.


CLASS ltcl_adso IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_bridge
      EXPORTING
        iv_object      = 'ADSO'
        iv_object_name = 'ZOBJUT_TEST_DUMMY'.
  ENDMETHOD.

  METHOD where_clause_one_value_field.
    cl_abap_unit_assert=>assert_equals( msg = 'Where clause not expected'
                                        exp = |ADSONM = 'ZOBJUT_TEST_DUMMY' AND OBJVERS = 'A'|
                                        act = mo_bridge->expose_where_clause( 'RSOADSO' ) ).

  ENDMETHOD.

  METHOD where_clause_2_value_fields_st.
    cl_abap_unit_assert=>assert_equals( msg = 'Where clause not expected'
                                        exp = |TLOGO = 'ADSO' AND OBJVERS = 'A' AND OBJNM = 'ZOBJUT_TEST_DUMMY'|
                                        act = mo_bridge->expose_where_clause( 'RSOOBJXREF' ) ).

  ENDMETHOD.
ENDCLASS.

CLASS ltcl_bmsm DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS where_clause_values_everywhere  FOR TESTING RAISING cx_static_check.
    METHODS setup RAISING cx_static_check.

    DATA mo_bridge TYPE REF TO lcl_test_bridge.
ENDCLASS.


CLASS ltcl_bmsm IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_bridge
      EXPORTING
        iv_object      = 'BMSM'
        iv_object_name = 'ZOBJUT_TEST_DUMMY'.

  ENDMETHOD.

  METHOD where_clause_values_everywhere.
    cl_abap_unit_assert=>assert_equals(
      msg = 'Where clause not expected'
      exp = |PARENT_TYP = 'T' AND PARENT_OBJ = 'ZOBJUT_TEST_DUMMY' AND AS4LOCAL = 'A' AND MODEL_TYP = 'PM'|
      act = mo_bridge->expose_where_clause( 'DF40D' ) ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_bobf DEFINITION FINAL FOR TESTING
  INHERITING FROM zcl_abapgitp_object_by_sobj
  DURATION MEDIUM
  RISK LEVEL DANGEROUS.

  PUBLIC SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    CLASS-DATA go_bridge TYPE REF TO lcl_test_bridge.

    CLASS-METHODS class_setup RAISING cx_static_check.
    CLASS-METHODS class_teardown RAISING cx_static_check.

* TYPE /bobf/conf_key does not exist in old ABAP systems
    TYPES: ty_conf_key TYPE x LENGTH 16.

    CLASS-DATA    gv_ut_bo_key     TYPE ty_conf_key.
    CLASS-DATA    gv_root_node_key TYPE ty_conf_key.

    METHODS:
      t001_import_bo            FOR TESTING RAISING cx_static_check,
      t002_check_existence      FOR TESTING RAISING cx_static_check,
      t100_use_imported_object  FOR TESTING RAISING cx_static_check,
      t150_export_object        FOR TESTING RAISING cx_static_check,
      t200_delete_object        FOR TESTING RAISING cx_static_check.

    METHODS get_bobf_container
      RETURNING VALUE(ro_container) TYPE REF TO lcl_abapgit_xml_container
      RAISING   cx_static_check.

    METHODS get_bopf_persisted_string
      RETURNING VALUE(rv_xml) TYPE string.
ENDCLASS.


CLASS ltcl_bobf IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    me->set_item( iv_obj_type = 'BOBF'
                  iv_obj_name = 'ZABAPGIT_UNITTEST' ).
  ENDMETHOD.

  METHOD class_setup.
    TRY.
        CREATE OBJECT go_bridge
          EXPORTING
            iv_object      = 'BOBF'
            iv_object_name = 'ZABAPGIT_UNITTEST'.
      CATCH cx_root.
        cl_abap_unit_assert=>fail(
          msg   = 'BOPF not available in this system. Test cannot be executed'
          level = cl_aunit_assert=>tolerable
          quit  = cl_aunit_assert=>class ).
    ENDTRY.

*    purge existing object if it exists from previous execution
    go_bridge->delete_object_on_db( ).

*    create ddic-structures which are needed by the BOPF runtime lateron
    DATA: ls_dd02v     TYPE dd02v,
          lt_dd03p     TYPE dd03ptab,
          ls_dd03p     LIKE LINE OF lt_dd03p,
          lv_ddobjname TYPE ddobjname.

*    data structure ZAGUT_S_ROOT_D
    CLEAR: ls_dd02v, lt_dd03p, ls_dd03p.
    lv_ddobjname = 'ZAGUT_S_ROOT_D'.

    ls_dd02v-tabname    = lv_ddobjname.
    ls_dd02v-tabclass   = 'INTTAB'.

    CLEAR ls_dd03p.
    ls_dd03p-tabname    = lv_ddobjname.
    ls_dd03p-fieldname  = 'INT_NUMBER'.
    ls_dd03p-position   = 1.
    ls_dd03p-rollname   = 'INT4'.
    INSERT ls_dd03p INTO TABLE lt_dd03p.

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name      = lv_ddobjname " Name of the Table to be Written
        dd02v_wa  = ls_dd02v
      TABLES
        dd03p_tab = lt_dd03p    " Table Fields
      EXCEPTIONS
        OTHERS    = 6.
    ASSERT sy-subrc = 0.

    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
      EXPORTING
        name   = lv_ddobjname    " Name of the Table to be Activated
      EXCEPTIONS
        OTHERS = 3.
    ASSERT sy-subrc = 0.

*    combined data structure ZAGUT_S_ROOT
    CLEAR: ls_dd02v, lt_dd03p, ls_dd03p.
    lv_ddobjname = 'ZAGUT_S_ROOT'.

    ls_dd02v-tabname    = lv_ddobjname.
    ls_dd02v-tabclass   = 'INTTAB'.

    CLEAR ls_dd03p.
    ls_dd03p-tabname    = lv_ddobjname.
    ls_dd03p-fieldname  = '.INCLUDE'.
    ls_dd03p-position   = 1.
    ls_dd03p-precfield  = '/BOBF/S_FRW_KEY_INCL'.
    INSERT ls_dd03p INTO TABLE lt_dd03p.

    ls_dd03p-tabname    = lv_ddobjname.
    ls_dd03p-fieldname  = '.INCLUDE'.
    ls_dd03p-position   = 2.
    ls_dd03p-precfield  = 'ZAGUT_S_ROOT_D'.
    ls_dd03p-groupname  = 'NODE_DATA'.
    INSERT ls_dd03p INTO TABLE lt_dd03p.

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name      = lv_ddobjname " Name of the Table to be Written
        dd02v_wa  = ls_dd02v
      TABLES
        dd03p_tab = lt_dd03p    " Table Fields
      EXCEPTIONS
        OTHERS    = 6.
    ASSERT sy-subrc = 0.

    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
      EXPORTING
        name   = lv_ddobjname    " Name of the Table to be Activated
      EXCEPTIONS
        OTHERS = 3.
    ASSERT sy-subrc = 0.

*    DB table ZAGUT_D_ROOT
    CLEAR: ls_dd02v, lt_dd03p, ls_dd03p.
    lv_ddobjname = 'ZAGUT_D_ROOT'.

    ls_dd02v-tabname    = lv_ddobjname.
    ls_dd02v-tabclass   = 'TRANSP'.
    ls_dd02v-clidep     = abap_true.
    ls_dd02v-contflag   = 'A'.
    ls_dd02v-exclass    = '3'.

    DATA ls_dd09l TYPE dd09l.
    CLEAR ls_dd09l.
    ls_dd09l-tabname    = lv_ddobjname.
    ls_dd09l-tabkat     = 4.
    ls_dd09l-tabart     = 'APPL1'.

    CLEAR ls_dd03p.
    ls_dd03p-tabname    = lv_ddobjname.
    ls_dd03p-fieldname  = 'MANDT'.
    ls_dd03p-position   = 1.
    ls_dd03p-rollname   = 'MANDT'.
    ls_dd03p-keyflag    = abap_true.
    INSERT ls_dd03p INTO TABLE lt_dd03p.

    CLEAR ls_dd03p.
    ls_dd03p-tabname    = lv_ddobjname.
    ls_dd03p-fieldname  = 'DB_KEY'.
    ls_dd03p-position   = 2.
    ls_dd03p-rollname   = '/BOBF/CONF_KEY'.
    ls_dd03p-keyflag    = abap_true.
    INSERT ls_dd03p INTO TABLE lt_dd03p.

    ls_dd03p-tabname    = lv_ddobjname.
    ls_dd03p-fieldname  = '.INCLUDE'.
    ls_dd03p-position   = 3.
    ls_dd03p-precfield  = 'ZAGUT_S_ROOT_D'.
    ls_dd03p-groupname  = 'NODE_DATA'.
    INSERT ls_dd03p INTO TABLE lt_dd03p.

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name      = lv_ddobjname " Name of the Table to be Written
        dd02v_wa  = ls_dd02v
        dd09l_wa  = ls_dd09l
      TABLES
        dd03p_tab = lt_dd03p    " Table Fields
      EXCEPTIONS
        OTHERS    = 6.
    ASSERT sy-subrc = 0.

    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
      EXPORTING
        name   = lv_ddobjname    " Name of the Table to be Activated
      EXCEPTIONS
        OTHERS = 3.
    ASSERT sy-subrc = 0.

*    combined table type
    DATA ls_dd40v TYPE dd40v.
    lv_ddobjname    = 'ZAGUT_T_ROOT'.
    ls_dd40v-typename = lv_ddobjname.
    ls_dd40v-rowtype = 'ZAGUT_S_ROOT'.
    ls_dd40v-rowkind = 'S'.

    CALL FUNCTION 'DDIF_TTYP_PUT'
      EXPORTING
        name     = lv_ddobjname
        dd40v_wa = ls_dd40v
      EXCEPTIONS
        OTHERS   = 6.
    ASSERT sy-subrc = 0.
    CALL FUNCTION 'DDIF_TTYP_ACTIVATE'
      EXPORTING
        name   = lv_ddobjname
      EXCEPTIONS
        OTHERS = 3.
    ASSERT sy-subrc = 0.
  ENDMETHOD.

  METHOD class_teardown.
    DATA lv_deleted TYPE abap_bool.

    CALL FUNCTION 'DDIF_OBJECT_DELETE'
      EXPORTING
        type          = 'TTYP'
        name          = 'ZAGUT_T_ROOT'
      IMPORTING
        deleted       = lv_deleted
      EXCEPTIONS
        illegal_input = 1
        no_authority  = 2
        OTHERS        = 3.
    cl_abap_unit_assert=>assert_not_initial( lv_deleted ).


    CALL FUNCTION 'DDIF_OBJECT_DELETE'
      EXPORTING
        type          = 'TABL'
        name          = 'ZAGUT_S_ROOT'
      IMPORTING
        deleted       = lv_deleted
      EXCEPTIONS
        illegal_input = 1
        no_authority  = 2
        OTHERS        = 3.
    cl_abap_unit_assert=>assert_not_initial( lv_deleted ).


    CALL FUNCTION 'DDIF_OBJECT_DELETE'
      EXPORTING
        type          = 'TABL'
        name          = 'ZAGUT_D_ROOT'
      IMPORTING
        deleted       = lv_deleted
      EXCEPTIONS
        illegal_input = 1
        no_authority  = 2
        OTHERS        = 3.
    cl_abap_unit_assert=>assert_not_initial( lv_deleted ).


    CALL FUNCTION 'DDIF_OBJECT_DELETE'
      EXPORTING
        type          = 'TABL'
        name          = 'ZAGUT_S_ROOT_D'
      IMPORTING
        deleted       = lv_deleted
      EXCEPTIONS
        illegal_input = 1
        no_authority  = 2
        OTHERS        = 3.
    cl_abap_unit_assert=>assert_not_initial( lv_deleted ).

  ENDMETHOD.

  METHOD t001_import_bo.
    go_bridge->import_object( me->get_bobf_container( ) ).

    SELECT COUNT(*) FROM ('/BOBF/ACT_CONF') WHERE name = 'ZABAPGIT_UNITTEST'.
    cl_abap_unit_assert=>assert_equals(
      msg = 'Database content of /BOBF/ACT_CONF found deviated'
      exp = 7
      act = sy-dbcnt ).

*    check whether the BOPF designtime accepted the imported object
* TODO, COMMENTED, DOWNPORT
*    DATA lo_conf TYPE REF TO /bobf/if_frw_configuration.
*
*    gv_ut_bo_key = /bobf/cl_frw_factory=>query_bo( iv_bo_name = 'ZABAPGIT_UNITTEST' ).
*    cl_abap_unit_assert=>assert_not_initial( gv_ut_bo_key ).
*
*    lo_conf = /bobf/cl_frw_factory=>get_configuration( gv_ut_bo_key ).
*    cl_abap_unit_assert=>assert_not_initial( lo_conf ).
*
*    gv_root_node_key = lo_conf->query_node( iv_node_name = 'ROOT' ).
*    cl_abap_unit_assert=>assert_not_initial( gv_root_node_key ).
  ENDMETHOD.

  METHOD t002_check_existence.
    cl_abap_unit_assert=>assert_equals(
      msg = 'Imported object type BOBF, ZABAPGIT_UNITTEST does not exist'
      exp = abap_true
      act = go_bridge->instance_exists( ) ).
  ENDMETHOD.

  METHOD t150_export_object.
    DATA lo_xml_container   TYPE REF TO lcl_abapgit_xml_container.
    DATA lv_xml_string      TYPE string.
    DATA lo_ixml            TYPE REF TO if_ixml.
    DATA lo_document        TYPE REF TO if_ixml_document.
    DATA lo_renderer        TYPE REF TO if_ixml_renderer.

    CREATE OBJECT lo_xml_container.
    lo_xml_container->set_xml_output( zcl_abapgitp_xml_factory=>create_xml_output( ) ).

    go_bridge->export_object( lo_xml_container ).

    lo_ixml = cl_ixml=>create( ).
    lo_document = lo_ixml->create_document( ).
    lo_document->append_child( lo_xml_container->mo_xml_input->get_raw( ) ). "Current design does not allow accessing the DOM of a created xml
    "(lcl_xml_output) and in- and out are two different instances...

    lo_renderer = lo_ixml->create_renderer(
      document = lo_document
      ostream  = lo_ixml->create_stream_factory( )->create_ostream_cstring( lv_xml_string ) ).

*    we should not compare the complete string, as the root element contains e. g. the encoding and version.
*    start with the first _- which is the escaped slash of the first BOPF table
    cl_abap_unit_assert=>assert_equals(
      msg = |Rendered string and input string don't match|
      exp = substring_after( val = me->get_bopf_persisted_string( ) sub = '_-' )
      act = substring_after( val = lv_xml_string                    sub = '_-' ) ).

  ENDMETHOD.

  METHOD t100_use_imported_object.

* TODO, COMMENTED, DOWNPORT
*    DATA lo_mo_serv_mgr TYPE REF TO /bobf/if_tra_service_manager.
*    lo_mo_serv_mgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( gv_ut_bo_key ).
*
*    DATA lt_modification TYPE /bobf/t_frw_modification.
*    DATA ls_modification LIKE LINE OF lt_modification.
*
*    ls_modification-node            = gv_root_node_key.
*    ls_modification-change_mode     = /bobf/if_frw_c=>sc_modify_create.
*    ls_modification-key             = /bobf/cl_frw_factory=>get_new_key( ).
*    INSERT ls_modification INTO TABLE lt_modification.
*
*    lo_mo_serv_mgr->modify(
*        lt_modification    " Changes
*    ).
*
*    DATA lt_key TYPE /bobf/t_frw_key.
*    DATA ls_key LIKE LINE OF lt_key.
*    DATA lt_failed_ley LIKE lt_key.
*
*    ls_key-key = ls_modification-key.
*    INSERT ls_key INTO TABLE lt_key.
*
*    DATA lr_root_node_tab TYPE REF TO data.
*    CREATE DATA lr_root_node_tab TYPE ('ZAGUT_T_ROOT').
*    FIELD-SYMBOLS <lt_root> TYPE ANY TABLE.
*    ASSIGN lr_root_node_tab->* TO <lt_root>.
*
*    lo_mo_serv_mgr->retrieve(
*      EXPORTING
*        iv_node_key             = gv_root_node_key    " Node
*        it_key                  = lt_key    " Key Table
*      IMPORTING
*        et_data                 = <lt_root>
*        et_failed_key           = lt_failed_ley " Key Table
*    ).
*
*    cl_aunit_assert=>assert_not_initial(
*            act              = <lt_root>    " Actual Data Object
*            msg              = |The BO instance which has been created previously could not be retrieved|
*        ).
*
*    cl_aunit_assert=>assert_initial(
*        act              = lt_failed_ley    " Actual Data Object
*        msg              = |The BO instance which has been created previously could not be retrieved|
*    ).
*
**    Save the transaction in order to be able to see the DB access in the log ;)
*    DATA lv_save_rejected TYPE abap_bool.
*    /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( )->save(
*      IMPORTING
*        ev_rejected            = lv_save_rejected    " Data element for domain BOOLE: TRUE (='X') and FALSE (=' ')
*    ).
*
*    cl_aunit_assert=>assert_initial(
*        act              = lv_save_rejected    " Actual Data Object
*        msg              = |The BO instance could not be persisted|
*    ).
*
*    DATA lv_key_from_db TYPE /bobf/conf_key.
*    CLEAR lv_key_from_db.
*    SELECT SINGLE db_key FROM ('ZAGUT_D_ROOT') INTO lv_key_from_db WHERE db_key = ls_key-key.
*    cl_aunit_assert=>assert_not_initial(
*               act              = <lt_root>    " Actual Data Object
*               msg              = |The BO instance which has been saved previously could not be SELECTed|
*           ).

  ENDMETHOD.

  METHOD t200_delete_object.
    go_bridge->delete_object_on_db( ).

    SELECT COUNT(*) FROM ('/BOBF/ACT_CONF') WHERE name = 'ZABAPGIT_UNITTEST'.
    cl_abap_unit_assert=>assert_equals(
      msg = 'Database content of /BOBF/ACT_CONF found deviated'
      exp = 0
      act = sy-dbcnt ).

  ENDMETHOD.

  METHOD get_bobf_container.

    CREATE OBJECT ro_container.

    ro_container->set_xml_input( zcl_abapgitp_xml_factory=>create_xml_input( iv_xml = me->get_bopf_persisted_string( ) ) ).
    ro_container->set_xml_output( zcl_abapgitp_xml_factory=>create_xml_output( ) ).

  ENDMETHOD.

  METHOD get_bopf_persisted_string.

    rv_xml = rv_xml && |<?xml version="1.0" encoding="utf-8"?><abapGit version="v1.0.0" serializer="ZCL_ABAPGIT_OBJECT_BY_SOBJ" serializer_version="1.0">|.
    rv_xml = rv_xml && |<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0"><asx:values><_-BOBF_-ACT_CONF>|.
    rv_xml = rv_xml && |<_-BOBF_-ACT_CONF><NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>00000</VERSION>|.
    rv_xml = rv_xml && |<CONF_KEY>AAwphY1KHuW5jj/mrFipDA==</CONF_KEY><BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY>|.
    rv_xml = rv_xml && |<ACT_KEY>AAwphY1KHuW5jj/mrFiJDA==</ACT_KEY><NODE_CAT_KEY>AAwphY1KHuW5jj/mrFfpDA==</NODE_CAT_KEY>|.
    rv_xml = rv_xml && |<CREATE_USER>DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME><CHANGE_USER>|.
    rv_xml = rv_xml && |DEVELOPER</CHANGE_USER><CHANGE_TIME>20160307143702</CHANGE_TIME></_-BOBF_-ACT_CONF>|.
    rv_xml = rv_xml && |<_-BOBF_-ACT_CONF><NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>00000</VERSION>|.
    rv_xml = rv_xml && |<CONF_KEY>AAwphY1KHuW5jj/mrFjpDA==</CONF_KEY><BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY>|.
    rv_xml = rv_xml && |<ACT_KEY>AAwphY1KHuW5jj/mrFjJDA==</ACT_KEY><NODE_CAT_KEY>AAwphY1KHuW5jj/mrFfpDA==</NODE_CAT_KEY>|.
    rv_xml = rv_xml && |<CREATE_USER>DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME><CHANGE_USER>|.
    rv_xml = rv_xml && |DEVELOPER</CHANGE_USER><CHANGE_TIME>20160307143702</CHANGE_TIME></_-BOBF_-ACT_CONF>|.
    rv_xml = rv_xml && |<_-BOBF_-ACT_CONF><NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>00000</VERSION>|.
    rv_xml = rv_xml && |<CONF_KEY>AAwphY1KHuW5jj/mrFlpDA==</CONF_KEY><BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY>|.
    rv_xml = rv_xml && |<ACT_KEY>AAwphY1KHuW5jj/mrFlJDA==</ACT_KEY><NODE_CAT_KEY>AAwphY1KHuW5jj/mrFfpDA==</NODE_CAT_KEY>|.
    rv_xml = rv_xml && |<CREATE_USER>DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME><CHANGE_USER>|.
    rv_xml = rv_xml && |DEVELOPER</CHANGE_USER><CHANGE_TIME>20160307143702</CHANGE_TIME></_-BOBF_-ACT_CONF>|.
    rv_xml = rv_xml && |<_-BOBF_-ACT_CONF><NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>00000</VERSION>|.
    rv_xml = rv_xml && |<CONF_KEY>AAwphY1KHuW5jj/mrFmpDA==</CONF_KEY><BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY>|.
    rv_xml = rv_xml && |<ACT_KEY>AAwphY1KHuW5jj/mrFmJDA==</ACT_KEY><NODE_CAT_KEY>AAwphY1KHuW5jj/mrFfpDA==</NODE_CAT_KEY>|.
    rv_xml = rv_xml && |<CREATE_USER>DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME><CHANGE_USER>|.
    rv_xml = rv_xml && |DEVELOPER</CHANGE_USER><CHANGE_TIME>20160307143702</CHANGE_TIME></_-BOBF_-ACT_CONF>|.
    rv_xml = rv_xml && |<_-BOBF_-ACT_CONF><NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>00000</VERSION>|.
    rv_xml = rv_xml && |<CONF_KEY>AAwphY1KHuW5jj/mrFnpDA==</CONF_KEY><BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY>|.
    rv_xml = rv_xml && |<ACT_KEY>AAwphY1KHuW5jj/mrFnJDA==</ACT_KEY><NODE_CAT_KEY>AAwphY1KHuW5jj/mrFfpDA==</NODE_CAT_KEY>|.
    rv_xml = rv_xml && |<CREATE_USER>DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME><CHANGE_USER>|.
    rv_xml = rv_xml && |DEVELOPER</CHANGE_USER><CHANGE_TIME>20160307143702</CHANGE_TIME></_-BOBF_-ACT_CONF>|.
    rv_xml = rv_xml && |<_-BOBF_-ACT_CONF><NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>00000</VERSION>|.
    rv_xml = rv_xml && |<CONF_KEY>AAwphY1KHuW5jj/mrFopDA==</CONF_KEY><BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY>|.
    rv_xml = rv_xml && |<ACT_KEY>AAwphY1KHuW5jj/mrFoJDA==</ACT_KEY><NODE_CAT_KEY>AAwphY1KHuW5jj/mrFfpDA==</NODE_CAT_KEY>|.
    rv_xml = rv_xml && |<CREATE_USER>DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME><CHANGE_USER>|.
    rv_xml = rv_xml && |DEVELOPER</CHANGE_USER><CHANGE_TIME>20160307143702</CHANGE_TIME></_-BOBF_-ACT_CONF>|.
    rv_xml = rv_xml && |<_-BOBF_-ACT_CONF><NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>00000</VERSION>|.
    rv_xml = rv_xml && |<CONF_KEY>AAwphY1KHuW5jj/mrFppDA==</CONF_KEY><BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY>|.
    rv_xml = rv_xml && |<ACT_KEY>AAwphY1KHuW5jj/mrFpJDA==</ACT_KEY><NODE_CAT_KEY>AAwphY1KHuW5jj/mrFfpDA==</NODE_CAT_KEY>|.
    rv_xml = rv_xml && |<CREATE_USER>DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME><CHANGE_USER>|.
    rv_xml = rv_xml && |DEVELOPER</CHANGE_USER><CHANGE_TIME>20160307143702</CHANGE_TIME></_-BOBF_-ACT_CONF>|.
    rv_xml = rv_xml && |</_-BOBF_-ACT_CONF><_-BOBF_-ACT_CONF_field_catalog><item><POS>1</POS><NAME>NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>CONF_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>BO_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>6</POS><NAME>ACT_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>NODE_CAT_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>8</POS><NAME>CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>9</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |10</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>11</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-ACT_CONF_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-ACT_LIST><_-BOBF_-ACT_LIST><NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>|.
    rv_xml = rv_xml && |00000</VERSION><ACT_KEY>AAwphY1KHuW5jj/mrFiJDA==</ACT_KEY><BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY>|.
    rv_xml = rv_xml && |<ACT_CAT>3</ACT_CAT><ACT_NAME>LOCK_ROOT</ACT_NAME><ACT_ESR_NAME/><ACT_GENIL_NAME/>|.
    rv_xml = rv_xml && |<ACT_CARDINALITY>2</ACT_CARDINALITY><NODE_KEY>AAwphY1KHuW5jj/mrFfJDA==</NODE_KEY>|.
    rv_xml = rv_xml && |<ACT_CLASS>/BOBF/CL_LIB_A_LOCK</ACT_CLASS><PARAM_DATA_TYPE>/BOBF/S_FRW_LOCK_PARAMETERS</PARAM_DATA_TYPE>|.
    rv_xml = rv_xml && |<ESR_PARAM_DATA_T/><EDIT_MODE>0</EDIT_MODE><EXEC_ONLY_ALL/><MSGID_SUCCESS/><MSGNO_SUCCESS/>|.
    rv_xml = rv_xml && |<MSGID_ERROR/><MSGNO_ERROR/><MSGID_CHECK/><MSGNO_CHECK/><ALIAS_ACTION/><SP_MAPPER_CLASS/>|.
    rv_xml = rv_xml && |<USE_PROXY_TYPE/><PREPARE_IMPL/><TD_CONTAINER/><TD_CONTAINER_VAR/><CHK_ACT_ALSO_INT/>|.
    rv_xml = rv_xml && |<BASE_ACTION_KEY/><EXTENDIBLE/><CREATE_USER>DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME>|.
    rv_xml = rv_xml && |<CHANGE_USER>DEVELOPER</CHANGE_USER><CHANGE_TIME>20160307143702</CHANGE_TIME></_-BOBF_-ACT_LIST>|.
    rv_xml = rv_xml && |<_-BOBF_-ACT_LIST><NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>00000</VERSION>|.
    rv_xml = rv_xml && |<ACT_KEY>AAwphY1KHuW5jj/mrFjJDA==</ACT_KEY><BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY>|.
    rv_xml = rv_xml && |<ACT_CAT>2</ACT_CAT><ACT_NAME>UNLOCK_ROOT</ACT_NAME><ACT_ESR_NAME/><ACT_GENIL_NAME/>|.
    rv_xml = rv_xml && |<ACT_CARDINALITY>2</ACT_CARDINALITY><NODE_KEY>AAwphY1KHuW5jj/mrFfJDA==</NODE_KEY>|.
    rv_xml = rv_xml && |<ACT_CLASS>/BOBF/CL_LIB_A_LOCK</ACT_CLASS><PARAM_DATA_TYPE>/BOBF/S_FRW_LOCK_PARAMETERS</PARAM_DATA_TYPE>|.
    rv_xml = rv_xml && |<ESR_PARAM_DATA_T/><EDIT_MODE>0</EDIT_MODE><EXEC_ONLY_ALL/><MSGID_SUCCESS/><MSGNO_SUCCESS/>|.
    rv_xml = rv_xml && |<MSGID_ERROR/><MSGNO_ERROR/><MSGID_CHECK/><MSGNO_CHECK/><ALIAS_ACTION/><SP_MAPPER_CLASS/>|.
    rv_xml = rv_xml && |<USE_PROXY_TYPE/><PREPARE_IMPL/><TD_CONTAINER/><TD_CONTAINER_VAR/><CHK_ACT_ALSO_INT/>|.
    rv_xml = rv_xml && |<BASE_ACTION_KEY/><EXTENDIBLE/><CREATE_USER>DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME>|.
    rv_xml = rv_xml && |<CHANGE_USER>DEVELOPER</CHANGE_USER><CHANGE_TIME>20160307143702</CHANGE_TIME></_-BOBF_-ACT_LIST>|.
    rv_xml = rv_xml && |<_-BOBF_-ACT_LIST><NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>00000</VERSION>|.
    rv_xml = rv_xml && |<ACT_KEY>AAwphY1KHuW5jj/mrFlJDA==</ACT_KEY><BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY>|.
    rv_xml = rv_xml && |<ACT_CAT>5</ACT_CAT><ACT_NAME>CREATE_ROOT</ACT_NAME><ACT_ESR_NAME/><ACT_GENIL_NAME/>|.
    rv_xml = rv_xml && |<ACT_CARDINALITY>2</ACT_CARDINALITY><NODE_KEY>AAwphY1KHuW5jj/mrFfJDA==</NODE_KEY>|.
    rv_xml = rv_xml && |<ACT_CLASS/><PARAM_DATA_TYPE/><ESR_PARAM_DATA_T/><EDIT_MODE>0</EDIT_MODE><EXEC_ONLY_ALL/>|.
    rv_xml = rv_xml && |<MSGID_SUCCESS/><MSGNO_SUCCESS/><MSGID_ERROR/><MSGNO_ERROR/><MSGID_CHECK/><MSGNO_CHECK/>|.
    rv_xml = rv_xml && |<ALIAS_ACTION/><SP_MAPPER_CLASS/><USE_PROXY_TYPE/><PREPARE_IMPL/><TD_CONTAINER/><TD_CONTAINER_VAR/>|.
    rv_xml = rv_xml && |<CHK_ACT_ALSO_INT/><BASE_ACTION_KEY/><EXTENDIBLE/><CREATE_USER>DEVELOPER</CREATE_USER>|.
    rv_xml = rv_xml && |<CREATE_TIME>20160307143702</CREATE_TIME><CHANGE_USER>DEVELOPER</CHANGE_USER><CHANGE_TIME>|.
    rv_xml = rv_xml && |20160307143702</CHANGE_TIME></_-BOBF_-ACT_LIST><_-BOBF_-ACT_LIST><NAME>ZABAPGIT_UNITTEST</NAME>|.
    rv_xml = rv_xml && |<EXTENSION/><VERSION>00000</VERSION><ACT_KEY>AAwphY1KHuW5jj/mrFmJDA==</ACT_KEY><BO_KEY>|.
    rv_xml = rv_xml && |AAwphY1KHuW5jj/mrFeJDA==</BO_KEY><ACT_CAT>4</ACT_CAT><ACT_NAME>UPDATE_ROOT</ACT_NAME>|.
    rv_xml = rv_xml && |<ACT_ESR_NAME/><ACT_GENIL_NAME/><ACT_CARDINALITY>2</ACT_CARDINALITY><NODE_KEY>AAwphY1KHuW5jj/mrFfJDA==</NODE_KEY>|.
    rv_xml = rv_xml && |<ACT_CLASS/><PARAM_DATA_TYPE/><ESR_PARAM_DATA_T/><EDIT_MODE>0</EDIT_MODE><EXEC_ONLY_ALL/>|.
    rv_xml = rv_xml && |<MSGID_SUCCESS/><MSGNO_SUCCESS/><MSGID_ERROR/><MSGNO_ERROR/><MSGID_CHECK/><MSGNO_CHECK/>|.
    rv_xml = rv_xml && |<ALIAS_ACTION/><SP_MAPPER_CLASS/><USE_PROXY_TYPE/><PREPARE_IMPL/><TD_CONTAINER/><TD_CONTAINER_VAR/>|.
    rv_xml = rv_xml && |<CHK_ACT_ALSO_INT/><BASE_ACTION_KEY/><EXTENDIBLE/><CREATE_USER>DEVELOPER</CREATE_USER>|.
    rv_xml = rv_xml && |<CREATE_TIME>20160307143702</CREATE_TIME><CHANGE_USER>DEVELOPER</CHANGE_USER><CHANGE_TIME>|.
    rv_xml = rv_xml && |20160307143702</CHANGE_TIME></_-BOBF_-ACT_LIST><_-BOBF_-ACT_LIST><NAME>ZABAPGIT_UNITTEST</NAME>|.
    rv_xml = rv_xml && |<EXTENSION/><VERSION>00000</VERSION><ACT_KEY>AAwphY1KHuW5jj/mrFnJDA==</ACT_KEY><BO_KEY>|.
    rv_xml = rv_xml && |AAwphY1KHuW5jj/mrFeJDA==</BO_KEY><ACT_CAT>6</ACT_CAT><ACT_NAME>DELETE_ROOT</ACT_NAME>|.
    rv_xml = rv_xml && |<ACT_ESR_NAME/><ACT_GENIL_NAME/><ACT_CARDINALITY>2</ACT_CARDINALITY><NODE_KEY>AAwphY1KHuW5jj/mrFfJDA==</NODE_KEY>|.
    rv_xml = rv_xml && |<ACT_CLASS/><PARAM_DATA_TYPE/><ESR_PARAM_DATA_T/><EDIT_MODE>0</EDIT_MODE><EXEC_ONLY_ALL/>|.
    rv_xml = rv_xml && |<MSGID_SUCCESS/><MSGNO_SUCCESS/><MSGID_ERROR/><MSGNO_ERROR/><MSGID_CHECK/><MSGNO_CHECK/>|.
    rv_xml = rv_xml && |<ALIAS_ACTION/><SP_MAPPER_CLASS/><USE_PROXY_TYPE/><PREPARE_IMPL/><TD_CONTAINER/><TD_CONTAINER_VAR/>|.
    rv_xml = rv_xml && |<CHK_ACT_ALSO_INT/><BASE_ACTION_KEY/><EXTENDIBLE/><CREATE_USER>DEVELOPER</CREATE_USER>|.
    rv_xml = rv_xml && |<CREATE_TIME>20160307143702</CREATE_TIME><CHANGE_USER>DEVELOPER</CHANGE_USER><CHANGE_TIME>|.
    rv_xml = rv_xml && |20160307143702</CHANGE_TIME></_-BOBF_-ACT_LIST><_-BOBF_-ACT_LIST><NAME>ZABAPGIT_UNITTEST</NAME>|.
    rv_xml = rv_xml && |<EXTENSION/><VERSION>00000</VERSION><ACT_KEY>AAwphY1KHuW5jj/mrFoJDA==</ACT_KEY><BO_KEY>|.
    rv_xml = rv_xml && |AAwphY1KHuW5jj/mrFeJDA==</BO_KEY><ACT_CAT>7</ACT_CAT><ACT_NAME>VALIDATE_ROOT</ACT_NAME>|.
    rv_xml = rv_xml && |<ACT_ESR_NAME/><ACT_GENIL_NAME/><ACT_CARDINALITY>2</ACT_CARDINALITY><NODE_KEY>AAwphY1KHuW5jj/mrFfJDA==</NODE_KEY>|.
    rv_xml = rv_xml && |<ACT_CLASS/><PARAM_DATA_TYPE/><ESR_PARAM_DATA_T/><EDIT_MODE>0</EDIT_MODE><EXEC_ONLY_ALL/>|.
    rv_xml = rv_xml && |<MSGID_SUCCESS/><MSGNO_SUCCESS/><MSGID_ERROR/><MSGNO_ERROR/><MSGID_CHECK/><MSGNO_CHECK/>|.
    rv_xml = rv_xml && |<ALIAS_ACTION/><SP_MAPPER_CLASS/><USE_PROXY_TYPE/><PREPARE_IMPL/><TD_CONTAINER/><TD_CONTAINER_VAR/>|.
    rv_xml = rv_xml && |<CHK_ACT_ALSO_INT/><BASE_ACTION_KEY/><EXTENDIBLE/><CREATE_USER>DEVELOPER</CREATE_USER>|.
    rv_xml = rv_xml && |<CREATE_TIME>20160307143702</CREATE_TIME><CHANGE_USER>DEVELOPER</CHANGE_USER><CHANGE_TIME>|.
    rv_xml = rv_xml && |20160307143702</CHANGE_TIME></_-BOBF_-ACT_LIST><_-BOBF_-ACT_LIST><NAME>ZABAPGIT_UNITTEST</NAME>|.
    rv_xml = rv_xml && |<EXTENSION/><VERSION>00000</VERSION><ACT_KEY>AAwphY1KHuW5jj/mrFpJDA==</ACT_KEY><BO_KEY>|.
    rv_xml = rv_xml && |AAwphY1KHuW5jj/mrFeJDA==</BO_KEY><ACT_CAT>8</ACT_CAT><ACT_NAME>SAVE_ROOT</ACT_NAME>|.
    rv_xml = rv_xml && |<ACT_ESR_NAME/><ACT_GENIL_NAME/><ACT_CARDINALITY>2</ACT_CARDINALITY><NODE_KEY>AAwphY1KHuW5jj/mrFfJDA==</NODE_KEY>|.
    rv_xml = rv_xml && |<ACT_CLASS/><PARAM_DATA_TYPE/><ESR_PARAM_DATA_T/><EDIT_MODE>0</EDIT_MODE><EXEC_ONLY_ALL/>|.
    rv_xml = rv_xml && |<MSGID_SUCCESS/><MSGNO_SUCCESS/><MSGID_ERROR/><MSGNO_ERROR/><MSGID_CHECK/><MSGNO_CHECK/>|.
    rv_xml = rv_xml && |<ALIAS_ACTION/><SP_MAPPER_CLASS/><USE_PROXY_TYPE/><PREPARE_IMPL/><TD_CONTAINER/><TD_CONTAINER_VAR/>|.
    rv_xml = rv_xml && |<CHK_ACT_ALSO_INT/><BASE_ACTION_KEY/><EXTENDIBLE/><CREATE_USER>DEVELOPER</CREATE_USER>|.
    rv_xml = rv_xml && |<CREATE_TIME>20160307143702</CREATE_TIME><CHANGE_USER>DEVELOPER</CHANGE_USER><CHANGE_TIME>|.
    rv_xml = rv_xml && |20160307143702</CHANGE_TIME></_-BOBF_-ACT_LIST></_-BOBF_-ACT_LIST><_-BOBF_-ACT_LIST_field_catalog>|.
    rv_xml = rv_xml && |<item><POS>1</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>3</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS><NAME>ACT_KEY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>5</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>6</POS><NAME>ACT_CAT</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>ACT_NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>8</POS><NAME>ACT_ESR_NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>9</POS><NAME>ACT_GENIL_NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>10</POS><NAME>ACT_CARDINALITY</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>11</POS><NAME>NODE_KEY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |12</POS><NAME>ACT_CLASS</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>13</POS><NAME>PARAM_DATA_TYPE</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |14</POS><NAME>ESR_PARAM_DATA_T</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>15</POS><NAME>EDIT_MODE</NAME><TYPE_KIND>b</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>3</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>16</POS><NAME>|.
    rv_xml = rv_xml && |EXEC_ONLY_ALL</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>17</POS><NAME>MSGID_SUCCESS</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>20</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>18</POS><NAME>|.
    rv_xml = rv_xml && |MSGNO_SUCCESS</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>3</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>19</POS><NAME>MSGID_ERROR</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>20</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>20</POS><NAME>|.
    rv_xml = rv_xml && |MSGNO_ERROR</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>3</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>21</POS><NAME>MSGID_CHECK</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>20</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>22</POS><NAME>|.
    rv_xml = rv_xml && |MSGNO_CHECK</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>3</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>23</POS><NAME>ALIAS_ACTION</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>24</POS><NAME>|.
    rv_xml = rv_xml && |SP_MAPPER_CLASS</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>25</POS><NAME>USE_PROXY_TYPE</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>26</POS><NAME>|.
    rv_xml = rv_xml && |PREPARE_IMPL</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>27</POS><NAME>TD_CONTAINER</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>28</POS><NAME>|.
    rv_xml = rv_xml && |TD_CONTAINER_VAR</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>29</POS><NAME>CHK_ACT_ALSO_INT</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>30</POS><NAME>|.
    rv_xml = rv_xml && |BASE_ACTION_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>31</POS><NAME>EXTENDIBLE</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>32</POS><NAME>|.
    rv_xml = rv_xml && |CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>33</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>34</POS><NAME>|.
    rv_xml = rv_xml && |CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>35</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-ACT_LIST_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-ACT_LISTT/><_-BOBF_-ACT_LISTT_field_catalog><item><POS>1</POS><NAME>LANGU</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>EXTENSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>ACT_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>6</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>DESCRIPTION</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>40</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-ACT_LISTT_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-DET_CONF/><_-BOBF_-DET_CONF_field_catalog><item><POS>1</POS><NAME>NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>CONF_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>BO_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>6</POS><NAME>EXEC_TIME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>DET_KEY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |8</POS><NAME>NODE_CAT_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>9</POS><NAME>CREATE_USER</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |10</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>11</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |12</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item></_-BOBF_-DET_CONF_field_catalog><_-BOBF_-DET_LIST/><_-BOBF_-DET_LIST_field_catalog>|.
    rv_xml = rv_xml && |<item><POS>1</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>3</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS><NAME>DET_KEY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>5</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>6</POS><NAME>DET_NAME</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>|.
    rv_xml = rv_xml && |NODE_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>8</POS><NAME>DET_CAT</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>9</POS><NAME>DET_CLASS</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |10</POS><NAME>EDIT_MODE</NAME><TYPE_KIND>b</TYPE_KIND><LENGTH>3</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>11</POS><NAME>NO_EXEC_IF_FAIL</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>12</POS>|.
    rv_xml = rv_xml && |<NAME>CHECK_IMPL</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>13</POS><NAME>CHECK_DELTA_IMPL</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>14</POS><NAME>|.
    rv_xml = rv_xml && |TD_CONTAINER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>15</POS><NAME>TD_CONTAINER_VAR</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>16</POS><NAME>|.
    rv_xml = rv_xml && |CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>17</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>18</POS><NAME>|.
    rv_xml = rv_xml && |CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>19</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>20</POS><NAME>|.
    rv_xml = rv_xml && |DET_PATTERN</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>21</POS><NAME>SIMPLIFIED_DT_TAG</NAME><TYPE_KIND>X</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-DET_LIST_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-DET_LISTT/><_-BOBF_-DET_LISTT_field_catalog><item><POS>1</POS><NAME>LANGU</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>EXTENSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>DET_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>6</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>DESCRIPTION</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>40</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-DET_LISTT_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-DET_NET/><_-BOBF_-DET_NET_field_catalog><item><POS>1</POS><NAME>NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>NET_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>BO_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>6</POS><NAME>DET_KEY_FROM</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>DET_KEY_TO</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |8</POS><NAME>CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>9</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |10</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>11</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-DET_NET_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_ASSOC><_-BOBF_-OBM_ASSOC><NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>|.
    rv_xml = rv_xml && |00000</VERSION><ASSOC_KEY>AAwphY1KHuW5jj/mrFgpDA==</ASSOC_KEY><BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY>|.
    rv_xml = rv_xml && |<ASSOC_TYPE>C</ASSOC_TYPE><ASSOC_CAT>V</ASSOC_CAT><ASSOC_NAME>MESSAGE</ASSOC_NAME>|.
    rv_xml = rv_xml && |<ASSOC_ESR_NAME/><ASSOC_GENIL_NAME/><ASSOC_CLASS/><SOURCE_NODE_KEY>AAwphY1KHuW5jj/mrFfJDA==</SOURCE_NODE_KEY>|.
    rv_xml = rv_xml && |<ATTR_NAME_SOURCE/><TARGET_NODE_KEY>AAwphY1KHuW5jj/mrFgJDA==</TARGET_NODE_KEY><PARAM_DATA_TYPE/>|.
    rv_xml = rv_xml && |<CARDINALITY>3</CARDINALITY><QUERY_KEY/><SECKEYNAME/><CHANGE_RESOLVE/><ALIAS_RETRIEVE/>|.
    rv_xml = rv_xml && |<ALIAS_RETRIEVE_S/><ALIAS_DELETE/><ALIAS_DELETE_S/><ESR_PARAM_DATA_T/><ASSOC_RESOLVE>|.
    rv_xml = rv_xml && |0</ASSOC_RESOLVE><USE_PROXY_TYPE/><SP_MAPPER_CLASS/><ASSOC_MODELED/><TD_CONTAINER/>|.
    rv_xml = rv_xml && |<TD_CONTAINER_VAR/><CREATE_USER>DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME>|.
    rv_xml = rv_xml && |<CHANGE_USER>DEVELOPER</CHANGE_USER><CHANGE_TIME>20160307143702</CHANGE_TIME></_-BOBF_-OBM_ASSOC>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_ASSOC><NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>00000</VERSION>|.
    rv_xml = rv_xml && |<ASSOC_KEY>AAwphY1KHuW5jj/mrFhpDA==</ASSOC_KEY><BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY>|.
    rv_xml = rv_xml && |<ASSOC_TYPE>C</ASSOC_TYPE><ASSOC_CAT>L</ASSOC_CAT><ASSOC_NAME>LOCK</ASSOC_NAME><ASSOC_ESR_NAME/>|.
    rv_xml = rv_xml && |<ASSOC_GENIL_NAME/><ASSOC_CLASS/><SOURCE_NODE_KEY>AAwphY1KHuW5jj/mrFfJDA==</SOURCE_NODE_KEY>|.
    rv_xml = rv_xml && |<ATTR_NAME_SOURCE/><TARGET_NODE_KEY>AAwphY1KHuW5jj/mrFhJDA==</TARGET_NODE_KEY><PARAM_DATA_TYPE/>|.
    rv_xml = rv_xml && |<CARDINALITY>2</CARDINALITY><QUERY_KEY/><SECKEYNAME/><CHANGE_RESOLVE/><ALIAS_RETRIEVE/>|.
    rv_xml = rv_xml && |<ALIAS_RETRIEVE_S/><ALIAS_DELETE/><ALIAS_DELETE_S/><ESR_PARAM_DATA_T/><ASSOC_RESOLVE>|.
    rv_xml = rv_xml && |0</ASSOC_RESOLVE><USE_PROXY_TYPE/><SP_MAPPER_CLASS/><ASSOC_MODELED/><TD_CONTAINER/>|.
    rv_xml = rv_xml && |<TD_CONTAINER_VAR/><CREATE_USER>DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME>|.
    rv_xml = rv_xml && |<CHANGE_USER>DEVELOPER</CHANGE_USER><CHANGE_TIME>20160307143702</CHANGE_TIME></_-BOBF_-OBM_ASSOC>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_ASSOC><NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>00000</VERSION>|.
    rv_xml = rv_xml && |<ASSOC_KEY>AAwphY1KHuW5jj/mrFkpDA==</ASSOC_KEY><BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY>|.
    rv_xml = rv_xml && |<ASSOC_TYPE>C</ASSOC_TYPE><ASSOC_CAT>F</ASSOC_CAT><ASSOC_NAME>PROPERTY</ASSOC_NAME>|.
    rv_xml = rv_xml && |<ASSOC_ESR_NAME/><ASSOC_GENIL_NAME/><ASSOC_CLASS/><SOURCE_NODE_KEY>AAwphY1KHuW5jj/mrFfJDA==</SOURCE_NODE_KEY>|.
    rv_xml = rv_xml && |<ATTR_NAME_SOURCE/><TARGET_NODE_KEY>AAwphY1KHuW5jj/mrFkJDA==</TARGET_NODE_KEY><PARAM_DATA_TYPE>|.
    rv_xml = rv_xml && |/BOBF/S_FRW_C_PROPERTY</PARAM_DATA_TYPE><CARDINALITY>3</CARDINALITY><QUERY_KEY/><SECKEYNAME/>|.
    rv_xml = rv_xml && |<CHANGE_RESOLVE/><ALIAS_RETRIEVE/><ALIAS_RETRIEVE_S/><ALIAS_DELETE/><ALIAS_DELETE_S/>|.
    rv_xml = rv_xml && |<ESR_PARAM_DATA_T/><ASSOC_RESOLVE>0</ASSOC_RESOLVE><USE_PROXY_TYPE/><SP_MAPPER_CLASS/>|.
    rv_xml = rv_xml && |<ASSOC_MODELED/><TD_CONTAINER/><TD_CONTAINER_VAR/><CREATE_USER>DEVELOPER</CREATE_USER>|.
    rv_xml = rv_xml && |<CREATE_TIME>20160307143702</CREATE_TIME><CHANGE_USER>DEVELOPER</CHANGE_USER><CHANGE_TIME>|.
    rv_xml = rv_xml && |20160307143702</CHANGE_TIME></_-BOBF_-OBM_ASSOC><_-BOBF_-OBM_ASSOC><NAME>ZABAPGIT_UNITTEST</NAME>|.
    rv_xml = rv_xml && |<EXTENSION/><VERSION>00000</VERSION><ASSOC_KEY>AAwphY1KHuW5jj/mrFqJDA==</ASSOC_KEY>|.
    rv_xml = rv_xml && |<BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY><ASSOC_TYPE>A</ASSOC_TYPE><ASSOC_CAT>P</ASSOC_CAT>|.
    rv_xml = rv_xml && |<ASSOC_NAME>TO_PARENT</ASSOC_NAME><ASSOC_ESR_NAME/><ASSOC_GENIL_NAME/><ASSOC_CLASS/>|.
    rv_xml = rv_xml && |<SOURCE_NODE_KEY>AAwphY1KHuW5jj/mrFgJDA==</SOURCE_NODE_KEY><ATTR_NAME_SOURCE/><TARGET_NODE_KEY>|.
    rv_xml = rv_xml && |AAwphY1KHuW5jj/mrFfJDA==</TARGET_NODE_KEY><PARAM_DATA_TYPE/><CARDINALITY>2</CARDINALITY>|.
    rv_xml = rv_xml && |<QUERY_KEY/><SECKEYNAME/><CHANGE_RESOLVE/><ALIAS_RETRIEVE/><ALIAS_RETRIEVE_S/><ALIAS_DELETE/>|.
    rv_xml = rv_xml && |<ALIAS_DELETE_S/><ESR_PARAM_DATA_T/><ASSOC_RESOLVE>1</ASSOC_RESOLVE><USE_PROXY_TYPE/>|.
    rv_xml = rv_xml && |<SP_MAPPER_CLASS/><ASSOC_MODELED/><TD_CONTAINER/><TD_CONTAINER_VAR/><CREATE_USER>|.
    rv_xml = rv_xml && |DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME><CHANGE_USER>DEVELOPER</CHANGE_USER>|.
    rv_xml = rv_xml && |<CHANGE_TIME>20160307143702</CHANGE_TIME></_-BOBF_-OBM_ASSOC><_-BOBF_-OBM_ASSOC><NAME>|.
    rv_xml = rv_xml && |ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>00000</VERSION><ASSOC_KEY>AAwphY1KHuW5jj/mrFqpDA==</ASSOC_KEY>|.
    rv_xml = rv_xml && |<BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY><ASSOC_TYPE>A</ASSOC_TYPE><ASSOC_CAT>P</ASSOC_CAT>|.
    rv_xml = rv_xml && |<ASSOC_NAME>TO_PARENT</ASSOC_NAME><ASSOC_ESR_NAME/><ASSOC_GENIL_NAME/><ASSOC_CLASS/>|.
    rv_xml = rv_xml && |<SOURCE_NODE_KEY>AAwphY1KHuW5jj/mrFhJDA==</SOURCE_NODE_KEY><ATTR_NAME_SOURCE/><TARGET_NODE_KEY>|.
    rv_xml = rv_xml && |AAwphY1KHuW5jj/mrFfJDA==</TARGET_NODE_KEY><PARAM_DATA_TYPE/><CARDINALITY>2</CARDINALITY>|.
    rv_xml = rv_xml && |<QUERY_KEY/><SECKEYNAME/><CHANGE_RESOLVE/><ALIAS_RETRIEVE/><ALIAS_RETRIEVE_S/><ALIAS_DELETE/>|.
    rv_xml = rv_xml && |<ALIAS_DELETE_S/><ESR_PARAM_DATA_T/><ASSOC_RESOLVE>1</ASSOC_RESOLVE><USE_PROXY_TYPE/>|.
    rv_xml = rv_xml && |<SP_MAPPER_CLASS/><ASSOC_MODELED/><TD_CONTAINER/><TD_CONTAINER_VAR/><CREATE_USER>|.
    rv_xml = rv_xml && |DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME><CHANGE_USER>DEVELOPER</CHANGE_USER>|.
    rv_xml = rv_xml && |<CHANGE_TIME>20160307143702</CHANGE_TIME></_-BOBF_-OBM_ASSOC><_-BOBF_-OBM_ASSOC><NAME>|.
    rv_xml = rv_xml && |ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>00000</VERSION><ASSOC_KEY>AAwphY1KHuW5jj/mrFrJDA==</ASSOC_KEY>|.
    rv_xml = rv_xml && |<BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY><ASSOC_TYPE>A</ASSOC_TYPE><ASSOC_CAT>P</ASSOC_CAT>|.
    rv_xml = rv_xml && |<ASSOC_NAME>TO_PARENT</ASSOC_NAME><ASSOC_ESR_NAME/><ASSOC_GENIL_NAME/><ASSOC_CLASS/>|.
    rv_xml = rv_xml && |<SOURCE_NODE_KEY>AAwphY1KHuW5jj/mrFkJDA==</SOURCE_NODE_KEY><ATTR_NAME_SOURCE/><TARGET_NODE_KEY>|.
    rv_xml = rv_xml && |AAwphY1KHuW5jj/mrFfJDA==</TARGET_NODE_KEY><PARAM_DATA_TYPE/><CARDINALITY>2</CARDINALITY>|.
    rv_xml = rv_xml && |<QUERY_KEY/><SECKEYNAME/><CHANGE_RESOLVE/><ALIAS_RETRIEVE/><ALIAS_RETRIEVE_S/><ALIAS_DELETE/>|.
    rv_xml = rv_xml && |<ALIAS_DELETE_S/><ESR_PARAM_DATA_T/><ASSOC_RESOLVE>1</ASSOC_RESOLVE><USE_PROXY_TYPE/>|.
    rv_xml = rv_xml && |<SP_MAPPER_CLASS/><ASSOC_MODELED/><TD_CONTAINER/><TD_CONTAINER_VAR/><CREATE_USER>|.
    rv_xml = rv_xml && |DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME><CHANGE_USER>DEVELOPER</CHANGE_USER>|.
    rv_xml = rv_xml && |<CHANGE_TIME>20160307143702</CHANGE_TIME></_-BOBF_-OBM_ASSOC></_-BOBF_-OBM_ASSOC>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_ASSOC_field_catalog><item><POS>1</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS>|.
    rv_xml = rv_xml && |<NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS>|.
    rv_xml = rv_xml && |<NAME>ASSOC_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>6</POS><NAME>|.
    rv_xml = rv_xml && |ASSOC_TYPE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>7</POS><NAME>ASSOC_CAT</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>8</POS><NAME>ASSOC_NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>9</POS><NAME>ASSOC_ESR_NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>10</POS><NAME>ASSOC_GENIL_NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>29</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>11</POS><NAME>ASSOC_CLASS</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>12</POS><NAME>SOURCE_NODE_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>13</POS><NAME>ATTR_NAME_SOURCE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>14</POS><NAME>TARGET_NODE_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>15</POS><NAME>PARAM_DATA_TYPE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>16</POS><NAME>CARDINALITY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>b</TYPE_KIND><LENGTH>3</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item>|.
    rv_xml = rv_xml && |<POS>17</POS><NAME>QUERY_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>18</POS><NAME>SECKEYNAME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |19</POS><NAME>CHANGE_RESOLVE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>20</POS><NAME>ALIAS_RETRIEVE</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |21</POS><NAME>ALIAS_RETRIEVE_S</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>22</POS><NAME>ALIAS_DELETE</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |23</POS><NAME>ALIAS_DELETE_S</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>24</POS><NAME>ESR_PARAM_DATA_T</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |25</POS><NAME>ASSOC_RESOLVE</NAME><TYPE_KIND>N</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>26</POS><NAME>USE_PROXY_TYPE</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>27</POS>|.
    rv_xml = rv_xml && |<NAME>SP_MAPPER_CLASS</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>28</POS><NAME>ASSOC_MODELED</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>29</POS>|.
    rv_xml = rv_xml && |<NAME>TD_CONTAINER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>30</POS><NAME>TD_CONTAINER_VAR</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>31</POS><NAME>|.
    rv_xml = rv_xml && |CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>32</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>33</POS><NAME>|.
    rv_xml = rv_xml && |CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>34</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_ASSOC_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_ASSOCC/><_-BOBF_-OBM_ASSOCC_field_catalog><item><POS>1</POS><NAME>NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>CONF_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>BO_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>6</POS><NAME>ASSOC_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>NODE_CAT_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>8</POS><NAME>CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>9</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |10</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>11</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_ASSOCC_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_ASSOCT/><_-BOBF_-OBM_ASSOCT_field_catalog><item><POS>1</POS><NAME>LANGU</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>EXTENSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>ASSOC_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>6</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>DESCRIPTION</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>40</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_ASSOCT_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_BO><_-BOBF_-OBM_BO><BO_NAME>ZABAPGIT_UNITTEST</BO_NAME><EXTENSION/><BO_DELETED/>|.
    rv_xml = rv_xml && |<BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY><SUPER_BO_KEY/><EXTENSION_KEY/><BO_GENERATED/>|.
    rv_xml = rv_xml && |<OBJCAT>0</OBJCAT><CUSTOMER_BO>X</CUSTOMER_BO><BO_ESR_NAME/><SUPER_BO_NAME/><CREATE_USER>|.
    rv_xml = rv_xml && |DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME><CHANGE_USER>DEVELOPER</CHANGE_USER>|.
    rv_xml = rv_xml && |<CHANGE_TIME>20160307143702</CHANGE_TIME></_-BOBF_-OBM_BO></_-BOBF_-OBM_BO><_-BOBF_-OBM_BO_field_catalog>|.
    rv_xml = rv_xml && |<item><POS>1</POS><NAME>BO_NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS><NAME>EXTENSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>3</POS><NAME>BO_DELETED</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS><NAME>BO_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>5</POS><NAME>SUPER_BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>6</POS><NAME>EXTENSION_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>7</POS><NAME>BO_GENERATED</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>8</POS><NAME>OBJCAT</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>9</POS>|.
    rv_xml = rv_xml && |<NAME>CUSTOMER_BO</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>10</POS><NAME>BO_ESR_NAME</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>11</POS><NAME>|.
    rv_xml = rv_xml && |SUPER_BO_NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>12</POS><NAME>CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>13</POS><NAME>|.
    rv_xml = rv_xml && |CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>14</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>15</POS><NAME>|.
    rv_xml = rv_xml && |CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item></_-BOBF_-OBM_BO_field_catalog><_-BOBF_-OBM_GROUP/><_-BOBF_-OBM_GROUP_field_catalog>|.
    rv_xml = rv_xml && |<item><POS>1</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>3</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS><NAME>GROUP_KEY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>5</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>6</POS><NAME>GROUP_NAME</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>|.
    rv_xml = rv_xml && |GROUP_CAT</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>8</POS><NAME>NODE_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>9</POS><NAME>STATUS_VARIABLE</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>10</POS><NAME>STA_VAR_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>11</POS><NAME>ACT_KEY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |12</POS><NAME>CHECK_IMMEDIATE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>13</POS><NAME>CREATE_USER</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |14</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>15</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |16</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_GROUP_field_catalog><_-BOBF_-OBM_GROUPC/>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_GROUPC_field_catalog><item><POS>1</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS>|.
    rv_xml = rv_xml && |<NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS>|.
    rv_xml = rv_xml && |<NAME>CONF_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>6</POS><NAME>|.
    rv_xml = rv_xml && |GROUP_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>7</POS><NAME>CONTENT_KEY</NAME><TYPE_KIND>X</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>8</POS><NAME>|.
    rv_xml = rv_xml && |CONTENT_CAT</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>9</POS><NAME>CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>10</POS><NAME>|.
    rv_xml = rv_xml && |CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>11</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>12</POS><NAME>|.
    rv_xml = rv_xml && |CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item></_-BOBF_-OBM_GROUPC_field_catalog><_-BOBF_-OBM_GROUPT/><_-BOBF_-OBM_GROUPT_field_catalog>|.
    rv_xml = rv_xml && |<item><POS>1</POS><NAME>LANGU</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS><NAME>NAME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>3</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS><NAME>VERSION</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>5</POS><NAME>GROUP_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>6</POS><NAME>BO_KEY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |7</POS><NAME>DESCRIPTION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>40</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_GROUPT_field_catalog><_-BOBF_-OBM_MAP/>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_MAP_field_catalog><item><POS>1</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS>|.
    rv_xml = rv_xml && |<NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS>|.
    rv_xml = rv_xml && |<NAME>MAP_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>6</POS><NAME>|.
    rv_xml = rv_xml && |CONTENT_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>7</POS><NAME>CONTENT_CAT</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>8</POS><NAME>HIERARCHY_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>9</POS><NAME>FIELDNAME_EXT</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>10</POS><NAME>FIELDNAME_INT</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>11</POS><NAME>CONV_RULE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>3</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>12</POS><NAME>REDUNDANT_ATTR</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item>|.
    rv_xml = rv_xml && |<POS>13</POS><NAME>CONST_INTERFACE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>14</POS><NAME>READ_REPEATABLE</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item>|.
    rv_xml = rv_xml && |<POS>15</POS><NAME>CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>16</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |17</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>18</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_MAP_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_NCAT><_-BOBF_-OBM_NCAT><NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>|.
    rv_xml = rv_xml && |00000</VERSION><NODE_CAT_KEY>AAwphY1KHuW5jj/mrFfpDA==</NODE_CAT_KEY><BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY>|.
    rv_xml = rv_xml && |<NODE_KEY>AAwphY1KHuW5jj/mrFfJDA==</NODE_KEY><NODE_CAT_NAME>ROOT</NODE_CAT_NAME><STATUS_SCHEMA/>|.
    rv_xml = rv_xml && |<STAT_SCHEMA_KEY/><CREATE_USER>DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME>|.
    rv_xml = rv_xml && |<CHANGE_USER>DEVELOPER</CHANGE_USER><CHANGE_TIME>20160307143702</CHANGE_TIME></_-BOBF_-OBM_NCAT>|.
    rv_xml = rv_xml && |</_-BOBF_-OBM_NCAT><_-BOBF_-OBM_NCAT_field_catalog><item><POS>1</POS><NAME>NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>NODE_CAT_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>|.
    rv_xml = rv_xml && |BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>6</POS><NAME>NODE_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>NODE_CAT_NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>8</POS><NAME>STATUS_SCHEMA</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>9</POS><NAME>STAT_SCHEMA_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>10</POS><NAME>CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>11</POS><NAME>CREATE_TIME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>12</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>13</POS><NAME>CHANGE_TIME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |</_-BOBF_-OBM_NCAT_field_catalog><_-BOBF_-OBM_NCATT/><_-BOBF_-OBM_NCATT_field_catalog>|.
    rv_xml = rv_xml && |<item><POS>1</POS><NAME>LANGU</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS><NAME>NAME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>3</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS><NAME>VERSION</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>5</POS><NAME>NODE_CAT_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>6</POS><NAME>BO_KEY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |7</POS><NAME>DESCRIPTION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>40</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_NCATT_field_catalog><_-BOBF_-OBM_NODE><_-BOBF_-OBM_NODE>|.
    rv_xml = rv_xml && |<NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>00000</VERSION><NODE_KEY>AAwphY1KHuW5jj/mrFfJDA==</NODE_KEY>|.
    rv_xml = rv_xml && |<BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY><OBJ_KEY>AAwphY1KHuW5jj/mrFepDA==</OBJ_KEY>|.
    rv_xml = rv_xml && |<NODE_NAME>ROOT</NODE_NAME><NODE_ESR_NAME/><NODE_ESR_PREFIX/><NODE_GENIL_NAME/><DATA_TYPE>|.
    rv_xml = rv_xml && |ZALICE_S_ROOT</DATA_TYPE><DATA_DATA_TYPE>ZALICE_S_ROOT_D</DATA_DATA_TYPE><DATA_DATA_TYPE_T/>|.
    rv_xml = rv_xml && |<DATA_TABLE_TYPE>ZALICE_T_ROOT</DATA_TABLE_TYPE><GDT_DATA_TYPE/><PRX_DAT_DAT_TYPE/>|.
    rv_xml = rv_xml && |<EXT_INCL_NAME/><EXT_INCL_NAME_T/><DATABASE_TABLE>ZALICE_D_ROOT</DATABASE_TABLE><NODE_TYPE>|.
    rv_xml = rv_xml && |N</NODE_TYPE><NODE_CAT_KEY>AAwphY1KHuW5jj/mrFfpDA==</NODE_CAT_KEY><SP_MAPPER_CLASS/>|.
    rv_xml = rv_xml && |<SP_ID_MAP_CLASS/><BUF_CLASS>/BOBF/CL_BUF_SIMPLE</BUF_CLASS><MAPPER_CLASS/><DELEGATION_CLASS/>|.
    rv_xml = rv_xml && |<LCP_WRAP_CLASS/><LOADABLE>X</LOADABLE><LOCKABLE>X</LOCKABLE><TRANSIENT/><SUBTREE_PROPERTY/>|.
    rv_xml = rv_xml && |<KEY_INHERITED/><USE_PROXY_TYPE/><NODE_CLASS/><ALIAS_RETRIEVE/><ALIAS_RETRIEVE_S/>|.
    rv_xml = rv_xml && |<ALIAS_CREATE_S/><ALIAS_UPDATE_S/><ALIAS_DELETE_S/><KEY_SECKEYNAME/><ROOT_SECKEYNAME/>|.
    rv_xml = rv_xml && |<STA_CHILD_ASSOC/><STA_PARENT_ASSOC/><REF_BO_KEY/><REF_NODE_KEY/><EXTENDIBLE/><AUTH_CHECK_CLASS/>|.
    rv_xml = rv_xml && |<AUTH_CHECK_RELEVANT/><CREATE_USER>DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME>|.
    rv_xml = rv_xml && |<CHANGE_USER>DEVELOPER</CHANGE_USER><CHANGE_TIME>20160307143702</CHANGE_TIME></_-BOBF_-OBM_NODE>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_NODE><NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>00000</VERSION>|.
    rv_xml = rv_xml && |<NODE_KEY>AAwphY1KHuW5jj/mrFgJDA==</NODE_KEY><BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY>|.
    rv_xml = rv_xml && |<OBJ_KEY>AAwphY1KHuW5jj/mrFepDA==</OBJ_KEY><NODE_NAME>ROOT_MESSAGE</NODE_NAME><NODE_ESR_NAME/>|.
    rv_xml = rv_xml && |<NODE_ESR_PREFIX/><NODE_GENIL_NAME/><DATA_TYPE>/BOBF/S_FRW_MESSAGE_K</DATA_TYPE><DATA_DATA_TYPE>|.
    rv_xml = rv_xml && |/BOBF/S_FRW_MESSAGE_D</DATA_DATA_TYPE><DATA_DATA_TYPE_T/><DATA_TABLE_TYPE>/BOBF/T_FRW_MESSAGE_K</DATA_TABLE_TYPE>|.
    rv_xml = rv_xml && |<GDT_DATA_TYPE/><PRX_DAT_DAT_TYPE/><EXT_INCL_NAME/><EXT_INCL_NAME_T/><DATABASE_TABLE/>|.
    rv_xml = rv_xml && |<NODE_TYPE>V</NODE_TYPE><NODE_CAT_KEY/><SP_MAPPER_CLASS/><SP_ID_MAP_CLASS/><BUF_CLASS>|.
    rv_xml = rv_xml && |/BOBF/CL_LIB_B_MESSAGE</BUF_CLASS><MAPPER_CLASS/><DELEGATION_CLASS/><LCP_WRAP_CLASS/>|.
    rv_xml = rv_xml && |<LOADABLE/><LOCKABLE/><TRANSIENT>X</TRANSIENT><SUBTREE_PROPERTY/><KEY_INHERITED/>|.
    rv_xml = rv_xml && |<USE_PROXY_TYPE/><NODE_CLASS/><ALIAS_RETRIEVE/><ALIAS_RETRIEVE_S/><ALIAS_CREATE_S/>|.
    rv_xml = rv_xml && |<ALIAS_UPDATE_S/><ALIAS_DELETE_S/><KEY_SECKEYNAME/><ROOT_SECKEYNAME/><STA_CHILD_ASSOC/>|.
    rv_xml = rv_xml && |<STA_PARENT_ASSOC/><REF_BO_KEY/><REF_NODE_KEY/><EXTENDIBLE/><AUTH_CHECK_CLASS/><AUTH_CHECK_RELEVANT/>|.
    rv_xml = rv_xml && |<CREATE_USER>DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME><CHANGE_USER>|.
    rv_xml = rv_xml && |DEVELOPER</CHANGE_USER><CHANGE_TIME>20160307143702</CHANGE_TIME></_-BOBF_-OBM_NODE>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_NODE><NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>00000</VERSION>|.
    rv_xml = rv_xml && |<NODE_KEY>AAwphY1KHuW5jj/mrFhJDA==</NODE_KEY><BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY>|.
    rv_xml = rv_xml && |<OBJ_KEY>AAwphY1KHuW5jj/mrFepDA==</OBJ_KEY><NODE_NAME>ROOT_LOCK</NODE_NAME><NODE_ESR_NAME/>|.
    rv_xml = rv_xml && |<NODE_ESR_PREFIX/><NODE_GENIL_NAME/><DATA_TYPE>/BOBF/S_FRW_LOCK_NODE</DATA_TYPE><DATA_DATA_TYPE>|.
    rv_xml = rv_xml && |/BOBF/S_FRW_LOCK_NODE_DATA</DATA_DATA_TYPE><DATA_DATA_TYPE_T/><DATA_TABLE_TYPE>/BOBF/T_FRW_LOCK_NODE</DATA_TABLE_TYPE>|.
    rv_xml = rv_xml && |<GDT_DATA_TYPE/><PRX_DAT_DAT_TYPE/><EXT_INCL_NAME/><EXT_INCL_NAME_T/><DATABASE_TABLE/>|.
    rv_xml = rv_xml && |<NODE_TYPE>L</NODE_TYPE><NODE_CAT_KEY/><SP_MAPPER_CLASS/><SP_ID_MAP_CLASS/><BUF_CLASS>|.
    rv_xml = rv_xml && |/BOBF/CL_LIB_B_LOCK</BUF_CLASS><MAPPER_CLASS/><DELEGATION_CLASS/><LCP_WRAP_CLASS/>|.
    rv_xml = rv_xml && |<LOADABLE/><LOCKABLE/><TRANSIENT>X</TRANSIENT><SUBTREE_PROPERTY/><KEY_INHERITED/>|.
    rv_xml = rv_xml && |<USE_PROXY_TYPE/><NODE_CLASS/><ALIAS_RETRIEVE/><ALIAS_RETRIEVE_S/><ALIAS_CREATE_S/>|.
    rv_xml = rv_xml && |<ALIAS_UPDATE_S/><ALIAS_DELETE_S/><KEY_SECKEYNAME/><ROOT_SECKEYNAME/><STA_CHILD_ASSOC/>|.
    rv_xml = rv_xml && |<STA_PARENT_ASSOC/><REF_BO_KEY/><REF_NODE_KEY/><EXTENDIBLE/><AUTH_CHECK_CLASS/><AUTH_CHECK_RELEVANT/>|.
    rv_xml = rv_xml && |<CREATE_USER>DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME><CHANGE_USER>|.
    rv_xml = rv_xml && |DEVELOPER</CHANGE_USER><CHANGE_TIME>20160307143702</CHANGE_TIME></_-BOBF_-OBM_NODE>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_NODE><NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>00000</VERSION>|.
    rv_xml = rv_xml && |<NODE_KEY>AAwphY1KHuW5jj/mrFkJDA==</NODE_KEY><BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY>|.
    rv_xml = rv_xml && |<OBJ_KEY>AAwphY1KHuW5jj/mrFepDA==</OBJ_KEY><NODE_NAME>ROOT_PROPERTY</NODE_NAME><NODE_ESR_NAME/>|.
    rv_xml = rv_xml && |<NODE_ESR_PREFIX/><NODE_GENIL_NAME/><DATA_TYPE>/BOBF/S_FRW_PROPERTY_K</DATA_TYPE>|.
    rv_xml = rv_xml && |<DATA_DATA_TYPE>/BOBF/S_FRW_PROPERTY_D</DATA_DATA_TYPE><DATA_DATA_TYPE_T/><DATA_TABLE_TYPE>|.
    rv_xml = rv_xml && |/BOBF/T_FRW_PROPERTY_K</DATA_TABLE_TYPE><GDT_DATA_TYPE/><PRX_DAT_DAT_TYPE/><EXT_INCL_NAME/>|.
    rv_xml = rv_xml && |<EXT_INCL_NAME_T/><DATABASE_TABLE/><NODE_TYPE>F</NODE_TYPE><NODE_CAT_KEY/><SP_MAPPER_CLASS/>|.
    rv_xml = rv_xml && |<SP_ID_MAP_CLASS/><BUF_CLASS>/BOBF/CL_LIB_B_PROPERTY</BUF_CLASS><MAPPER_CLASS/><DELEGATION_CLASS/>|.
    rv_xml = rv_xml && |<LCP_WRAP_CLASS/><LOADABLE/><LOCKABLE/><TRANSIENT>X</TRANSIENT><SUBTREE_PROPERTY/>|.
    rv_xml = rv_xml && |<KEY_INHERITED/><USE_PROXY_TYPE/><NODE_CLASS/><ALIAS_RETRIEVE/><ALIAS_RETRIEVE_S/>|.
    rv_xml = rv_xml && |<ALIAS_CREATE_S/><ALIAS_UPDATE_S/><ALIAS_DELETE_S/><KEY_SECKEYNAME/><ROOT_SECKEYNAME/>|.
    rv_xml = rv_xml && |<STA_CHILD_ASSOC/><STA_PARENT_ASSOC/><REF_BO_KEY/><REF_NODE_KEY/><EXTENDIBLE/><AUTH_CHECK_CLASS/>|.
    rv_xml = rv_xml && |<AUTH_CHECK_RELEVANT/><CREATE_USER>DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME>|.
    rv_xml = rv_xml && |<CHANGE_USER>DEVELOPER</CHANGE_USER><CHANGE_TIME>20160307143702</CHANGE_TIME></_-BOBF_-OBM_NODE>|.
    rv_xml = rv_xml && |</_-BOBF_-OBM_NODE><_-BOBF_-OBM_NODE_field_catalog><item><POS>1</POS><NAME>NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>NODE_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>BO_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>6</POS><NAME>OBJ_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>NODE_NAME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |8</POS><NAME>NODE_ESR_NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>9</POS><NAME>NODE_ESR_PREFIX</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |g</TYPE_KIND><LENGTH>255</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |10</POS><NAME>NODE_GENIL_NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>19</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>11</POS><NAME>DATA_TYPE</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>12</POS><NAME>|.
    rv_xml = rv_xml && |DATA_DATA_TYPE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>13</POS><NAME>DATA_DATA_TYPE_T</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>14</POS><NAME>|.
    rv_xml = rv_xml && |DATA_TABLE_TYPE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>15</POS><NAME>GDT_DATA_TYPE</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>62</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>16</POS><NAME>|.
    rv_xml = rv_xml && |PRX_DAT_DAT_TYPE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>17</POS><NAME>EXT_INCL_NAME</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>18</POS><NAME>|.
    rv_xml = rv_xml && |EXT_INCL_NAME_T</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>19</POS><NAME>DATABASE_TABLE</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>20</POS><NAME>|.
    rv_xml = rv_xml && |NODE_TYPE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>21</POS><NAME>NODE_CAT_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>22</POS><NAME>SP_MAPPER_CLASS</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>23</POS><NAME>SP_ID_MAP_CLASS</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>24</POS><NAME>BUF_CLASS</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |25</POS><NAME>MAPPER_CLASS</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>26</POS><NAME>DELEGATION_CLASS</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |27</POS><NAME>LCP_WRAP_CLASS</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>28</POS><NAME>LOADABLE</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>29</POS><NAME>|.
    rv_xml = rv_xml && |LOCKABLE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>30</POS><NAME>TRANSIENT</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>31</POS><NAME>SUBTREE_PROPERTY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item>|.
    rv_xml = rv_xml && |<POS>32</POS><NAME>KEY_INHERITED</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>33</POS><NAME>USE_PROXY_TYPE</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item>|.
    rv_xml = rv_xml && |<POS>34</POS><NAME>NODE_CLASS</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>35</POS><NAME>ALIAS_RETRIEVE</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |36</POS><NAME>ALIAS_RETRIEVE_S</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>37</POS><NAME>ALIAS_CREATE_S</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |38</POS><NAME>ALIAS_UPDATE_S</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>39</POS><NAME>ALIAS_DELETE_S</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |40</POS><NAME>KEY_SECKEYNAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>41</POS><NAME>ROOT_SECKEYNAME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |42</POS><NAME>STA_CHILD_ASSOC</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>43</POS><NAME>STA_PARENT_ASSOC</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |44</POS><NAME>REF_BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>45</POS><NAME>REF_NODE_KEY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |46</POS><NAME>EXTENDIBLE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>47</POS><NAME>AUTH_CHECK_CLASS</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |48</POS><NAME>AUTH_CHECK_RELEVANT</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>49</POS><NAME>CREATE_USER</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>50</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>51</POS><NAME>CHANGE_USER</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>52</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_NODE_field_catalog><_-BOBF_-OBM_NODET>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_NODET><LANGU>E</LANGU><NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>|.
    rv_xml = rv_xml && |00000</VERSION><NODE_KEY>AAwphY1KHuW5jj/mrFfJDA==</NODE_KEY><BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY>|.
    rv_xml = rv_xml && |<DESCRIPTION>Root node</DESCRIPTION></_-BOBF_-OBM_NODET></_-BOBF_-OBM_NODET><_-BOBF_-OBM_NODET_field_catalog>|.
    rv_xml = rv_xml && |<item><POS>1</POS><NAME>LANGU</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS><NAME>NAME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>3</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS><NAME>VERSION</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>5</POS><NAME>NODE_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>6</POS><NAME>BO_KEY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |7</POS><NAME>DESCRIPTION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>40</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_NODET_field_catalog><_-BOBF_-OBM_OBJ><_-BOBF_-OBM_OBJ>|.
    rv_xml = rv_xml && |<NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>00000</VERSION><OBJ_KEY>AAwphY1KHuW5jj/mrFepDA==</OBJ_KEY>|.
    rv_xml = rv_xml && |<BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY><ROOT_NODE_KEY>AAwphY1KHuW5jj/mrFfJDA==</ROOT_NODE_KEY>|.
    rv_xml = rv_xml && |<BUFFER_CLASS>/BOBF/CL_BUF_SIMPLE</BUFFER_CLASS><MAPPER_CLASS>/BOBF/CL_DAC_TABLE</MAPPER_CLASS>|.
    rv_xml = rv_xml && |<SP_MAPPER_CLASS/><SP_CLASS/><CONST_INTERFACE/><ACCESS_CLASS/><STATUS_CLASS/><DERIVATOR_CLASS/>|.
    rv_xml = rv_xml && |<CLEANUP_MODE>2</CLEANUP_MODE><ENQUEUE_SCOPE>2</ENQUEUE_SCOPE><OBJCAT>0</OBJCAT><PROXY_INCOMPLETE/>|.
    rv_xml = rv_xml && |<PROXY_CHECKSUM/><PROXY_TIME>0</PROXY_TIME><GENIL_ENABLED/><GENIL_COMP_NAME/><GENIL_PREFIX/>|.
    rv_xml = rv_xml && |<CHECK_SERVICES>X</CHECK_SERVICES><CHK_SERV_ASSOC>N</CHK_SERV_ASSOC><CHK_SERV_ACTION>|.
    rv_xml = rv_xml && |N</CHK_SERV_ACTION><FINAL/><EXTENDIBLE/><ABSTRACT/><NAMESPACE/><PREFIX/><TD_CONTAINER/>|.
    rv_xml = rv_xml && |<AUTH_CHECK_RELEVANT/><ES_RELEVANT/><ES_TEMPLATE/><ES_DATA_EXTRACTOR_CLASS/><LAST_CI_RELEVANT_CHANGE>|.
    rv_xml = rv_xml && |0</LAST_CI_RELEVANT_CHANGE><LAST_CI_GENERATION>0</LAST_CI_GENERATION><CREATE_USER>|.
    rv_xml = rv_xml && |DEVELOPER</CREATE_USER><CREATE_TIME>20160307143702</CREATE_TIME><CHANGE_USER>DEVELOPER</CHANGE_USER>|.
    rv_xml = rv_xml && |<CHANGE_TIME>20160307143702</CHANGE_TIME><LAST_CHANGE_USER>DEVELOPER</LAST_CHANGE_USER>|.
    rv_xml = rv_xml && |<LAST_CHANGE_TIME>20160307143702</LAST_CHANGE_TIME></_-BOBF_-OBM_OBJ></_-BOBF_-OBM_OBJ>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_OBJ_field_catalog><item><POS>1</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS>|.
    rv_xml = rv_xml && |<NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS>|.
    rv_xml = rv_xml && |<NAME>OBJ_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>6</POS><NAME>|.
    rv_xml = rv_xml && |ROOT_NODE_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>7</POS><NAME>BUFFER_CLASS</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>8</POS><NAME>|.
    rv_xml = rv_xml && |MAPPER_CLASS</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>9</POS><NAME>SP_MAPPER_CLASS</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>10</POS><NAME>|.
    rv_xml = rv_xml && |SP_CLASS</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>11</POS><NAME>CONST_INTERFACE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>12</POS><NAME>ACCESS_CLASS</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>13</POS><NAME>STATUS_CLASS</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>14</POS><NAME>DERIVATOR_CLASS</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>15</POS><NAME>CLEANUP_MODE</NAME><TYPE_KIND>N</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>16</POS><NAME>ENQUEUE_SCOPE</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>N</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item>|.
    rv_xml = rv_xml && |<POS>17</POS><NAME>OBJCAT</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>18</POS><NAME>PROXY_INCOMPLETE</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>19</POS>|.
    rv_xml = rv_xml && |<NAME>PROXY_CHECKSUM</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>40</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>20</POS><NAME>PROXY_TIME</NAME><TYPE_KIND>P</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>21</POS><NAME>|.
    rv_xml = rv_xml && |GENIL_ENABLED</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>22</POS><NAME>GENIL_COMP_NAME</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>6</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>23</POS><NAME>|.
    rv_xml = rv_xml && |GENIL_PREFIX</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>10</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>24</POS><NAME>CHECK_SERVICES</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>25</POS><NAME>|.
    rv_xml = rv_xml && |CHK_SERV_ASSOC</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>26</POS><NAME>CHK_SERV_ACTION</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>27</POS><NAME>|.
    rv_xml = rv_xml && |FINAL</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>28</POS><NAME>EXTENDIBLE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>29</POS><NAME>ABSTRACT</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item>|.
    rv_xml = rv_xml && |<POS>30</POS><NAME>NAMESPACE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>10</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>31</POS><NAME>PREFIX</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>10</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>32</POS><NAME>|.
    rv_xml = rv_xml && |TD_CONTAINER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>33</POS><NAME>AUTH_CHECK_RELEVANT</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>34</POS><NAME>|.
    rv_xml = rv_xml && |ES_RELEVANT</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>35</POS><NAME>ES_TEMPLATE</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>19</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>36</POS><NAME>|.
    rv_xml = rv_xml && |ES_DATA_EXTRACTOR_CLASS</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>37</POS><NAME>LAST_CI_RELEVANT_CHANGE</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>38</POS><NAME>LAST_CI_GENERATION</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>39</POS><NAME>CREATE_USER</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>40</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>41</POS><NAME>CHANGE_USER</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>42</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>43</POS><NAME>LAST_CHANGE_USER</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>44</POS><NAME>LAST_CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_OBJ_field_catalog><_-BOBF_-OBM_OBJT>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_OBJT><LANGU>E</LANGU><NAME>ZABAPGIT_UNITTEST</NAME><EXTENSION/><VERSION>|.
    rv_xml = rv_xml && |00000</VERSION><OBJ_KEY>AAwphY1KHuW5jj/mrFepDA==</OBJ_KEY><BO_KEY>AAwphY1KHuW5jj/mrFeJDA==</BO_KEY>|.
    rv_xml = rv_xml && |<DESCRIPTION>Demo object for complete roundtrip</DESCRIPTION></_-BOBF_-OBM_OBJT></_-BOBF_-OBM_OBJT>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_OBJT_field_catalog><item><POS>1</POS><NAME>LANGU</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS>|.
    rv_xml = rv_xml && |<NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS>|.
    rv_xml = rv_xml && |<NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>OBJ_KEY</NAME><TYPE_KIND>X</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>6</POS>|.
    rv_xml = rv_xml && |<NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>7</POS><NAME>DESCRIPTION</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>40</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_OBJT_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_RTW/><_-BOBF_-OBM_RTW_field_catalog><item><POS>1</POS><NAME>NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>RTW_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>BO_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>6</POS><NAME>ACCESS_CAT</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>CONTENT_CAT</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>8</POS>|.
    rv_xml = rv_xml && |<NAME>CONTENT_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>9</POS><NAME>NODE_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>10</POS><NAME>ASSOC_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>11</POS><NAME>TRIGGER_CREATE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>12</POS><NAME>TRIGGER_UPDATE</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item>|.
    rv_xml = rv_xml && |<POS>13</POS><NAME>TRIGGER_DELETE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>14</POS><NAME>TRIGGER_CHECK</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item>|.
    rv_xml = rv_xml && |<POS>15</POS><NAME>TRIGGER_LOAD</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>16</POS><NAME>TRIGGER_PROPERTY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>17</POS>|.
    rv_xml = rv_xml && |<NAME>TRIGGER_LOCK</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>18</POS><NAME>TRIGGER_RETRIEVE</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>19</POS><NAME>|.
    rv_xml = rv_xml && |MODELED_ONLY</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>20</POS><NAME>CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>21</POS><NAME>|.
    rv_xml = rv_xml && |CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>22</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>23</POS><NAME>|.
    rv_xml = rv_xml && |CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item></_-BOBF_-OBM_RTW_field_catalog><_-BOBF_-VAL_CONF/><_-BOBF_-VAL_CONF_field_catalog>|.
    rv_xml = rv_xml && |<item><POS>1</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>3</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS><NAME>CONF_KEY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>5</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>6</POS><NAME>ACT_KEY</NAME><TYPE_KIND>X</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>|.
    rv_xml = rv_xml && |VAL_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>8</POS><NAME>NODE_CAT_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>9</POS><NAME>CREATE_USER</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>10</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>11</POS><NAME>CHANGE_USER</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>12</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-VAL_CONF_field_catalog><_-BOBF_-VAL_LIST/>|.
    rv_xml = rv_xml && |<_-BOBF_-VAL_LIST_field_catalog><item><POS>1</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS>|.
    rv_xml = rv_xml && |<NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS>|.
    rv_xml = rv_xml && |<NAME>VAL_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>6</POS><NAME>|.
    rv_xml = rv_xml && |VAL_NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>7</POS><NAME>NODE_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>8</POS><NAME>VAL_CLASS</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |9</POS><NAME>VAL_CAT</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>10</POS><NAME>NO_EXEC_IF_FAIL</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>11</POS><NAME>|.
    rv_xml = rv_xml && |CHECK_IMPL</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>12</POS><NAME>CHECK_DELTA_IMPL</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>13</POS><NAME>|.
    rv_xml = rv_xml && |TD_CONTAINER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>14</POS><NAME>TD_CONTAINER_VAR</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>15</POS><NAME>|.
    rv_xml = rv_xml && |CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>16</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>17</POS><NAME>|.
    rv_xml = rv_xml && |CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>18</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>19</POS><NAME>|.
    rv_xml = rv_xml && |VAL_IMPACT</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>20</POS><NAME>VAL_SUB_CAT</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>21</POS><NAME>|.
    rv_xml = rv_xml && |SIMPLIFIED_DT_TAG</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item></_-BOBF_-VAL_LIST_field_catalog><_-BOBF_-VAL_LISTT/><_-BOBF_-VAL_LISTT_field_catalog>|.
    rv_xml = rv_xml && |<item><POS>1</POS><NAME>LANGU</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS><NAME>NAME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>3</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS><NAME>VERSION</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>5</POS><NAME>VAL_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>6</POS><NAME>BO_KEY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |7</POS><NAME>DESCRIPTION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>40</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item></_-BOBF_-VAL_LISTT_field_catalog><_-BOBF_-VAL_NET/><_-BOBF_-VAL_NET_field_catalog>|.
    rv_xml = rv_xml && |<item><POS>1</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>3</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS><NAME>NET_KEY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>5</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>6</POS><NAME>VAL_KEY_FROM</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |7</POS><NAME>VAL_KEY_TO</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>8</POS><NAME>CREATE_USER</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |9</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>10</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |11</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item></_-BOBF_-VAL_NET_field_catalog><_-BOBF_-OBM_PROPTY/>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_PROPTY_field_catalog><item><POS>1</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS>|.
    rv_xml = rv_xml && |<NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS>|.
    rv_xml = rv_xml && |<NAME>PROPERTY_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>6</POS><NAME>|.
    rv_xml = rv_xml && |NODE_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>7</POS><NAME>NODE_CAT_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>8</POS><NAME>ATTRIBUTE_NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>256</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>9</POS><NAME>PROPERTY_NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>10</POS><NAME>PROPERTY_VALUE</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item>|.
    rv_xml = rv_xml && |<POS>11</POS><NAME>FINAL</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>12</POS><NAME>CREATE_USER</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |13</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>14</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |15</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_PROPTY_field_catalog><_-BOBF_-OBM_ALTKEY/>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_ALTKEY_field_catalog><item><POS>1</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS>|.
    rv_xml = rv_xml && |<NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS>|.
    rv_xml = rv_xml && |<NAME>ALTKEY_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>6</POS><NAME>|.
    rv_xml = rv_xml && |NODE_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>7</POS><NAME>ALTKEY_NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>8</POS><NAME>ALTKEY_ESR_NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>9</POS><NAME>DATA_TYPE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>10</POS><NAME>DATA_TABLE_TYPE</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>11</POS><NAME>PRX_DATA_TYPE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>12</POS><NAME>QUERY_KEY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |13</POS><NAME>SECKEYNAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>14</POS><NAME>NOT_UNIQUE</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>15</POS>|.
    rv_xml = rv_xml && |<NAME>SP_MAPPER_CLASS</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>16</POS><NAME>ALIAS_ALTKEY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |17</POS><NAME>ALIAS_ALTKEY_S</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>18</POS><NAME>NOT_DAC_RELEVANT</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>19</POS>|.
    rv_xml = rv_xml && |<NAME>ALTKEY_NAME_AIE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>20</POS><NAME>UNIQUE_CHECK_AIE</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>21</POS>|.
    rv_xml = rv_xml && |<NAME>CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>22</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>23</POS><NAME>|.
    rv_xml = rv_xml && |CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>24</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_ALTKEY_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_ALTKET/><_-BOBF_-OBM_ALTKET_field_catalog><item><POS>1</POS><NAME>LANGU</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>EXTENSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>ALTKEY_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>6</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>DESCRIPTION</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>40</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_ALTKET_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_QUERY/><_-BOBF_-OBM_QUERY_field_catalog><item><POS>1</POS><NAME>NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>QUERY_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>BO_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>6</POS><NAME>NODE_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>QUERY_NAME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |8</POS><NAME>QUERY_ESR_NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>9</POS><NAME>QUERY_GENIL_NAME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>18</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |10</POS><NAME>DATA_TYPE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>11</POS><NAME>PRX_DATA_TYPE</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |12</POS><NAME>QUERY_CLASS</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>13</POS><NAME>ALIAS_QUERY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |14</POS><NAME>SP_MAPPER_CLASS</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>15</POS><NAME>QUERY_CAT</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>16</POS><NAME>|.
    rv_xml = rv_xml && |USE_PROXY_TYPE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>17</POS><NAME>TD_CONTAINER</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>18</POS><NAME>|.
    rv_xml = rv_xml && |TD_CONTAINER_VAR</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>19</POS><NAME>RESULT_TYPE</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>20</POS><NAME>|.
    rv_xml = rv_xml && |RESULT_TYPE_T</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>21</POS><NAME>HANA_VIEW</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>255</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>22</POS><NAME>|.
    rv_xml = rv_xml && |CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>23</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>24</POS><NAME>|.
    rv_xml = rv_xml && |CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>25</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_QUERY_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_QUERYT/><_-BOBF_-OBM_QUERYT_field_catalog><item><POS>1</POS><NAME>LANGU</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>EXTENSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>QUERY_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>6</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>DESCRIPTION</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>40</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_QUERYT_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_ASSOCB/><_-BOBF_-OBM_ASSOCB_field_catalog><item><POS>1</POS><NAME>NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>ASSOCB_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>BO_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>6</POS><NAME>ASSOC_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>ATTRIBUTE_CAT</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item>|.
    rv_xml = rv_xml && |<POS>8</POS><NAME>ATTRIBUTE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>256</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>9</POS><NAME>FROM_BINDING_CAT</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>10</POS>|.
    rv_xml = rv_xml && |<NAME>FROM_BINDING</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>256</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>11</POS><NAME>TO_BINDING_CAT</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>12</POS><NAME>|.
    rv_xml = rv_xml && |TO_BINDING</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>256</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>13</POS><NAME>SIGN</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |2</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>14</POS><NAME>CONST_INTERFACE</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>15</POS><NAME>DATA_ACCESS_RELE</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>16</POS><NAME>ALTKEY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |17</POS><NAME>CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>18</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |19</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>20</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_ASSOCB_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_PRXPTY/><_-BOBF_-OBM_PRXPTY_field_catalog><item><POS>1</POS><NAME>NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>PROPERTY_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>|.
    rv_xml = rv_xml && |BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>6</POS><NAME>CONTENT_CAT</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>CONTENT_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>8</POS><NAME>HIERARCHY_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>9</POS><NAME>ATTRIBUTE_NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>10</POS><NAME>PROPERTY_NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>11</POS><NAME>PROPERTY_VALUE</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item>|.
    rv_xml = rv_xml && |<POS>12</POS><NAME>FINAL</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>13</POS><NAME>CREATE_USER</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |14</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>15</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |16</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_PRXPTY_field_catalog><_-BOBF_-STA_SCHEMA/>|.
    rv_xml = rv_xml && |<_-BOBF_-STA_SCHEMA_field_catalog><item><POS>1</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS>|.
    rv_xml = rv_xml && |<NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS>|.
    rv_xml = rv_xml && |<NAME>STA_SCHEMA_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>6</POS><NAME>|.
    rv_xml = rv_xml && |NODE_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>7</POS><NAME>SCHEMA_NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>8</POS><NAME>SCHEMA_ESR_NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>9</POS><NAME>CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>10</POS><NAME>CREATE_TIME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>11</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>12</POS><NAME>CHANGE_TIME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |</_-BOBF_-STA_SCHEMA_field_catalog><_-BOBF_-STA_SCHEMT/><_-BOBF_-STA_SCHEMT_field_catalog>|.
    rv_xml = rv_xml && |<item><POS>1</POS><NAME>LANGU</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS><NAME>NAME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>3</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS><NAME>VERSION</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>5</POS><NAME>STA_SCHEMA_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>6</POS><NAME>BO_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>7</POS><NAME>DESCRIPTION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>40</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-STA_SCHEMT_field_catalog><_-BOBF_-STA_DERIV/>|.
    rv_xml = rv_xml && |<_-BOBF_-STA_DERIV_field_catalog><item><POS>1</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS>|.
    rv_xml = rv_xml && |<NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS>|.
    rv_xml = rv_xml && |<NAME>STA_DERIV_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>6</POS><NAME>|.
    rv_xml = rv_xml && |NODE_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>7</POS><NAME>DERIV_NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>8</POS><NAME>DERIV_ESR_NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>9</POS><NAME>DERIVATOR_CAT</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>10</POS><NAME>DERIVATOR_CLASS</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>11</POS><NAME>TD_CONTAINER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>12</POS><NAME>TD_CONTAINER_VAR</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>13</POS><NAME>CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>14</POS><NAME>CREATE_TIME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>15</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>16</POS><NAME>CHANGE_TIME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |</_-BOBF_-STA_DERIV_field_catalog><_-BOBF_-STA_DERIVT/><_-BOBF_-STA_DERIVT_field_catalog>|.
    rv_xml = rv_xml && |<item><POS>1</POS><NAME>LANGU</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS><NAME>NAME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>3</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS><NAME>VERSION</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>5</POS><NAME>STA_DERIV_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>6</POS><NAME>BO_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>7</POS><NAME>DESCRIPTION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>40</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-STA_DERIVT_field_catalog><_-BOBF_-STA_VAR/>|.
    rv_xml = rv_xml && |<_-BOBF_-STA_VAR_field_catalog><item><POS>1</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS>|.
    rv_xml = rv_xml && |<NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS>|.
    rv_xml = rv_xml && |<NAME>STA_VAR_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>6</POS><NAME>|.
    rv_xml = rv_xml && |NODE_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>7</POS><NAME>STA_VAR_NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>8</POS><NAME>STA_VAR_ESR_NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>9</POS><NAME>STA_VAR_CAT</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>10</POS><NAME>ATTRIBUTE_NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>256</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>11</POS><NAME>CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>12</POS><NAME>CREATE_TIME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>13</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>14</POS><NAME>CHANGE_TIME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |</_-BOBF_-STA_VAR_field_catalog><_-BOBF_-STA_VART/><_-BOBF_-STA_VART_field_catalog>|.
    rv_xml = rv_xml && |<item><POS>1</POS><NAME>LANGU</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS><NAME>NAME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>3</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS><NAME>VERSION</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>5</POS><NAME>STA_VAR_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>6</POS><NAME>BO_KEY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |7</POS><NAME>DESCRIPTION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>40</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item></_-BOBF_-STA_VART_field_catalog><_-BOBF_-OBM_VSET/><_-BOBF_-OBM_VSET_field_catalog>|.
    rv_xml = rv_xml && |<item><POS>1</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>3</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS><NAME>VSET_KEY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item>|.
    rv_xml = rv_xml && |<POS>5</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>6</POS><NAME>CONTENT_CAT</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS>|.
    rv_xml = rv_xml && |<NAME>CONTENT_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>8</POS><NAME>VSET_NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>9</POS><NAME>VSET_ESR_NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>255</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>10</POS><NAME>VSET_CAT</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>11</POS><NAME>VALUE_SET_CLASS</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>12</POS><NAME>NODE_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>13</POS><NAME>ASSOC_KEY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |14</POS><NAME>CODE_CIF_NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>15</POS><NAME>TD_CONTAINER</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |16</POS><NAME>TD_CONTAINER_VAR</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>17</POS><NAME>CREATE_USER</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |18</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>19</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |20</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_VSET_field_catalog><_-BOBF_-OBM_VSETT/>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_VSETT_field_catalog><item><POS>1</POS><NAME>LANGU</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS>|.
    rv_xml = rv_xml && |<NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS>|.
    rv_xml = rv_xml && |<NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>VSET_KEY</NAME><TYPE_KIND>X</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>6</POS>|.
    rv_xml = rv_xml && |<NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>7</POS><NAME>DESCRIPTION</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>40</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_VSETT_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_RTW_A/><_-BOBF_-OBM_RTW_A_field_catalog><item><POS>1</POS><NAME>NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>CONF_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>BO_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>6</POS><NAME>RTW_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>ATTRIBUTE_NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>256</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>8</POS><NAME>ACCESS_CAT</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>9</POS><NAME>CONTENT_CAT</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>10</POS>|.
    rv_xml = rv_xml && |<NAME>CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>11</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>12</POS><NAME>|.
    rv_xml = rv_xml && |CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>13</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_RTW_A_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_CODE_L/><_-BOBF_-OBM_CODE_L_field_catalog><item><POS>1</POS><NAME>NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>VSET_CL_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>|.
    rv_xml = rv_xml && |BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>6</POS><NAME>VSET_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>CODE_NAME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |8</POS><NAME>CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>9</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |10</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>11</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_CODE_L_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_GEN_IN/><_-BOBF_-OBM_GEN_IN_field_catalog><item><POS>1</POS><NAME>NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>GEN_INFO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>|.
    rv_xml = rv_xml && |BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>6</POS><NAME>CONTENT_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>CONTENT_CAT</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item>|.
    rv_xml = rv_xml && |<POS>8</POS><NAME>GENERATE_TYPE</NAME><TYPE_KIND>b</TYPE_KIND><LENGTH>3</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>9</POS><NAME>GENERATE_VARIANT</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |b</TYPE_KIND><LENGTH>3</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>10</POS>|.
    rv_xml = rv_xml && |<NAME>TEMPL_VERSION</NAME><TYPE_KIND>I</TYPE_KIND><LENGTH>10</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>11</POS><NAME>RELEV_CHANGES</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>1</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>12</POS><NAME>|.
    rv_xml = rv_xml && |CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>13</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>14</POS><NAME>|.
    rv_xml = rv_xml && |CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>15</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_GEN_IN_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_DISEL/><_-BOBF_-OBM_DISEL_field_catalog><item><POS>1</POS><NAME>NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>DISABLED_EL_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>|.
    rv_xml = rv_xml && |BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>6</POS><NAME>CONTENT_NODE_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>CONTENT_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>8</POS><NAME>CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>9</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |10</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>11</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_DISEL_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_FINORD/><_-BOBF_-OBM_FINORD_field_catalog><item><POS>1</POS><NAME>NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>FINALIZE_NET_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>|.
    rv_xml = rv_xml && |BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>6</POS><NAME>PREDECESSOR_BO</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>SUCCESSOR_BO</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>8</POS><NAME>CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>9</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |10</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>11</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_FINORD_field_catalog>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_SUBSCR/><_-BOBF_-OBM_SUBSCR_field_catalog><item><POS>1</POS><NAME>NAME</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>2</POS><NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>N</TYPE_KIND><LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY>|.
    rv_xml = rv_xml && |</item><item><POS>4</POS><NAME>SUBSCR_NET_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>|.
    rv_xml = rv_xml && |BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>6</POS><NAME>OBSERVED_BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>7</POS><NAME>CREATE_USER</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>8</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>9</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |10</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_SUBSCR_field_catalog><_-BOBF_-ACF_MAP/>|.
    rv_xml = rv_xml && |<_-BOBF_-ACF_MAP_field_catalog><item><POS>1</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS>|.
    rv_xml = rv_xml && |<NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS>|.
    rv_xml = rv_xml && |<NAME>ACF_MAP_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>ACF</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>10</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>6</POS><NAME>|.
    rv_xml = rv_xml && |AUTH_OBJ_NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>10</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY/></item><item><POS>7</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>8</POS><NAME>NODE_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>9</POS><NAME>ASSOC_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>10</POS><NAME>TARGET_NODE_KEY</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>11</POS><NAME>TARGET_FIELD_NAME</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>|.
    rv_xml = rv_xml && |30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>12</POS><NAME>CREATE_USER</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>13</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>14</POS><NAME>CHANGE_USER</NAME>|.
    rv_xml = rv_xml && |<TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item>|.
    rv_xml = rv_xml && |<item><POS>15</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-ACF_MAP_field_catalog><_-BOBF_-AUTH_OBJ/>|.
    rv_xml = rv_xml && |<_-BOBF_-AUTH_OBJ_field_catalog><item><POS>1</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS>|.
    rv_xml = rv_xml && |<NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS>|.
    rv_xml = rv_xml && |<NAME>AUTH_OBJ_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>AUTH_OBJ_NAME</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>10</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>6</POS><NAME>|.
    rv_xml = rv_xml && |BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>7</POS><NAME>NODE_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>8</POS><NAME>CREATE_USER</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |9</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>10</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |11</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item></_-BOBF_-AUTH_OBJ_field_catalog><_-BOBF_-OBM_AK_FLD/>|.
    rv_xml = rv_xml && |<_-BOBF_-OBM_AK_FLD_field_catalog><item><POS>1</POS><NAME>NAME</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>2</POS>|.
    rv_xml = rv_xml && |<NAME>EXTENSION</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>1</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>3</POS><NAME>VERSION</NAME><TYPE_KIND>N</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>5</LENGTH><DECIMALS>0</DECIMALS><IS_KEY>X</IS_KEY></item><item><POS>4</POS>|.
    rv_xml = rv_xml && |<NAME>AK_FIELD_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS>|.
    rv_xml = rv_xml && |<IS_KEY>X</IS_KEY></item><item><POS>5</POS><NAME>FIELD_NAME</NAME><TYPE_KIND>C</TYPE_KIND>|.
    rv_xml = rv_xml && |<LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>6</POS><NAME>|.
    rv_xml = rv_xml && |FIELD_NO</NAME><TYPE_KIND>b</TYPE_KIND><LENGTH>3</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/>|.
    rv_xml = rv_xml && |</item><item><POS>7</POS><NAME>BO_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH>|.
    rv_xml = rv_xml && |<DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>8</POS><NAME>NODE_KEY</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |9</POS><NAME>ALTKEY_KEY</NAME><TYPE_KIND>X</TYPE_KIND><LENGTH>16</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>10</POS><NAME>ALTKEY_NAME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |C</TYPE_KIND><LENGTH>30</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |11</POS><NAME>CREATE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>12</POS><NAME>CREATE_TIME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item><item><POS>|.
    rv_xml = rv_xml && |13</POS><NAME>CHANGE_USER</NAME><TYPE_KIND>C</TYPE_KIND><LENGTH>12</LENGTH><DECIMALS>|.
    rv_xml = rv_xml && |0</DECIMALS><IS_KEY/></item><item><POS>14</POS><NAME>CHANGE_TIME</NAME><TYPE_KIND>|.
    rv_xml = rv_xml && |P</TYPE_KIND><LENGTH>15</LENGTH><DECIMALS>0</DECIMALS><IS_KEY/></item></_-BOBF_-OBM_AK_FLD_field_catalog>|.
    rv_xml = rv_xml && |</asx:values></asx:abap></abapGit>|.

  ENDMETHOD.

ENDCLASS.
