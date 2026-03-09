CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_abapgit_objects_generic DEFINITION LOCAL FRIENDS ltcl_test.
CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      serialize FOR TESTING RAISING zcx_abapgit_exception,
      get_primary_table FOR TESTING RAISING zcx_abapgit_exception,
      get_key_fields FOR TESTING RAISING zcx_abapgit_exception,
      get_where_clause FOR TESTING RAISING zcx_abapgit_exception,
      resolve_logical_object FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD serialize.

    DATA: lo_cut  TYPE REF TO zcl_abapgit_objects_generic,
          li_xml  TYPE REF TO zif_abapgit_xml_output,
          ls_item TYPE zif_abapgit_definitions=>ty_item.

* assumption: this object exists in all SAP systems
    ls_item-obj_type = 'ASFC'.
    ls_item-obj_name = 'SAP_AS_TEST_001'.

    CREATE OBJECT lo_cut
      EXPORTING
        is_item = ls_item.

    CREATE OBJECT li_xml TYPE zcl_abapgit_xml_output.

    lo_cut->serialize( li_xml ).
* checks that it does not dump

  ENDMETHOD.

  METHOD get_primary_table.

    DATA: lo_cut  TYPE REF TO zcl_abapgit_objects_generic,
          ls_item TYPE zif_abapgit_definitions=>ty_item.

* assumption: this object exists in all SAP systems
    ls_item-obj_type = 'ACGR'.
    ls_item-obj_name = 'SAP_BC_BASIS_ADMIN'.

    CREATE OBJECT lo_cut
      EXPORTING
        is_item     = ls_item
        iv_language = zif_abapgit_definitions=>c_english.

    cl_abap_unit_assert=>assert_equals(
      exp = 'AGR_DEFINE'
      act = lo_cut->get_primary_table( ) ).

  ENDMETHOD.

  METHOD get_key_fields.

    DATA: lo_cut        TYPE REF TO zcl_abapgit_objects_generic,
          ls_item       TYPE zif_abapgit_definitions=>ty_item,
          ls_keys       TYPE dfies,
          lt_key_fields TYPE ddfields.

* assumption: this object exists in all SAP systems
    ls_item-obj_type = 'ACGR'.
    ls_item-obj_name = 'SAP_BC_BASIS_ADMIN'.

    CREATE OBJECT lo_cut
      EXPORTING
        is_item     = ls_item
        iv_language = zif_abapgit_definitions=>c_english.

    lt_key_fields = lo_cut->get_key_fields( 'AGR_HIER' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 3
      act = lines( lt_key_fields ) ).

    READ TABLE lt_key_fields INTO ls_keys INDEX 3.

    cl_abap_unit_assert=>assert_equals(
      exp = 'OBJECT_ID'
      act = ls_keys-fieldname ).

  ENDMETHOD.

  METHOD get_where_clause.

    DATA: lo_cut            TYPE REF TO zcl_abapgit_objects_generic,
          ls_item           TYPE zif_abapgit_definitions=>ty_item,
          lt_resolved_e071k TYPE e071k_t,
          ls_resolved_e071k LIKE LINE OF lt_resolved_e071k,
          lt_exp_where_tab  TYPE zcl_abapgit_objects_generic=>ty_where_tab,
          ls_exp_where_tab  LIKE LINE OF lt_exp_where_tab,
          lt_act_where_tab  TYPE zcl_abapgit_objects_generic=>ty_where_tab,
          lv_tab_name       TYPE tabname.


* assumption: this object exists in all SAP systems
    ls_item-obj_type = 'NSPC'.
    ls_item-obj_name = '/BIC/'.

    CREATE OBJECT lo_cut
      EXPORTING
        is_item     = ls_item
        iv_language = zif_abapgit_definitions=>c_english.

    lo_cut->resolve_logical_object( IMPORTING et_resolved_e071k = lt_resolved_e071k ).

    READ TABLE lt_resolved_e071k INTO ls_resolved_e071k INDEX 1.
    lv_tab_name = ls_resolved_e071k-mastername.
    lt_act_where_tab = lo_cut->get_where_clause( iv_tabname = lv_tab_name
                                                 iv_tabkey  = ls_resolved_e071k-tabkey ).

    ls_exp_where_tab-line = ` NAMESPACE EQ '/BIC/'`.
    APPEND ls_exp_where_tab TO lt_exp_where_tab.

    cl_abap_unit_assert=>assert_equals( exp = lt_exp_where_tab
                                        act = lt_act_where_tab ).

    CLEAR ls_exp_where_tab.

    READ TABLE lt_resolved_e071k INTO ls_resolved_e071k INDEX 2.
    lv_tab_name = ls_resolved_e071k-mastername.
    lt_act_where_tab = lo_cut->get_where_clause( iv_tabname = lv_tab_name
                                                 iv_tabkey  = ls_resolved_e071k-tabkey ).

    ls_exp_where_tab-line = ` NAMESPACE EQ '/BIC/'`.
    ls_exp_where_tab-line = `AND SPRAS EQ 'E'`.
    APPEND ls_exp_where_tab TO lt_exp_where_tab.

    cl_abap_unit_assert=>assert_equals( exp = lt_exp_where_tab
                                        act = lt_act_where_tab ).

  ENDMETHOD.

  METHOD resolve_logical_object.

    DATA: lo_cut                TYPE REF TO zcl_abapgit_objects_generic,
          ls_item               TYPE zif_abapgit_definitions=>ty_item,
          lt_exp_resolved_e071  TYPE e071tab,
          ls_exp_resolved_e071  LIKE LINE OF lt_exp_resolved_e071,
          lt_exp_resolved_e071k TYPE e071k_t,
          ls_exp_resolved_e071k LIKE LINE OF lt_exp_resolved_e071k,
          lt_act_resolved_e071  TYPE e071tab,
          lt_act_resolved_e071k TYPE e071k_t.

* assumption: this object exists in all SAP systems
    ls_item-obj_type = 'NSPC'.
    ls_item-obj_name = '/BIC/'.

    CREATE OBJECT lo_cut
      EXPORTING
        is_item     = ls_item
        iv_language = zif_abapgit_definitions=>c_english.

    ls_exp_resolved_e071-pgmid = 'R3TR'.
    ls_exp_resolved_e071-object = 'TABU'.
    ls_exp_resolved_e071-obj_name = 'TRNSPACET'.
    ls_exp_resolved_e071-objfunc = 'K'.
    APPEND ls_exp_resolved_e071 TO lt_exp_resolved_e071.

    ls_exp_resolved_e071-obj_name = 'TRNSPACETT'.
    APPEND ls_exp_resolved_e071 TO lt_exp_resolved_e071.


    ls_exp_resolved_e071k-pgmid = 'R3TR'.
    ls_exp_resolved_e071k-object = 'TABU'.
    ls_exp_resolved_e071k-objname = 'TRNSPACET'.
    ls_exp_resolved_e071k-mastertype = 'TABU'.
    ls_exp_resolved_e071k-mastername = 'TRNSPACET'.
    ls_exp_resolved_e071k-tabkey = '/BIC/'.
    APPEND ls_exp_resolved_e071k TO lt_exp_resolved_e071k.


    ls_exp_resolved_e071k-objname = 'TRNSPACETT'.
    ls_exp_resolved_e071k-mastertype = 'TABU'.
    ls_exp_resolved_e071k-mastername = 'TRNSPACETT'.
    ls_exp_resolved_e071k-tabkey = '/BIC/     E'.
    APPEND ls_exp_resolved_e071k TO lt_exp_resolved_e071k.

    lo_cut->resolve_logical_object( IMPORTING et_resolved_e071  = lt_act_resolved_e071
                                              et_resolved_e071k = lt_act_resolved_e071k ).

    cl_abap_unit_assert=>assert_equals( exp = lt_exp_resolved_e071
                                        act = lt_act_resolved_e071 ).

    cl_abap_unit_assert=>assert_equals( exp = lt_exp_resolved_e071k
                                        act = lt_act_resolved_e071k ).


  ENDMETHOD.

ENDCLASS.
