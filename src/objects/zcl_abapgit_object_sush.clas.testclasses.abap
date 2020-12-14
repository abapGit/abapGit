*"* use this source file for your ABAP unit test classes

class lcl_test_object_sush definition deferred.
class zcl_abapgit_object_sush definition local friends lcl_test_object_sush.
class lcl_test_object_sush definition for testing
  duration medium
  risk level harmless.

  private section.
    class-data:
      gv_test_obj_name type sobj_name,
      gv_name          type xupname.
    data:
      f_cut type ref to zcl_abapgit_object_sush.  "class under test

    class-methods:
      class_setup.

    methods:
      create_xml
        importing
          name          type string
        returning
          value(rv_xml) type string.

    methods: setup.
    methods: status_calls for testing.
    methods: deserialize for testing.
    methods: serialize for testing.
    methods: delete for testing.

endclass.

"--------------------------------------------------------------------------------------------------
" implementations
"--------------------------------------------------------------------------------------------------
class lcl_test_object_sush implementation.

  method class_setup.
    data:
      lo_su22  type ref to cl_su22_adt_object,
      ls_f4    type cl_su2x_api=>ts_su2x_h_vh,
      lt_f4    type cl_su2x_api=>tt_su2x_h_vh,
      ls_uhash type usobhash,
      ls_key   type usobkey.

    create object lo_su22.

*    "simple test for local update
    try.
        create object lo_su22.
        lt_f4 = lo_su22->if_su22_adt_object~get_value_help_for_crea( exporting iv_object = 'IW*'
                                                                               iv_name   = 'Z*'
                                                                               iv_devc   = '$TMP' ).
        read table lt_f4 index 1 into ls_f4.
        if lt_f4 is not initial.
          "create with transport object key of the leading application
          try.
              ls_uhash-pgmid    = 'R3TR'.
              ls_uhash-object   = ls_f4-object.
              ls_uhash-obj_name = ls_f4-obj_name.
              ls_key = lo_su22->if_su22_adt_object~create( exporting iv_new_key       = ls_uhash
                                                                     iv_abap_vers_flg = '1'
                                                                     iv_no_cts        = abap_true
                                                                     iv_task          = space ).
            catch cx_su2n_raise_events.
          endtry.
          if ls_key is not initial.
            gv_test_obj_name = ls_key.
            gv_name          = ls_key-name.
          endif.
        endif.
      catch cx_su2n_raise_events.
    endtry.

  endmethod.

  method setup.

    create object f_cut
      exporting
        is_item     = value #( obj_type = 'SUSH' obj_name = gv_test_obj_name )
        iv_language = sy-langu.

  endmethod.

  method status_calls.

    try.
        f_cut->zif_abapgit_object~exists(  ).
        f_cut->zif_abapgit_object~get_comparator(  ).
        f_cut->zif_abapgit_object~is_active(  ).
        f_cut->zif_abapgit_object~is_locked(  ).
        f_cut->zif_abapgit_object~changed_by(  ).
        f_cut->zif_abapgit_object~get_metadata(  ).
        f_cut->zif_abapgit_object~get_deserialize_steps(  ).
      catch  zcx_abapgit_exception into data(cx_ref).
        cl_abap_unit_assert=>skip( msg = 'Status check failed'
                                   detail  = cx_ref->get_text( )    ).
    endtry.
  endmethod.

  method deserialize.

    data lv_package type devclass.
    data lo_xml type ref to zcl_abapgit_xml_input.
    data iv_step type zif_abapgit_object=>ty_deserialization_step.
    data li_log type ref to zif_abapgit_log.
    data rv_complete_status type if_abapgit_object=>ty_complete_status.

    try.
        lo_xml = new zcl_abapgit_xml_input( create_xml( name = |{ gv_name }| ) ).

        rv_complete_status = f_cut->zif_abapgit_object~deserialize(
            iv_package  = lv_package
            io_xml      = lo_xml
            iv_step     = iv_step
            ii_log      = li_log
            iv_unittest = abap_true ).
      catch zcx_abapgit_exception .
        cl_abap_unit_assert=>fail( msg = 'DESERIALIZE failed' ).
    endtry.
    case rv_complete_status.
      when zif_abapgit_object=>c_complete_status-complete.
        cl_abap_unit_assert=>assert_equals(
              act   = rv_complete_status
              exp   = if_abapgit_object=>c_complete_status-complete ).
      when zif_abapgit_object=>c_complete_status-nothing.
        cl_abap_unit_assert=>assert_equals(
              act   = rv_complete_status
              exp   = if_abapgit_object=>c_complete_status-nothing ).
    endcase.

  endmethod.

  method serialize.

    data lo_xml type ref to zcl_abapgit_xml_output.
    data li_log type ref to zif_abapgit_log.

    li_log      = new zcl_abapgit_log( ) .
    create object lo_xml.

    try.
        f_cut->zif_abapgit_object~serialize(
            io_xml = lo_xml
            ii_log = li_log ).
      catch  zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( msg = 'SERIALIZE failed' ).
    endtry.

    li_log      = new zcl_abapgit_log( ) .
    create object f_cut  "serialize one existing object
      exporting
        is_item     = value #( obj_type = 'SUSH' obj_name = gv_test_obj_name )
        iv_language = sy-langu.

    try.
        f_cut->zif_abapgit_object~serialize(
          io_xml = lo_xml
          ii_log = li_log ).

      catch  zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( msg = 'SERIALIZE failed' ).
    endtry.

  endmethod.

  method delete.

    try.
        f_cut->zif_abapgit_object~delete( '$TMP' ).
      catch  zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( msg = 'DELETE failed' ).
    endtry.

  endmethod.

  method create_xml.

    rv_xml = | <?xml version="1.0" encoding="utf-8"?>| &&
    |<abapGit version="v1.0.0" serializer="LCL_OBJECT_SUSH" serializer_version="v1.0.0">| &&
    | <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">| &&
    |  <asx:values>| &&
    |   <HEAD>| &&
    |    <NAME>{ name }</NAME>| &&
    |    <TYPE>HT</TYPE>| &&
    |    <SRCSYSTEM>B6S</SRCSYSTEM>| &&
    |    <DEVCLASS>$TMP</DEVCLASS>| &&
    |    <ABAP_LANGUAGE_VERSION>1</ABAP_LANGUAGE_VERSION>| &&
    |   </HEAD>| &&
    |  </asx:values>| &&
    | </asx:abap>| &&
    |</abapGit>| .


  endmethod.

endclass.
