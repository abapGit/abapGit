class ltcl_unit_test definition deferred.
class zcl_abapgit_object_intf definition local friends ltcl_unit_test.

class lth_oo_object_fnc definition final for testing.

  public section.
    interfaces zif_abapgit_oo_object_fnc.

    data:
      mv_create_package     type devclass,
      ms_create_vseointerf  type vseointerf,
      ms_deserialize_key    type seoclskey,
      mt_deserialize_source type zif_abapgit_definitions=>ty_string_tt,
      ms_descriptions_key   type seoclskey,
      mt_descriptions       type zif_abapgit_oo_object_fnc=>ty_seocompotx_tt,
      ms_activation_item    type zif_abapgit_definitions=>ty_item.
endclass.

class lth_oo_object_fnc implementation.

  method zif_abapgit_oo_object_fnc~add_to_activation_list.
    ms_activation_item = is_item.
  endmethod.

  method zif_abapgit_oo_object_fnc~create.
    mv_create_package = iv_package.
    ms_create_vseointerf = cg_properties.
  endmethod.

  method zif_abapgit_oo_object_fnc~create_documentation.
  endmethod.

  method zif_abapgit_oo_object_fnc~create_sotr.
  endmethod.
  method zif_abapgit_oo_object_fnc~delete.
  endmethod.

  method zif_abapgit_oo_object_fnc~delete_documentation.
  endmethod.

  method zif_abapgit_oo_object_fnc~deserialize_source.
    ms_deserialize_key = is_key.
    mt_deserialize_source = it_source.
  endmethod.

  method zif_abapgit_oo_object_fnc~exists.
  endmethod.

  method zif_abapgit_oo_object_fnc~generate_locals.
  endmethod.

  method zif_abapgit_oo_object_fnc~get_class_properties.
  endmethod.

  method zif_abapgit_oo_object_fnc~get_includes.
  endmethod.

  method zif_abapgit_oo_object_fnc~get_interface_properties.
  endmethod.

  method zif_abapgit_oo_object_fnc~get_skip_test_classes.
  endmethod.

  method zif_abapgit_oo_object_fnc~insert_text_pool.
  endmethod.

  method zif_abapgit_oo_object_fnc~read_attributes.
  endmethod.

  method zif_abapgit_oo_object_fnc~read_descriptions.
  endmethod.

  method zif_abapgit_oo_object_fnc~read_documentation.
  endmethod.

  method zif_abapgit_oo_object_fnc~read_sotr.
  endmethod.

  method zif_abapgit_oo_object_fnc~read_superclass.
  endmethod.

  method zif_abapgit_oo_object_fnc~read_text_pool.
  endmethod.

  method zif_abapgit_oo_object_fnc~serialize_abap.
  endmethod.

  method zif_abapgit_oo_object_fnc~update_descriptions.
    ms_descriptions_key = is_key.
    mt_descriptions = it_descriptions.
  endmethod.

endclass.


class ltcl_unit_test definition final for testing duration short risk level harmless.

  private section.
    data:
      mo_cut        type ref to zif_abapgit_object,
      mo_log        type ref to zcl_abapgit_log,
      mo_object_fnc type ref to lth_oo_object_fnc,
      ms_item       type zif_abapgit_definitions=>ty_item.

    methods:
      setup,

      get_xml
        returning value(rv_xml) type string,
      get_source
        returning value(rt_source) type rswsourcet,

      deserializes for testing raising cx_static_check.
endclass.


class ltcl_unit_test implementation.

  method setup.
    data lo_cut type ref to zcl_abapgit_object_intf.

    ms_item-obj_name = 'ZIF_ABAPGIT_TEST_INTF'.
    ms_item-obj_type = 'INTF'.

    create object lo_cut
      exporting
        is_item     = ms_item
        iv_language = 'E'.

    create object lo_cut->zif_abapgit_object~mo_files
      exporting
        is_item = ms_item.

    create object mo_log.

    create object me->mo_object_fnc.
    lo_cut->mi_object_oriented_object_fct  = me->mo_object_fnc.

    me->mo_cut = lo_cut.

  endmethod.

  method deserializes.
    data lo_xmlin type ref to zcl_abapgit_xml_input.

    create object lo_xmlin type zcl_abapgit_xml_input
      exporting
        iv_xml = get_xml( ).

    mo_cut->mo_files->add_abap( get_source( ) ).

    mo_cut->deserialize(
      iv_package   = 'MY_PACKAGE'
      io_xml       = lo_xmlin
      iv_step      = zif_abapgit_object=>gc_step_id-abap
      ii_log       = mo_log
      iv_transport = 'XXX12345678' ).


    cl_abap_unit_assert=>assert_equals( exp = 'MY_PACKAGE' act = me->mo_object_fnc->mv_create_package ).

    data ls_expected_vseointerf type vseointerf.
    ls_expected_vseointerf-clsname = 'zif_abapgit_test_intf'.
    ls_expected_vseointerf-langu = 'e'.
    ls_expected_vseointerf-descript = 'test interface for abap git'.
    ls_expected_vseointerf-exposure = '2'.
    ls_expected_vseointerf-state = '1'.
    ls_expected_vseointerf-unicode = 'x'.
    cl_abap_unit_assert=>assert_equals( exp = ls_expected_vseointerf act = me->mo_object_fnc->ms_create_vseointerf ).

    data ls_expected_clskey type seoclskey.
    ls_expected_clskey-clsname = 'ZIF_ABAPGIT_TEST_INTF'.
    cl_abap_unit_assert=>assert_equals( exp = ls_expected_clskey act = me->mo_object_fnc->ms_deserialize_key ).
    cl_abap_unit_assert=>assert_equals( exp = ls_expected_clskey act = me->mo_object_fnc->ms_deserialize_key ).

    data lt_expected_descriptions type zif_abapgit_oo_object_fnc=>ty_seocompotx_tt.
    cl_abap_unit_assert=>assert_equals( exp = ls_expected_clskey act = me->mo_object_fnc->ms_descriptions_key ).
    cl_abap_unit_assert=>assert_equals( exp = lt_expected_descriptions act = me->mo_object_fnc->mt_descriptions ).

    cl_abap_unit_assert=>assert_equals( exp = me->ms_item act = me->mo_object_fnc->ms_activation_item ).
  endmethod.

  method get_xml.
    rv_xml =
    '<?xml version="1.0" encoding="UTF-8"?>' &&'<abapGit version="v1.0.0">' &&
    '<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">' &&
      '<asx:values>' &&
        '<VSEOINTERF>' &&
          '<CLSNAME>zif_abapgit_test_intf</CLSNAME>' &&
          '<LANGU>e</LANGU>' &&
          '<DESCRIPT>test interface for abap git</DESCRIPT>' &&
          '<EXPOSURE>2</EXPOSURE>' &&
          '<STATE>1</STATE>' &&
          '<UNICODE>x</UNICODE>' &&
        '</VSEOINTERF>' &&
      '</asx:values>' &&
    '</asx:abap>' &&
  '</abapGit>'.
  endmethod.

  method get_source.
    append 'interface zif_abapgit_test_intf' to rt_source.
    append '  public.' to rt_source.
    append '  methods:' to rt_source.
    append '    one_method' to rt_source.
    append '      returning value(rv_value) type string.' to rt_source.
    append '' to rt_source.
    append 'endinterface.' to rt_source.
  endmethod.

endclass.
