CLASS ltcl_json_path DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mt_act  TYPE string_table,
          mt_exp  TYPE string_table,
          ms_data TYPE zif_abapgit_aff_intf_v1=>ty_main.
    METHODS:
      flat_structure FOR TESTING RAISING cx_static_check,
      array          FOR TESTING RAISING cx_static_check,
      array_nested   FOR TESTING RAISING cx_static_check.
    METHODS:
      serialize
        IMPORTING is_data          TYPE zif_abapgit_aff_intf_v1=>ty_main
        RETURNING VALUE(rt_result) TYPE string_table
        RAISING   zcx_abapgit_ajson_error
                  zcx_abapgit_exception.
ENDCLASS.


CLASS ltcl_json_path IMPLEMENTATION.

  METHOD serialize.
    DATA:
      lo_ajson TYPE REF TO zif_abapgit_ajson,
      lo_cut   TYPE REF TO zcl_abapgit_json_path.


    lo_ajson = zcl_abapgit_ajson=>new( iv_keep_item_order = abap_true
      )->set( iv_path = '/'
              iv_val  = is_data
      )->map( zcl_abapgit_ajson_mapping=>create_to_camel_case( )
      )->filter( zcl_abapgit_ajson_filter_lib=>create_empty_filter( ) ).

    lo_ajson->delete( '/category/' ).
    lo_ajson->delete( '/proxy/' ).

    CREATE OBJECT lo_cut.
    rt_result = lo_cut->serialize( lo_ajson->stringify( ) ).
  ENDMETHOD.


  METHOD flat_structure.
    DATA lv_header_descr TYPE string.
    lv_header_descr = `$.header.description=Text`.

    ms_data-header-description = 'Text'.

    mt_act = serialize( ms_data ).
    APPEND lv_header_descr TO mt_exp.

    cl_abap_unit_assert=>assert_equals( exp = mt_exp
                                        act = mt_act ).

  ENDMETHOD.

  METHOD array.
    DATA lv_header_descr TYPE string.
    DATA lv_descr_meth_1 TYPE string.
    DATA lv_descr_meth_2 TYPE string.
    DATA ls_meth_desc    TYPE zif_abapgit_aff_oo_types_v1=>ty_method.

    lv_header_descr = `$.header.description=Text`.
    lv_descr_meth_1 = `$.descriptions.methods[?(@.name=='METH1')].description=Sonne`.
    lv_descr_meth_2 = `$.descriptions.methods[?(@.name=='METH2')].description=Mond`.

    APPEND lv_header_descr TO mt_exp.
    APPEND lv_descr_meth_1 TO mt_exp.
    APPEND lv_descr_meth_2 TO mt_exp.


    ms_data-header-description = 'Text'.

    ls_meth_desc-name = `METH1`.
    ls_meth_desc-description = `Sonne`.
    APPEND ls_meth_desc TO ms_data-descriptions-methods.
    CLEAR ls_meth_desc.
    ls_meth_desc-name = `METH2`.
    ls_meth_desc-description = `Mond`.
    APPEND ls_meth_desc TO ms_data-descriptions-methods.


    mt_act = serialize( ms_data ).

    cl_abap_unit_assert=>assert_equals( exp = mt_exp
                                        act = mt_act ).

  ENDMETHOD.

  METHOD array_nested.
    DATA lv_header_descr TYPE string.
    DATA lv_descr_meth_1 TYPE string.
    DATA lv_descr_meth_1_param_1 TYPE string.
    DATA lv_descr_meth_1_param_2 TYPE string.
    DATA lv_descr_meth_2 TYPE string.
    DATA ls_meth_desc    TYPE zif_abapgit_aff_oo_types_v1=>ty_method.
    DATA ls_meth_param   TYPE zif_abapgit_aff_oo_types_v1=>ty_component_description.

    lv_header_descr = `$.header.description=Text`.
    lv_descr_meth_1 = `$.descriptions.methods[?(@.name=='METH1')].description=Sonne`.
    lv_descr_meth_1_param_1 =
     `$.descriptions.methods[?(@.name=='METH1')].parameters[?(@.name=='param1')].description=Parameter A`.
    lv_descr_meth_1_param_2 =
      `$.descriptions.methods[?(@.name=='METH1')].parameters[?(@.name=='param2')].description=Parameter B`.
    lv_descr_meth_2 = `$.descriptions.methods[?(@.name=='METH2')].description=Mond`.

    APPEND lv_header_descr TO mt_exp.
    APPEND lv_descr_meth_1 TO mt_exp.
    APPEND lv_descr_meth_1_param_1 TO mt_exp.
    APPEND lv_descr_meth_1_param_2 TO mt_exp.
    APPEND lv_descr_meth_2 TO mt_exp.



    ls_meth_param-name = 'param1'.
    ls_meth_param-description = 'Parameter A'.
    APPEND ls_meth_param TO ls_meth_desc-parameters.

    ls_meth_param-name = 'param2'.
    ls_meth_param-description = 'Parameter B'.
    APPEND ls_meth_param TO ls_meth_desc-parameters.

    ls_meth_desc-name = `METH1`.
    ls_meth_desc-description = `Sonne`.
    APPEND ls_meth_desc TO ms_data-descriptions-methods.
    CLEAR ls_meth_desc.

    ls_meth_desc-name = `METH2`.
    ls_meth_desc-description = `Mond`.
    APPEND ls_meth_desc TO ms_data-descriptions-methods.

    ms_data-header-description = 'Text'.

    mt_act = serialize( ms_data ).

    cl_abap_unit_assert=>assert_equals( exp = mt_exp
                                        act = mt_act ).

  ENDMETHOD.

ENDCLASS.
