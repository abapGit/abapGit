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
        IMPORTING data          TYPE zif_abapgit_aff_intf_v1=>ty_main
        RETURNING VALUE(result) TYPE string_table
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
              iv_val  = data
      )->map( zcl_abapgit_ajson_mapping=>create_to_camel_case( )
      )->filter( zcl_abapgit_ajson_filter_lib=>create_empty_filter( ) ).

    lo_ajson->delete( '/category/' ).
    lo_ajson->delete( '/proxy/' ).

    lo_cut = NEW zcl_abapgit_json_path( ).
    result = lo_cut->serialize( lo_ajson->stringify( ) ).
  ENDMETHOD.


  METHOD flat_structure.

    ms_data-header-description = 'Text'.

    mt_act = serialize( ms_data ).
    mt_exp = VALUE string_table( ( `$.header.description=Text` ) ).

    cl_abap_unit_assert=>assert_equals( exp = mt_exp
                                        act = mt_act ).

  ENDMETHOD.

  METHOD array.

    ms_data-header-description = 'Text'.
    ms_data-descriptions-methods = VALUE #(
      ( name = 'METH1'
        description = 'Sonne' )
      ( name = 'METH2'
        description = 'Mond' ) ) .


    mt_act = serialize( ms_data ).
    mt_exp = VALUE string_table(
      ( `$.header.description=Text` )
      ( `$.descriptions.methods[?(@.name=='METH1')].description=Sonne` )
      ( `$.descriptions.methods[?(@.name=='METH2')].description=mond` ) ).

    cl_abap_unit_assert=>assert_equals( exp = mt_exp
                                        act = mt_act ).

  ENDMETHOD.

  METHOD array_nested.

    ms_data-header-description = 'Text'.
    ms_data-descriptions-methods = VALUE #(
      ( name = 'METH1'
        description = 'Sonne'
        parameters = VALUE #( ( name = `param1` description = `Parameter A` )
                              ( name = `param2` description = `Parameter B` ) ) )
      ( name = 'METH2'
        description = 'Mond' ) ).

    mt_act = serialize( ms_data ).
    mt_exp = VALUE string_table(
    ( `$.header.description=Text` )
    ( `$.descriptions.methods[?(@.name=='METH1')].description=Sonne` )
    ( `$.descriptions.methods[?(@.name=='METH1')].parameters[?(@.name=='param1')].description=Parameter A` )
    ( `$.descriptions.methods[?(@.name=='METH1')].parameters[?(@.name=='param2')].description=Objekttyp` )
    ( `$.descriptions.methods[?(@.name=='METH2')].description=Mond` ) ).

    cl_abap_unit_assert=>assert_equals( exp = mt_exp
                                        act = mt_act ).

  ENDMETHOD.

ENDCLASS.
