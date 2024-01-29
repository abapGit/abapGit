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

    ms_data-header-description = 'Interface zum BAdI: BADI_TADIR_CHANGED'.

    mt_act = serialize( ms_data ).
    mt_exp = VALUE string_table( ( `$.header.description=Interface zum BAdI: BADI_TADIR_CHANGED` ) ).

    cl_abap_unit_assert=>assert_equals( exp = mt_exp
                                        act = mt_act ).

  ENDMETHOD.

  METHOD array.


    ms_data-header-description = 'Interface zum BAdI: BADI_TADIR_CHANGED'.
    ms_data-descriptions-methods = VALUE #( ( name = 'AFTER_TADIR_CHANGED'
                                              description = 'Objektkatalog geändert (insert,change,delete)' )
                                            ( name = 'BEFORE_TADIR_CHANGED'
                                              description = 'Objektkatalog wird geändert (insert,change,delete)' ) ) .

    mt_act = serialize( ms_data ).
    mt_exp = VALUE string_table(
      ( `$.header.description=Interface zum BAdI: BADI_TADIR_CHANGED` )
      ( `$.descriptions.methods[?(@.name=='AFTER_TADIR_CHANGED')].description=Objektkatalog geändert (insert,change,delete)` )
      ( `$.descriptions.methods[?(@.name=='BEFORE_TADIR_CHANGED')].description=Objektkatalog wird geändert (insert,change,delete)` ) ).

    cl_abap_unit_assert=>assert_equals( exp = mt_exp
                                        act = mt_act ).

  ENDMETHOD.

  METHOD array_nested.

    ms_data-header-description = 'Interface zum BAdI: BADI_TADIR_CHANGED'.
    ms_data-descriptions-methods = VALUE #( ( name = 'AFTER_TADIR_CHANGED'
                                              description = 'Objektkatalog geändert (insert,change,delete)'
                                              parameters = VALUE #( ( name = `IM_SOBJ_NAME` description = `Objektname im Objektkatalog` )
                                                                    ( name = `IM_TROBJTYPE` description = `Objekttyp` ) ) )
                                            ( name = 'BEFORE_TADIR_CHANGED'
                                              description = 'Objektkatalog wird geändert (insert,change,delete)' ) ).

    mt_act = serialize( ms_data ).
    mt_exp = VALUE string_table(
      ( `$.header.description=Interface zum BAdI: BADI_TADIR_CHANGED` )
      ( `$.descriptions.methods[?(@.name=='AFTER_TADIR_CHANGED')].description=Objektkatalog geändert (insert,change,delete)` )
      ( `$.descriptions.methods[?(@.name=='AFTER_TADIR_CHANGED')].parameters[?(@.name=='IM_SOBJ_NAME')].description=Objektname im Objektkatalog` )
      ( `$.descriptions.methods[?(@.name=='AFTER_TADIR_CHANGED')].parameters[?(@.name=='IM_TROBJTYPE')].description=Objekttyp` )
      ( `$.descriptions.methods[?(@.name=='BEFORE_TADIR_CHANGED')].description=Objektkatalog wird geändert (insert,change,delete)` ) ).

    cl_abap_unit_assert=>assert_equals( exp = mt_exp
                                        act = mt_act ).

  ENDMETHOD.

ENDCLASS.
