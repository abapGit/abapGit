CLASS ltcl_event DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.

    METHODS query FOR TESTING RAISING zcx_abapgit_exception.
    METHODS form_data FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_event IMPLEMENTATION.

  METHOD query.

    DATA li_cut TYPE REF TO zif_abapgit_gui_event.
    DATA lo_map TYPE REF TO zcl_abapgit_string_map.
    DATA lo_x TYPE REF TO zcx_abapgit_exception.

    CREATE OBJECT li_cut TYPE zcl_abapgit_gui_event
      EXPORTING
        iv_action = 'XXX'
        iv_getdata = 'not_a_param'.

    lo_map = li_cut->query( ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->size( )
      exp = 0 ).

    CREATE OBJECT li_cut TYPE zcl_abapgit_gui_event
      EXPORTING
        iv_action = 'XXX'
        iv_getdata = 'a=b&b=c'.

    " Cross check just in case
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->form_data( )->size( )
      exp = 0 ).

    lo_map = li_cut->query( iv_upper_cased = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->size( )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'a' )
      exp = 'b' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'b' )
      exp = 'c' ).

    lo_map = li_cut->query( iv_upper_cased = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->size( )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'A' )
      exp = 'b' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'B' )
      exp = 'c' ).

    " Check defaults
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->query( )->get( 'A' )
      exp = 'b' ).

    TRY.
        lo_map->set(
          iv_key = 'x'
          iv_val = 'y' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lo_x.
        cl_abap_unit_assert=>assert_char_cp(
          act = lo_x->get_text( )
          exp = '*immutable*' ).
    ENDTRY.

  ENDMETHOD.

  METHOD form_data.

    DATA li_cut TYPE REF TO zif_abapgit_gui_event.
    DATA lo_map TYPE REF TO zcl_abapgit_string_map.
    DATA lo_x TYPE REF TO zcx_abapgit_exception.
    DATA lt_postdata TYPE cnht_post_data_tab.

    CREATE OBJECT li_cut TYPE zcl_abapgit_gui_event
      EXPORTING
        iv_action = 'XXX'.

    lo_map = li_cut->form_data( ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->size( )
      exp = 0 ).

    APPEND 'a=b&b=c' TO lt_postdata.
    CREATE OBJECT li_cut TYPE zcl_abapgit_gui_event
      EXPORTING
        iv_action   = 'XXX'
        it_postdata = lt_postdata.

    " Cross check just in case
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->query( )->size( )
      exp = 0 ).

    lo_map = li_cut->form_data( iv_upper_cased = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->size( )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'a' )
      exp = 'b' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'b' )
      exp = 'c' ).

    lo_map = li_cut->form_data( iv_upper_cased = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->size( )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'A' )
      exp = 'b' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'B' )
      exp = 'c' ).

    " Check defaults
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->form_data( )->get( 'a' )
      exp = 'b' ).

    TRY.
        lo_map->set(
          iv_key = 'x'
          iv_val = 'y' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lo_x.
        cl_abap_unit_assert=>assert_char_cp(
          act = lo_x->get_text( )
          exp = '*immutable*' ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
