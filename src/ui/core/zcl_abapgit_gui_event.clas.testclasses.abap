CLASS ltcl_event DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.

    METHODS query_wrong_data FOR TESTING RAISING zcx_abapgit_exception.
    METHODS form_wrong_data FOR TESTING RAISING zcx_abapgit_exception.
    METHODS query FOR TESTING RAISING zcx_abapgit_exception.
    METHODS form_data FOR TESTING RAISING zcx_abapgit_exception.
    METHODS immutability FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_event IMPLEMENTATION.

  METHOD query_wrong_data.

    DATA li_cut TYPE REF TO zif_abapgit_gui_event.
    DATA lo_map TYPE REF TO zcl_abapgit_string_map.

    CREATE OBJECT li_cut TYPE zcl_abapgit_gui_event
      EXPORTING
        iv_action = 'XXX'
        iv_getdata = 'not_a_param'.

    lo_map = li_cut->query( ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->size( )
      exp = 0 ).

  ENDMETHOD.

  METHOD form_wrong_data.

    DATA li_cut TYPE REF TO zif_abapgit_gui_event.
    DATA lo_map TYPE REF TO zcl_abapgit_string_map.

    CREATE OBJECT li_cut TYPE zcl_abapgit_gui_event
      EXPORTING
        iv_action = 'XXX'.

    lo_map = li_cut->form_data( ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->size( )
      exp = 0 ).

  ENDMETHOD.

  METHOD query.

    DATA li_cut TYPE REF TO zif_abapgit_gui_event.
    DATA lo_map TYPE REF TO zcl_abapgit_string_map.

    CREATE OBJECT li_cut TYPE zcl_abapgit_gui_event
      EXPORTING
        iv_action = 'XXX'
        iv_getdata = 'a=b&b=c'.

    " Cross check just in case
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->form_data( )->size( )
      exp = 0 ).

    lo_map = li_cut->query( ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->size( )
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'a' )
      exp = 'b' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'b' )
      exp = 'c' ).

    " Case insensitivity
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'A' )
      exp = 'b' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'B' )
      exp = 'c' ).

  ENDMETHOD.

  METHOD form_data.

    DATA li_cut TYPE REF TO zif_abapgit_gui_event.
    DATA lo_map TYPE REF TO zcl_abapgit_string_map.
    DATA lt_postdata TYPE zif_abapgit_html_viewer=>ty_post_data.

    APPEND 'a=b&b=c' TO lt_postdata.
    CREATE OBJECT li_cut TYPE zcl_abapgit_gui_event
      EXPORTING
        iv_action   = 'XXX'
        it_postdata = lt_postdata.

    " Cross check just in case
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->query( )->size( )
      exp = 0 ).

    lo_map = li_cut->form_data( ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->size( )
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'a' )
      exp = 'b' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'b' )
      exp = 'c' ).

    " Case insensitivity
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->size( )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'A' )
      exp = 'b' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'B' )
      exp = 'c' ).

  ENDMETHOD.

  METHOD immutability.

    DATA li_cut TYPE REF TO zif_abapgit_gui_event.
    DATA lo_x TYPE REF TO zcx_abapgit_exception.

    CREATE OBJECT li_cut TYPE zcl_abapgit_gui_event
      EXPORTING
        iv_getdata = 'a=b&b=c'
        iv_action  = 'XXX'.

    TRY.
        li_cut->form_data( )->set(
          iv_key = 'x'
          iv_val = 'y' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lo_x.
        cl_abap_unit_assert=>assert_char_cp(
          act = lo_x->get_text( )
          exp = '*immutable*' ).
    ENDTRY.

    TRY.
        li_cut->query( )->set(
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
