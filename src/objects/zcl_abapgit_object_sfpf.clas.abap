CLASS zcl_abapgit_object_sfpf DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .

    ALIASES mo_files
      FOR zif_abapgit_object~mo_files .

    CLASS-METHODS fix_oref
      IMPORTING
        !ii_document TYPE REF TO if_ixml_document
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      load
        RETURNING VALUE(ri_wb_form) TYPE REF TO if_fp_wb_form
        RAISING   zcx_abapgit_exception,
      form_to_xstring
        RETURNING VALUE(rv_xstr) TYPE xstring
        RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_SFPF IMPLEMENTATION.


  METHOD fix_oref.

* During serialization of a SFPF / SFPI object the interface hierarchy
* is represented by attributes "id" and "href", where the id looks
* like "o<number>" and href like "#o<number>". Every run of
* serialization generates a new <number> in these  attributes, that
* leads to differences even by comparing of untouched forms.
* The purpose of this method is to renumber the id's consequentially
* and therefore to avoid fictive differences.

* NB: As the method iterator->get_next() works quite slowly,
*     it is better to collect all attributes in a cache table
*     instead of implementing of a nested loop using get_next().

    DATA:
      li_iterator TYPE REF TO if_ixml_node_iterator,
      li_elem     TYPE REF TO if_ixml_element,
      lv_new      TYPE string,
      lv_old      TYPE string,
      lv_count    TYPE i,
      BEGIN OF ls_attr_href,
        val  TYPE string,
        attr TYPE REF TO if_ixml_attribute,
      END OF ls_attr_href,
      lt_attr_href LIKE SORTED TABLE OF ls_attr_href WITH NON-UNIQUE KEY val.

    FIELD-SYMBOLS <ls_attr_href> LIKE LINE OF lt_attr_href.

*   Collect all attributes href='#o...' in the cache table
    li_iterator = ii_document->create_iterator_filtered(
      ii_document->create_filter_and(
        filter1 = ii_document->create_filter_node_type( if_ixml_node=>co_node_element )
        filter2 = ii_document->create_filter_attribute( 'href' ) ) ).
    li_elem ?= li_iterator->get_next( ).
    WHILE li_elem IS NOT INITIAL.
      ls_attr_href-attr = li_elem->get_attribute_node( 'href' ).
      ls_attr_href-val = ls_attr_href-attr->get_value( ).
      IF ls_attr_href-val CP '##o*'.
        INSERT ls_attr_href INTO TABLE lt_attr_href.
      ENDIF.
      li_elem ?= li_iterator->get_next( ).
    ENDWHILE.

*   Renumber id='o...' attributes
    li_iterator = ii_document->create_iterator_filtered(
      ii_document->create_filter_and(
        filter1 = ii_document->create_filter_node_type( if_ixml_node=>co_node_element  )
        filter2 = ii_document->create_filter_attribute( 'id' ) ) ).
    li_elem ?= li_iterator->get_next( ).
    WHILE li_elem IS NOT INITIAL.
      lv_old = li_elem->get_attribute( 'id' ).
      IF lv_old CP 'o*'.
        lv_count = lv_count + 1.
        lv_new = |o{ lv_count }|.
*       Rewrite id
        IF li_elem->set_attribute( name = 'id' value = lv_new ) IS NOT INITIAL.
          zcx_abapgit_exception=>raise( 'SFPF error, FIX_OREF' ).
        ENDIF.
*       Update references
        LOOP AT lt_attr_href ASSIGNING <ls_attr_href> WHERE val = '#' && lv_old.
          IF <ls_attr_href>-attr->set_value( '#' && lv_new ) IS NOT INITIAL.
            zcx_abapgit_exception=>raise( 'SFPF error, FIX_OREF' ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      li_elem ?= li_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.


  METHOD form_to_xstring.

    DATA: li_fp_form TYPE REF TO if_fp_form,
          li_wb_form TYPE REF TO if_fp_wb_form.


    TRY.
        li_wb_form = load( ).
        li_fp_form ?= li_wb_form->get_object( ).
        rv_xstr = cl_fp_helper=>convert_form_to_xstring( li_fp_form ).
      CATCH cx_fp_api.
        zcx_abapgit_exception=>raise( 'SFPF error, form_to_xstring' ).
    ENDTRY.

  ENDMETHOD.


  METHOD load.

    DATA: lv_name TYPE fpname.


    lv_name = ms_item-obj_name.

    TRY.
        ri_wb_form = cl_fp_wb_form=>load( lv_name ).
      CATCH cx_fp_api.
        zcx_abapgit_exception=>raise( 'SFPF error, load' ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE lastuser FROM fplayout
      INTO rv_user
      WHERE name = ms_item-obj_name
      AND state = 'A'.
    IF rv_user IS INITIAL.
      SELECT SINGLE firstuser FROM fplayout
        INTO rv_user
        WHERE name = ms_item-obj_name
        AND state = 'A'.
    ENDIF.
    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_name    TYPE fpname,
          lo_wb_form TYPE REF TO cl_fp_wb_form.


    lo_wb_form ?= load( ).

    lv_name = ms_item-obj_name.

    TRY.
        lo_wb_form->delete( lv_name ).
      CATCH cx_fp_api.
        zcx_abapgit_exception=>raise( 'SFPI error, delete' ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_xstr      TYPE xstring,
          lv_name      TYPE fpname,
          li_wb_object TYPE REF TO if_fp_wb_form,
          li_form      TYPE REF TO if_fp_form.


    lv_name = ms_item-obj_name.
    lv_xstr = cl_ixml_80_20=>render_to_xstring( io_xml->get_raw( ) ).

    TRY.
        li_form = cl_fp_helper=>convert_xstring_to_form( lv_xstr ).

        IF zif_abapgit_object~exists( ) = abap_true.
          cl_fp_wb_form=>delete( lv_name ).
        ENDIF.

        tadir_insert( iv_package ).
        li_wb_object = cl_fp_wb_form=>create( i_name = lv_name
                                              i_form = li_form ).
        li_wb_object->save( ).
        li_wb_object->free( ).
      CATCH cx_fp_api.
        zcx_abapgit_exception=>raise( 'SFPF error, deserialize' ).
    ENDTRY.

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_name TYPE fpname.

    SELECT SINGLE name FROM fplayout
      INTO lv_name
      WHERE name = ms_item-obj_name
      AND state = 'A'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    DATA: lv_object TYPE seqg3-garg.

    lv_object = |{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '                              '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EFPFORM'
                                            iv_argument    = lv_object ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation   = 'SHOW'
        object_name = ms_item-obj_name
        object_type = ms_item-obj_type.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_xstr     TYPE xstring,
          li_document TYPE REF TO if_ixml_document.


    lv_xstr = form_to_xstring( ).
    li_document = cl_ixml_80_20=>parse_to_document( stream_xstring = lv_xstr ).
    fix_oref( li_document ).
    io_xml->set_raw( li_document->get_root_element( ) ).

  ENDMETHOD.
ENDCLASS.
