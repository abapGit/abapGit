CLASS zcl_abapgit_object_sfpf DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

    CLASS-METHODS:
      fix_oref
        IMPORTING ii_document TYPE REF TO if_ixml_document.

  PRIVATE SECTION.
    METHODS:
      load
        RETURNING VALUE(ri_wb_form) TYPE REF TO if_fp_wb_form
        RAISING   zcx_abapgit_exception,
      form_to_xstring
        RETURNING VALUE(rv_xstr) TYPE xstring
        RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_object_sfpf IMPLEMENTATION.


  METHOD fix_oref.

    DATA: li_iterator TYPE REF TO if_ixml_node_iterator,
          lv_new      TYPE n LENGTH 3,
          lv_old      TYPE string,
          lt_map      TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          li_attr_map TYPE REF TO if_ixml_named_node_map,
          li_attr     TYPE REF TO if_ixml_node,
          li_node     TYPE REF TO if_ixml_node.

    DEFINE _lookup.
      READ TABLE lt_map FROM &1 TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND &1 TO lt_map.
        READ TABLE lt_map FROM &1 TRANSPORTING NO FIELDS.
      ENDIF.
      lv_new = sy-tabix + 100.
    END-OF-DEFINITION.


    li_iterator = ii_document->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.
      li_attr_map = li_node->get_attributes( ).

      IF li_attr_map IS BOUND.
        li_attr = li_attr_map->get_named_item_ns( 'href' ).
        IF li_attr IS BOUND.
          lv_old = li_attr->get_value( ).
          IF lv_old(2) = '#o'.
            _lookup lv_old+1.
            li_attr->set_value( '#o' && lv_new ).
          ENDIF.
        ENDIF.

        li_attr = li_attr_map->get_named_item_ns( 'id' ).
        IF li_attr IS BOUND.
          lv_old = li_attr->get_value( ).
          IF lv_old(1) = 'o'.
            _lookup lv_old.
            li_attr->set_value( 'o' && lv_new ).
          ENDIF.
        ENDIF.
      ENDIF.

      li_node = li_iterator->get_next( ).
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

  METHOD zif_abapgit_object~is_locked.

    DATA: lv_object TYPE seqg3-garg.

    lv_object = |{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '                              '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EFPFORM'
                                            iv_argument    = lv_object ).


  ENDMETHOD.

ENDCLASS.
