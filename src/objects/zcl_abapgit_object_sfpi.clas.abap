CLASS zcl_abapgit_object_sfpi DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PRIVATE SECTION.
    METHODS:
      load
        RETURNING VALUE(ri_wb_interface) TYPE REF TO if_fp_wb_interface
        RAISING zcx_abapgit_exception,
      interface_to_xstring
        RETURNING VALUE(rv_xstr) TYPE xstring
        RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS zcl_abapgit_object_sfpi IMPLEMENTATION.

  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "zif_abapgit_object~has_changed_since

  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE lastuser FROM fpinterface
      INTO rv_user
      WHERE name = ms_item-obj_name
      AND state = 'A'.
    IF rv_user IS INITIAL.
      SELECT SINGLE firstuser FROM fpinterface
        INTO rv_user
        WHERE name = ms_item-obj_name
        AND state = 'A'.
    ENDIF.
    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "zif_abapgit_object~get_metadata

  METHOD zif_abapgit_object~exists.

    DATA: lv_name TYPE fpinterface-name.

    SELECT SINGLE name FROM fpinterface
      INTO lv_name
      WHERE name = ms_item-obj_name
      AND state = 'A'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "zif_abapgit_object~exists

  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation   = 'SHOW'
        object_name = ms_item-obj_name
        object_type = ms_item-obj_type.

  ENDMETHOD.                    "jump

  METHOD zif_abapgit_object~delete.

    DATA: lv_name TYPE fpname,
          lo_wb_interface TYPE REF TO cl_fp_wb_interface.


    lo_wb_interface ?= load( ).

    lv_name = ms_item-obj_name.

    TRY.
        lo_wb_interface->delete( lv_name ).
      CATCH cx_fp_api.
        zcx_abapgit_exception=>raise( 'SFPI error, delete' ).
    ENDTRY.

  ENDMETHOD.                    "delete

  METHOD load.

    DATA: lv_name TYPE fpname.


    lv_name = ms_item-obj_name.

    TRY.
        ri_wb_interface = cl_fp_wb_interface=>load( lv_name ).
      CATCH cx_fp_api.
        zcx_abapgit_exception=>raise( 'SFPI error, load' ).
    ENDTRY.

  ENDMETHOD.

  METHOD interface_to_xstring.

    DATA: li_fp_interface TYPE REF TO if_fp_interface,
          li_wb_interface TYPE REF TO if_fp_wb_interface.


    TRY.
        li_wb_interface = load( ).
        li_fp_interface ?= li_wb_interface->get_object( ).
        rv_xstr = cl_fp_helper=>convert_interface_to_xstring( li_fp_interface ).
      CATCH cx_fp_api.
        zcx_abapgit_exception=>raise( 'SFPI error, interface_to_xstring' ).
    ENDTRY.

  ENDMETHOD.

  METHOD zif_abapgit_object~serialize.

    DATA: lv_xstr     TYPE xstring,
          li_document TYPE REF TO if_ixml_document.


    lv_xstr = interface_to_xstring( ).
    li_document = cl_ixml_80_20=>parse_to_document( stream_xstring = lv_xstr ).
    zcl_abapgit_object_sfpf=>fix_oref( li_document ).
    io_xml->set_raw( li_document->get_root_element( ) ).

  ENDMETHOD.                    "serialize

  METHOD zif_abapgit_object~deserialize.

    DATA: lv_xstr      TYPE xstring,
          lv_name      TYPE fpname,
          li_wb_object TYPE REF TO if_fp_wb_interface,
          li_interface TYPE REF TO if_fp_interface.


    lv_name = ms_item-obj_name.
    lv_xstr = cl_ixml_80_20=>render_to_xstring( io_xml->get_raw( ) ).

    TRY.
        li_interface = cl_fp_helper=>convert_xstring_to_interface( lv_xstr ).
        tadir_insert( iv_package ).
        li_wb_object = cl_fp_wb_interface=>create( i_name      = lv_name
                                                   i_interface = li_interface ).
        li_wb_object->save( ).
        li_wb_object->free( ).
      CATCH cx_fp_api.
        zcx_abapgit_exception=>raise( 'SFPI error, deserialize' ).
    ENDTRY.

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.

ENDCLASS.                    "zcl_abapgit_object_doma IMPLEMENTATION
