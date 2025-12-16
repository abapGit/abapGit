CLASS zcl_abapgit_object_trul DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS parse_xml
      IMPORTING
        iv_xml        TYPE string
      RETURNING
        VALUE(ri_doc) TYPE REF TO if_ixml_document.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_TRUL IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE chuser FROM /ltb/tr_hdr INTO rv_user WHERE id = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

* TODO

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

* TODO

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA lv_id TYPE /ltb/tr_hdr-id.

    SELECT SINGLE id FROM /ltb/tr_hdr INTO lv_id WHERE id = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.

    DATA lo_instance TYPE REF TO /ltb/cl_tr_standard_rule.

    lo_instance ?= /ltb/cl_tr_standard_rule=>create( |{ ms_item-obj_name }| ).
    rv_active = boolc( lo_instance->is_inactive( ) = abap_false ).

  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for(
      iv_lock_object = '/LTB/E_TR_HDR'
      iv_argument    = ms_item-obj_name ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD parse_xml.

    DATA li_factory TYPE REF TO if_ixml_stream_factory.
    DATA li_istream TYPE REF TO if_ixml_istream.
    DATA li_parser  TYPE REF TO if_ixml_parser.
    DATA li_ixml    TYPE REF TO if_ixml.
    DATA lv_subrc   TYPE i.

    li_ixml = cl_ixml=>create( ).
    li_factory = li_ixml->create_stream_factory( ).
    ri_doc = li_ixml->create_document( ).
    li_istream = li_factory->create_istream_string( iv_xml ).
    li_parser = li_ixml->create_parser(
      stream_factory = li_factory
      istream        = li_istream
      document       = ri_doc ).
    li_parser->add_strip_space_element( ).
    lv_subrc = li_parser->parse( ).
    ASSERT lv_subrc = 0.
    li_istream->close( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA lv_xml     TYPE string.
    DATA li_element TYPE REF TO if_ixml_element.

    lv_xml = /ltb/cl_tr_standard_rule=>load_to_xml( |{ ms_item-obj_name }| ).

    li_element = parse_xml( lv_xml )->get_root_element( ).

    io_xml->add( iv_name = 'DUMMY'
                 ig_data = 2 ).

    io_xml->add_xml(
      iv_name = 'XML_DATA'
      ii_xml  = li_element ).

  ENDMETHOD.
ENDCLASS.
