CLASS zcl_abapgit_object_trul DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_xml_tag_name TYPE string VALUE 'XML_DATA'.

    METHODS parse_xml
      IMPORTING
        iv_xml        TYPE string
      RETURNING
        VALUE(ri_doc) TYPE REF TO if_ixml_document.

    METHODS render_xml
      IMPORTING
        ii_element    TYPE REF TO if_ixml_element
      RETURNING
        VALUE(rv_xml) TYPE string.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_TRUL IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE chuser FROM ('/LTB/TR_HDR') INTO rv_user WHERE id = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA lo_instance TYPE REF TO object.

    CALL METHOD /ltb/cl_tr_standard_rule=>('CREATE')
      EXPORTING
        iv_tr_id    = |{ ms_item-obj_name }|
      RECEIVING
        ro_instance = lo_instance.

    CALL METHOD lo_instance->('DELETE').

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA li_document          TYPE REF TO if_ixml_document.
    DATA li_container_element TYPE REF TO if_ixml_element.
    DATA lv_xml               TYPE string.

    li_document = io_xml->get_raw( ).

    li_container_element = li_document->find_from_name_ns( c_xml_tag_name ).
    lv_xml = render_xml( li_container_element ).

    CALL METHOD /ltb/cl_tr_standard_rule=>('PERSIST_FROM_XML')
      EXPORTING
        iv_xml = lv_xml
        iv_id  = |{ ms_item-obj_name }|.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA lv_flag TYPE c LENGTH 1.

    SELECT SINGLE inactive FROM ('/LTB/TR_HDR') INTO lv_flag WHERE id = ms_item-obj_name.
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

    DATA lo_instance TYPE REF TO object.
    DATA lv_inactive TYPE abap_bool.

    CALL METHOD /ltb/cl_tr_standard_rule=>('CREATE')
      EXPORTING
        iv_tr_id    = |{ ms_item-obj_name }|
      RECEIVING
        ro_instance = lo_instance.

    CALL METHOD lo_instance->('IS_INACTIVE')
      RECEIVING
        rv_inactive = lv_inactive.

    rv_active = boolc( lv_inactive = abap_false ).

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

    CALL METHOD /ltb/cl_tr_standard_rule=>('LOAD_TO_XML')
      EXPORTING
        iv_id  = |{ ms_item-obj_name }|
      RECEIVING
        rv_xml = lv_xml.

    li_element = parse_xml( lv_xml )->get_root_element( ).

    io_xml->add(
      iv_name = 'NAME'
      ig_data = ms_item-obj_name ).

    io_xml->add_xml(
      iv_name = c_xml_tag_name
      ii_xml  = li_element ).

  ENDMETHOD.


  METHOD render_xml.

    DATA li_stream   TYPE REF TO if_ixml_ostream.
    DATA li_document TYPE REF TO if_ixml_document.


    li_document = cl_ixml=>create( )->create_document( ).
    li_stream = cl_ixml=>create( )->create_stream_factory( )->create_ostream_cstring( rv_xml ).
    li_document->append_child( ii_element ).

    cl_ixml=>create( )->create_renderer(
        document = li_document
        ostream  = li_stream
    )->render( ).

  ENDMETHOD.
ENDCLASS.
