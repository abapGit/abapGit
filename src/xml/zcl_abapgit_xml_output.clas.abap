CLASS zcl_abapgit_xml_output DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_xml
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_xml_output.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mi_raw TYPE REF TO if_ixml_element .
    DATA ms_i18n_params TYPE zif_abapgit_definitions=>ty_i18n_params .

    METHODS build_asx_node
      RETURNING
        VALUE(ri_element) TYPE REF TO if_ixml_element .
ENDCLASS.



CLASS zcl_abapgit_xml_output IMPLEMENTATION.


  METHOD build_asx_node.

    DATA: li_attr TYPE REF TO if_ixml_attribute.


    ri_element = mi_xml_doc->create_element_ns(
      name   = 'abap'
      prefix = 'asx' ).

    li_attr = mi_xml_doc->create_attribute_ns( 'version' ).
    li_attr->if_ixml_node~set_value( '1.0' ).
    ri_element->set_attribute_node_ns( li_attr ).

    li_attr = mi_xml_doc->create_attribute_ns(
      name   = 'asx'
      prefix = 'xmlns' ).
    li_attr->if_ixml_node~set_value( 'http://www.sap.com/abapxml' ).
    ri_element->set_attribute_node_ns( li_attr ).

  ENDMETHOD.


  METHOD zif_abapgit_xml_output~add.

    DATA: li_node TYPE REF TO if_ixml_node,
          li_doc  TYPE REF TO if_ixml_document,
          lt_stab TYPE abap_trans_srcbind_tab.

    FIELD-SYMBOLS: <ls_stab> LIKE LINE OF lt_stab.


    ASSERT NOT iv_name IS INITIAL.

    IF ig_data IS INITIAL.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO lt_stab ASSIGNING <ls_stab>.
    <ls_stab>-name = iv_name.
    GET REFERENCE OF ig_data INTO <ls_stab>-value.

    li_doc = cl_ixml=>create( )->create_document( ).

    CALL TRANSFORMATION id
      OPTIONS initial_components = 'suppress'
      SOURCE (lt_stab)
      RESULT XML li_doc.

    li_node = mi_xml_doc->get_root( )->get_first_child( ).
    IF li_node IS BOUND.
      mi_xml_doc->get_root( )->get_first_child( )->get_first_child( )->append_child(
        li_doc->get_root( )->get_first_child( )->get_first_child( )->get_first_child( ) ).
    ELSE.
      mi_xml_doc->get_root( )->append_child( li_doc->get_root( )->get_first_child( ) ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_xml_output~add_xml.

    DATA: li_element TYPE REF TO if_ixml_element.

    li_element = mi_xml_doc->create_element( iv_name ).
    li_element->append_child( ii_xml ).

    mi_xml_doc->get_root( )->get_first_child( )->get_first_child( )->append_child( li_element ).

  ENDMETHOD.


  METHOD zif_abapgit_xml_output~i18n_params.

    IF is_i18n_params IS SUPPLIED.
      ms_i18n_params = is_i18n_params.
    ENDIF.

    rs_i18n_params = ms_i18n_params.

  ENDMETHOD.


  METHOD zif_abapgit_xml_output~render.

    DATA: li_git  TYPE REF TO if_ixml_element,
          li_abap TYPE REF TO if_ixml_element.


    IF mi_raw IS INITIAL.
      li_abap ?= mi_xml_doc->get_root( )->get_first_child( ).
      mi_xml_doc->get_root( )->remove_child( li_abap ).
      IF li_abap IS INITIAL.
        li_abap = build_asx_node( ).
      ENDIF.
    ELSE.
      li_abap = mi_raw.
    ENDIF.

    li_git = mi_xml_doc->create_element( c_abapgit_tag ).
    li_git->set_attribute( name = c_attr_version
                           value = zif_abapgit_version=>c_xml_version ).
    IF NOT is_metadata IS INITIAL.
      li_git->set_attribute( name  = c_attr_serializer
                             value = is_metadata-class ).
      li_git->set_attribute( name  = c_attr_serializer_version
                             value = is_metadata-version ).
    ENDIF.
    li_git->append_child( li_abap ).
    mi_xml_doc->get_root( )->append_child( li_git ).

    rv_xml = to_xml( iv_normalize ).

  ENDMETHOD.


  METHOD zif_abapgit_xml_output~set_raw.
    mi_raw = ii_raw.
  ENDMETHOD.
ENDCLASS.
