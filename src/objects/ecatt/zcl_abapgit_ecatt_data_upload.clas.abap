CLASS zcl_abapgit_ecatt_data_upload DEFINITION
  PUBLIC
  INHERITING FROM cl_apl_ecatt_data_upload
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      z_set_stream_for_upload
        IMPORTING
          im_xml TYPE xstring.

  PROTECTED SECTION.
    METHODS:
      upload_data_from_stream REDEFINITION.

  PRIVATE SECTION.
    DATA: mv_external_xml TYPE xstring.

ENDCLASS.



CLASS zcl_abapgit_ecatt_data_upload IMPLEMENTATION.


  METHOD upload_data_from_stream.

    " Downport

    DATA:
      lt_eing          TYPE etxml_xline_tabtype,
      lo_xml           TYPE REF TO cl_apl_ecatt_xml,
      lv_xstr          TYPE xstring,
      lv_nc_xmlref_typ TYPE REF TO if_ixml_node_collection,
      lv_n_xmlref_typ  TYPE REF TO if_ixml_node,
      lv_index         TYPE i VALUE 0,
      lv_count         TYPE i.

    lv_xstr = mv_external_xml.

    lo_xml = cl_apl_ecatt_xml=>create( im_type = if_apl_ecatt_xml=>co_xml ).

* whitespace stripping needs a namespace
* remove white spaces only at the time of upload
    lo_xml->stream_to_dom( im_xstream            = lv_xstr
                           im_ignore_white_space = 'X'
                           im_uri                = cl_apl_xml_const=>schema_uri ).

    lo_xml->get_attributes(
      IMPORTING
        ex_dom = template_over_all ).

* MD: Workaround, because nodes starting with "XML" are not allowed
    lv_nc_xmlref_typ = template_over_all->get_elements_by_tag_name_ns(
                      'XMLREF_TYP' ).                       "#EC NOTEXT
    lv_count = lv_nc_xmlref_typ->get_length( ).
    WHILE lv_index LT lv_count.
      lv_n_xmlref_typ = lv_nc_xmlref_typ->get_item( lv_index ).
      lv_n_xmlref_typ->set_name( 'X-MLREF_TYP' ).
      lv_index = lv_index + 1.
    ENDWHILE.

    FREE: lt_eing.
    CLEAR: lo_xml, lv_xstr.

  ENDMETHOD.


  METHOD z_set_stream_for_upload.

    " donwnpoort from CL_ABAPGIT_ECATT_DATA_UPLOAD SET_STREAM_FOR_UPLOAD
    mv_external_xml = im_xml.

  ENDMETHOD.
ENDCLASS.
