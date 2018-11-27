CLASS zcl_abapgit_ecatt_helper DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      build_xml_of_object
        IMPORTING
          im_object_name     TYPE  etobj_name
          im_object_version  TYPE  etobj_ver
          im_object_type     TYPE  etobj_type
          io_download        TYPE REF TO cl_apl_ecatt_download
        EXPORTING
          ex_xml_stream      TYPE  xstring
          ex_xml_stream_size TYPE  int4
        RAISING
          zcx_abapgit_exception,

      download_data
        IMPORTING
          ii_template_over_all TYPE REF TO if_ixml_document
        EXPORTING
          ev_xml_stream        TYPE xstring
          ev_xml_stream_size   TYPE i,

      upload_data_from_stream
        IMPORTING
          iv_xml_stream               TYPE xstring
        RETURNING
          VALUE(ri_template_over_all) TYPE REF TO if_ixml_document
        RAISING
          cx_ecatt_apl_xml.

  PRIVATE SECTION.
    CONSTANTS:
      co_xml TYPE int4 VALUE 1. " downport of if_apl_ecatt_xml=>co_xml

ENDCLASS.



CLASS ZCL_ABAPGIT_ECATT_HELPER IMPLEMENTATION.


  METHOD build_xml_of_object.

    " downport of CL_APL_ECATT_DOWNLOAD=>BUILD_XML_OF_OBJECT

    DATA: lo_load_help_dummy TYPE REF TO cl_apl_ecatt_load_help,
          lx_ecatt           TYPE REF TO cx_ecatt_apl,
          lv_text            TYPE string.

    "download method will create the xml stream
    "note: it's the redefined download( ) of each object type specific download, which is called
    TRY.
        CREATE OBJECT lo_load_help_dummy
          EXPORTING
            im_maintain_function = ''.

        io_download->download( im_object_name    = im_object_name
                               im_object_version = im_object_version
                               im_object_type    = im_object_type
                               im_load_help      = lo_load_help_dummy ).

      CATCH cx_ecatt_apl INTO lx_ecatt.
        lv_text = lx_ecatt->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
        " CATCH cx_ecatt_ui_attachment. " Doesn't exist in 702
      CATCH cx_ecatt.
        "will never be raised from download, when called with mv_generate_xml_no_download = 'X'.
    ENDTRY.

    CALL METHOD io_download->('GET_XML_STREAM')
      RECEIVING
        rv_xml_stream = ex_xml_stream.

    CALL METHOD io_download->('GET_XML_STREAM_SIZE')
      RECEIVING
        rv_xml_stream_size = ex_xml_stream_size.

  ENDMETHOD.


  METHOD download_data.

    DATA:
      lo_xml   TYPE REF TO cl_apl_ecatt_xml,
      lv_size  TYPE int4.

    CLEAR: ev_xml_stream,
           ev_xml_stream_size.

    TRY.
        CALL METHOD cl_apl_ecatt_xml=>('CREATE') " doesn't exist in 702
          EXPORTING
            im_type = co_xml
          RECEIVING
            re_xml  = lo_xml.

        lo_xml->set_attributes( im_dom = ii_template_over_all ).

        lo_xml->get_attributes(
          IMPORTING
            ex_size_xstring = lv_size
            ex_xml          = ev_xml_stream ).

        ev_xml_stream_size = lv_size.

      CATCH cx_ecatt_apl_xml.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD upload_data_from_stream.

    DATA:
      lo_xml           TYPE REF TO cl_apl_ecatt_xml,
      lv_xstr          TYPE xstring,
      lv_nc_xmlref_typ TYPE REF TO if_ixml_node_collection,
      lv_n_xmlref_typ  TYPE REF TO if_ixml_node,
      lv_index         TYPE i VALUE 0,
      lv_count         TYPE i.

    lv_xstr = iv_xml_stream.

    CALL METHOD cl_apl_ecatt_xml=>('CREATE') " doesn't exist in 702
      EXPORTING
        im_type = co_xml
      RECEIVING
        re_xml  = lo_xml.

* whitespace stripping needs a namespace
* remove white spaces only at the time of upload
    lo_xml->stream_to_dom( im_xstream            = lv_xstr
                           im_ignore_white_space = 'X'
                           im_uri                = cl_apl_xml_const=>schema_uri ).

    lo_xml->get_attributes(
      IMPORTING
        ex_dom = ri_template_over_all ).

* MD: Workaround, because nodes starting with "XML" are not allowed
    lv_nc_xmlref_typ ?= ri_template_over_all->get_elements_by_tag_name_ns(
                          'XMLREF_TYP' ).                   "#EC NOTEXT
    CALL METHOD lv_nc_xmlref_typ->('GET_LENGTH')  " downport
      RECEIVING
        rval = lv_count.

    WHILE lv_index LT lv_count.
      lv_n_xmlref_typ = lv_nc_xmlref_typ->get_item( lv_index ).
      lv_n_xmlref_typ->set_name( 'X-MLREF_TYP' ).
      lv_index = lv_index + 1.
    ENDWHILE.

  ENDMETHOD.
ENDCLASS.
