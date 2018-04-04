CLASS zcl_abapgit_ecatt_download DEFINITION
  PUBLIC
  INHERITING FROM cl_apl_ecatt_download
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      z_build_xml_of_object
        IMPORTING
          im_object_name     TYPE  etobj_name
          im_object_version  TYPE  etobj_ver
          im_object_type     TYPE  etobj_type
        EXPORTING
          ex_xml_stream      TYPE  xstring
          ex_xml_stream_size TYPE  int4
        RAISING
          zcx_abapgit_exception,

      z_download_data
        IMPORTING
          ii_template_over_all TYPE REF TO if_ixml_document
        EXPORTING
          ev_xml_stream        TYPE xstring
          ev_xml_stream_size   TYPE i.

ENDCLASS.



CLASS zcl_abapgit_ecatt_download IMPLEMENTATION.


  METHOD z_build_xml_of_object.

    " Downport of CL_APL_ECATT_DOWNLOAD=>BUILD_XML_OF_OBJECT

    DATA: lo_download        TYPE REF TO cl_apl_ecatt_download,
          lo_load_help_dummy TYPE REF TO cl_apl_ecatt_load_help,
          lx_ecatt           TYPE REF TO cx_ecatt_apl,
          lv_text            TYPE string.

    CASE im_object_type.
*      WHEN cl_apl_ecatt_const=>obj_type_ecatt.
*        CREATE OBJECT lo_download TYPE cl_apl_ecatt_script_download.
      WHEN cl_apl_ecatt_const=>obj_type_test_data.
        CREATE OBJECT lo_download TYPE zcl_abapgit_ecatt_data_downl.
*      WHEN cl_apl_ecatt_const=>obj_type_system_data.
*        CREATE OBJECT lo_download TYPE cl_apl_ecatt_systems_download.
*      WHEN cl_apl_ecatt_const=>obj_type_test_config.
*        CREATE OBJECT lo_download TYPE cl_apl_ecatt_config_download.
*      WHEN cl_apl_ecatt_const=>obj_type_ecatt_vo.
*        CREATE OBJECT lo_download TYPE cl_apl_ecatt_vo_download.
*      WHEN cl_apl_ecatt_const=>obj_type_start_profile.
*        CREATE OBJECT lo_download TYPE cl_apl_ecatt_sp_download.
    ENDCASE.

    IF lo_download IS NOT BOUND.
      zcx_abapgit_exception=>raise( |ECATT: Download not possible { im_object_type } { im_object_name }| ).
    ENDIF.

    CALL METHOD lo_download->('SET_GENERATE_XML_NO_DOWNLOAD')
      EXPORTING
        iv_generate_xml_no_download = abap_true.

    "download method will create the xml stream
    "note: it's the redefined download( ) of each object type specific download, which is called
    TRY.
        CREATE OBJECT lo_load_help_dummy
          EXPORTING
            im_maintain_function = ''.

        lo_download->download( im_object_name    = im_object_name
                               im_object_version = im_object_version
                               im_object_type    = im_object_type
                               im_load_help      = lo_load_help_dummy ).

      CATCH cx_ecatt_apl INTO lx_ecatt.
        lv_text = lx_ecatt->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
      CATCH cx_ecatt_ui_attachment.
        "will never be raised from download, when called with mv_generate_xml_no_download = 'X'.
    ENDTRY.

    CALL METHOD lo_download->('GET_XML_STREAM')
      RECEIVING
        rv_xml_stream = ex_xml_stream.

    CALL METHOD lo_download->('GET_XML_STREAM_SIZE')
      RECEIVING
        rv_xml_stream_size = ex_xml_stream_size.

  ENDMETHOD.


  METHOD z_download_data.

    DATA:
      lv_xtab  TYPE etxml_xline_tabtype,
      lo_xml   TYPE REF TO cl_apl_ecatt_xml,
      lv_size  TYPE int4,
      lx_ecatt TYPE REF TO cx_ecatt_apl_xml.

    CLEAR: ev_xml_stream,
           ev_xml_stream_size.

    TRY.
        lo_xml = cl_apl_ecatt_xml=>create( im_type = if_apl_ecatt_xml=>co_xml ).

        lo_xml->set_attributes( im_dom = ii_template_over_all ).

        lo_xml->get_attributes(
          IMPORTING
            ex_xtab         = lv_xtab
            ex_size_xstring = lv_size
            ex_xml          = ev_xml_stream ).

        ev_xml_stream_size = lv_size.

      CATCH cx_ecatt_apl_xml INTO lx_ecatt.
        RETURN.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
