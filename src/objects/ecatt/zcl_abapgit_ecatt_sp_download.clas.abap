CLASS zcl_abapgit_ecatt_sp_download DEFINITION
  PUBLIC
  INHERITING FROM cl_apl_ecatt_download
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_ecatt_download.

    METHODS:
      download REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      download_data REDEFINITION.

  PRIVATE SECTION.
    DATA:
      mv_xml_stream TYPE xstring.

    METHODS:
      set_sp_data_to_template.

ENDCLASS.



CLASS ZCL_ABAPGIT_ECATT_SP_DOWNLOAD IMPLEMENTATION.


  METHOD download.

    " We inherit from CL_APL_ECATT_DOWNLOAD because CL_APL_ECATT_SP_DOWNLOAD
    " doesn't exist in 702

    " Downport

    load_help = im_load_help.
    typ = im_object_type.

    TRY.
        cl_apl_ecatt_object=>show_object(
          EXPORTING
            im_obj_type = im_object_type
            im_name     = im_object_name
            im_version  = im_object_version
          IMPORTING
            re_object   = ecatt_object ).
      CATCH cx_ecatt INTO ex_ecatt.
        RETURN.
    ENDTRY.

    set_attributes_to_template( ).

    set_sp_data_to_template( ).

    download_data( ).

  ENDMETHOD.


  METHOD download_data.

    " Downport

    mv_xml_stream = zcl_abapgit_ecatt_helper=>download_data( template_over_all ).

  ENDMETHOD.


  METHOD set_sp_data_to_template.

    " downport

    DATA: li_dom                     TYPE REF TO if_ixml_document,
          li_start_profile_data_node TYPE REF TO if_ixml_element,
          li_element                 TYPE REF TO if_ixml_element,
          lv_sp_xml                  TYPE etxml_line_str,
          lo_ecatt_sp                TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_object> TYPE data.

    li_start_profile_data_node = template_over_all->create_simple_element(
                                   name = 'START_PROFILE'
                                   parent = root_node ).

    ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_object>.
    ASSERT sy-subrc = 0.

    lo_ecatt_sp = <lg_ecatt_object>.

    TRY.
        CALL METHOD lo_ecatt_sp->('GET_SP_ATTRIBUTES')
          IMPORTING
            e_sp_xml = lv_sp_xml.
      CATCH cx_ecatt_apl.
    ENDTRY.

    CALL FUNCTION 'SDIXML_XML_TO_DOM'
      EXPORTING
        xml      = lv_sp_xml
      IMPORTING
        document = li_dom.

    li_element = li_dom->get_root_element( ).
    li_start_profile_data_node->append_child( new_child = li_element ).

  ENDMETHOD.


  METHOD zif_abapgit_ecatt_download~get_xml_stream.

    rv_xml_stream = mv_xml_stream.

  ENDMETHOD.

ENDCLASS.
