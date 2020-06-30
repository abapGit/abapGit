CLASS zcl_abapgit_ecatt_system_downl DEFINITION
  PUBLIC
  INHERITING FROM cl_apl_ecatt_systems_download
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
      set_systems_data_to_template.

ENDCLASS.



CLASS zcl_abapgit_ecatt_system_downl IMPLEMENTATION.


  METHOD download.

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
    set_systems_data_to_template( ).
    download_data( ).

  ENDMETHOD.


  METHOD download_data.

    " Downport

    mv_xml_stream = zcl_abapgit_ecatt_helper=>download_data( template_over_all ).

  ENDMETHOD.


  METHOD zif_abapgit_ecatt_download~get_xml_stream.

    rv_xml_stream = mv_xml_stream.

  ENDMETHOD.


  METHOD set_systems_data_to_template.

    DATA: lo_ecatt_systems TYPE REF TO cl_apl_ecatt_system_data,
          lt_sys_data      TYPE etsys_def_tabtype,
          ls_sys_data      TYPE etsys_def,
          li_item          TYPE REF TO if_ixml_element,
          li_sysdata_node  TYPE REF TO if_ixml_element.

    lo_ecatt_systems ?= ecatt_object.
    lt_sys_data = lo_ecatt_systems->get_system_data( ).

    li_sysdata_node = template_over_all->create_simple_element(
                        name = 'SYSTEMS_DATA'
                        parent = root_node ).

    etpar_node = template_over_all->create_simple_element(
                   name = 'ETSYS_DEF'
                   parent = li_sysdata_node ).

    LOOP AT lt_sys_data INTO ls_sys_data.

      CLEAR: ls_sys_data-sys_desc, ls_sys_data-instance.

      CALL FUNCTION 'SDIXML_DATA_TO_DOM'
        EXPORTING
          name         = 'item'
          dataobject   = ls_sys_data
        IMPORTING
          data_as_dom  = li_item
        CHANGING
          document     = template_over_all
        EXCEPTIONS
          illegal_name = 1
          OTHERS       = 2.
      ASSERT sy-subrc = 0.

      etpar_node->append_child( new_child = li_item ).

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
