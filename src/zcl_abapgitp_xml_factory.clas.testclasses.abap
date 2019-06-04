
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS: create_xml_output FOR TESTING RAISING zcx_abapgitp_object.

ENDCLASS.       "ltcl_Test


CLASS ltcl_test IMPLEMENTATION.

  METHOD create_xml_output.

    DATA: li_output TYPE REF TO zif_abapgitp_xml_output,
          ls_meta   TYPE zif_abapgit_definitions=>ty_metadata,
          lv_output TYPE string,
          ls_data   TYPE usr02.

    li_output = zcl_abapgitp_xml_factory=>create_xml_output( ).

    li_output->add(
      iv_name = 'FOOBAR'
      ig_data = ls_data ).

    lv_output = li_output->render( ls_meta ).

    cl_abap_unit_assert=>assert_not_initial( lv_output ).

  ENDMETHOD.

ENDCLASS.
