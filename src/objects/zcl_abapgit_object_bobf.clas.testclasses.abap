CLASS lct_integration_sales_order DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS exists FOR TESTING
      RAISING
        zcx_abapgit_exception.
    METHODS serialize FOR TESTING
      RAISING
        zcx_abapgit_exception
        zcx_abapgitp_object.
    METHODS deserialize FOR TESTING
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
    CONSTANTS c_bo_name TYPE sobj_name VALUE '/BOBF/EPM_PRODUCT'.

    DATA f_cut TYPE REF TO zcl_abapgit_object_bobf.
    METHODS setup.
    METHODS delete_old_model
      RAISING
        zcx_abapgit_exception.

ENDCLASS.

CLASS lct_integration_sales_order IMPLEMENTATION.


  METHOD setup.
    f_cut = NEW zcl_abapgit_object_bobf(
        is_item     = VALUE #(  obj_name = c_bo_name
                                obj_type = 'BOBF'
                                inactive = abap_false
                                devclass = '$TMP' )
        iv_language = sy-langu
    ).
  ENDMETHOD.

  METHOD exists.
    cl_aunit_assert=>assert_equals(
      EXPORTING
        exp                  = abap_true
        act                  = f_cut->zif_abapgit_object~exists( )
    ).
  ENDMETHOD.

  METHOD serialize.
    DATA(lo_xml_output) = NEW zcl_abapgit_xml_output( ).
    f_cut->zif_abapgit_object~serialize( io_xml = lo_xml_output ).

    DATA(lv_xml) = lo_xml_output->render( is_metadata  = VALUE #(  ) ).

    cl_aunit_assert=>assert_char_cp(
      EXPORTING
        act              = lv_xml
        exp              = |*<MODEL>*{ c_bo_name }*|
    ).

  ENDMETHOD.

  METHOD deserialize.
    DATA(lo_xml_output) = NEW zcl_abapgit_xml_output( ).

*    Delete old object if not cleaned up properly
    delete_old_model( ).

    f_cut->zif_abapgit_object~serialize( io_xml = lo_xml_output ).

    DATA(lv_xml) = lo_xml_output->render( is_metadata  = VALUE #(  ) ).
    REPLACE ALL OCCURRENCES OF c_bo_name IN lv_xml WITH 'ZABAPGIT_TEST'.
    REPLACE ALL OCCURRENCES OF '/BOBF/IF_EPM_PRODUCT_C' IN lv_xml WITH 'ZIF_ABAPGIT_TEST'.

    f_cut->zif_abapgit_object~deserialize(
      EXPORTING
        iv_package            = '$TMP'
        io_xml                = NEW zcl_abapgit_xml_input(
        iv_xml                = lv_xml
    )
        iv_step               =  zif_abapgit_object=>gc_step_id-abap
        ii_log                = NEW zcl_abapgit_log( )
    ).
*      CATCH zcx_abapgit_exception.    "
  ENDMETHOD.


  METHOD delete_old_model.
    NEW /bobf/cl_conf_model_api_adt( )->delete_business_object(
      EXPORTING
        iv_business_object_name = CONV #( 'ZABAPGIT_TEST' )
      IMPORTING
        ev_success              = DATA(lv_deleted)
    ).
  ENDMETHOD.

ENDCLASS.
