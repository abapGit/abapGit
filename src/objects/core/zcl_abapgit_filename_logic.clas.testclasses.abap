CLASS ltcl_run_checks DEFINITION DEFERRED.
CLASS zcl_abapgit_filename_logic DEFINITION LOCAL FRIENDS ltcl_run_checks.

CLASS ltcl_run_checks DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA mo_dot TYPE REF TO zcl_abapgit_dot_abapgit.

    METHODS:
      setup,
      file_to_object FOR TESTING RAISING zcx_abapgit_exception,
      object_to_file FOR TESTING RAISING zcx_abapgit_exception,
      file_to_object_pack FOR TESTING RAISING zcx_abapgit_exception,
      object_to_file_pack FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_run_checks IMPLEMENTATION.

  METHOD setup.

    " Assume for unit tests that starting folder is /src/ with prefix logic
    mo_dot = zcl_abapgit_dot_abapgit=>build_default( ).

  ENDMETHOD.

  METHOD file_to_object.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_is_xml TYPE abap_bool.

    zcl_abapgit_filename_logic=>file_to_object(
      EXPORTING
        iv_filename = 'zprogram.prog.abap'
        iv_path     = '/src/'
        iv_devclass = '$PACK'
        io_dot      = mo_dot
      IMPORTING
        es_item     = ls_item
        ev_is_xml   = lv_is_xml ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'PROG'
      act = ls_item-obj_type ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'ZPROGRAM'
      act = ls_item-obj_name ).
    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = lv_is_xml ).

    " Subpackage
    zcl_abapgit_filename_logic=>file_to_object(
      EXPORTING
        iv_filename = 'zprogram.prog.abap'
        iv_path     = '/src/subpack/'
        iv_devclass = '$PACK'
        io_dot      = mo_dot
      IMPORTING
        es_item     = ls_item
        ev_is_xml   = lv_is_xml ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'PROG'
      act = ls_item-obj_type ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'ZPROGRAM'
      act = ls_item-obj_name ).
    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = lv_is_xml ).

    " XML
    zcl_abapgit_filename_logic=>file_to_object(
      EXPORTING
        iv_filename = 'zprogram.prog.xml'
        iv_path     = '/src/'
        iv_devclass = '$PACK'
        io_dot      = mo_dot
      IMPORTING
        es_item     = ls_item
        ev_is_xml   = lv_is_xml ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'PROG'
      act = ls_item-obj_type ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'ZPROGRAM'
      act = ls_item-obj_name ).
    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = lv_is_xml ).

    " With special characters
    zcl_abapgit_filename_logic=>file_to_object(
     EXPORTING
       iv_filename = 'ztest%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3dvc.prog.abap'
       iv_path     = '/src/'
       iv_devclass = '$PACK'
       io_dot      = mo_dot
     IMPORTING
       es_item     = ls_item
       ev_is_xml   = lv_is_xml ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'PROG'
      act = ls_item-obj_type ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'ZTEST=========================VC'
      act = ls_item-obj_name ).

    zcl_abapgit_filename_logic=>file_to_object(
     EXPORTING
       iv_filename = 'zmime_%3c%3e_%3f.w3mi.jpg'
       iv_path     = '/src/'
       iv_devclass = '$PACK'
       io_dot      = mo_dot
     IMPORTING
       es_item     = ls_item
       ev_is_xml   = lv_is_xml ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'W3MI'
      act = ls_item-obj_type ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'ZMIME_<>_?'
      act = ls_item-obj_name ).

  ENDMETHOD.

  METHOD object_to_file.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_filename TYPE string.

    ls_item-obj_type = 'PROG'.
    ls_item-obj_name = 'ZPROGRAM'.

    lv_filename = zcl_abapgit_filename_logic=>object_to_file(
      is_item = ls_item
      iv_ext  = 'abap' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'zprogram.prog.abap'
      act = lv_filename ).

    " With namespace
    ls_item-obj_type = 'PROG'.
    ls_item-obj_name = '/TEST/ZPROGRAM'.

    lv_filename = zcl_abapgit_filename_logic=>object_to_file(
      is_item = ls_item
      iv_ext  = 'abap' ).

    cl_abap_unit_assert=>assert_equals(
      exp = '#test#zprogram.prog.abap'
      act = lv_filename ).

    " With extra extension
    ls_item-obj_type = 'CLAS'.
    ls_item-obj_name = 'ZCLASS'.

    lv_filename = zcl_abapgit_filename_logic=>object_to_file(
      is_item  = ls_item
      iv_ext   = 'abap'
      iv_extra = 'testclasses' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'zclass.clas.testclasses.abap'
      act = lv_filename ).

    " With special characters
    ls_item-obj_type = 'PROG'.
    ls_item-obj_name = 'ZTEST=========================VC'.

    lv_filename = zcl_abapgit_filename_logic=>object_to_file(
      is_item  = ls_item
      iv_ext   = 'abap' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'ztest%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3dvc.prog.abap'
      act = lv_filename ).

    ls_item-obj_type = 'W3MI'.
    ls_item-obj_name = 'ZMIME_<>_?'.

    lv_filename = zcl_abapgit_filename_logic=>object_to_file(
      is_item  = ls_item
      iv_ext   = 'jpg' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'zmime_%3c%3e_%3f.w3mi.jpg'
      act = lv_filename ).

  ENDMETHOD.

  METHOD file_to_object_pack.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.

    zcl_abapgit_filename_logic=>file_to_object(
      EXPORTING
        iv_filename = 'package.devc.xml'
        iv_path     = '/src/'
        iv_devclass = '$PACK'
        io_dot      = mo_dot
      IMPORTING
        es_item     = ls_item ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'DEVC'
      act = ls_item-obj_type ).
    cl_abap_unit_assert=>assert_equals(
      exp = '$PACK'
      act = ls_item-obj_name ).

    " Subpackage
    zcl_abapgit_filename_logic=>file_to_object(
      EXPORTING
        iv_filename = 'package.devc.xml'
        iv_path     = '/src/subpack/'
        iv_devclass = '$PACK'
        io_dot      = mo_dot
      IMPORTING
        es_item     = ls_item ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'DEVC'
      act = ls_item-obj_type ).
    cl_abap_unit_assert=>assert_equals(
      exp = '$PACK_SUBPACK'
      act = ls_item-obj_name ).

  ENDMETHOD.

  METHOD object_to_file_pack.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_filename TYPE string.

    ls_item-obj_type = 'DEVC'.
    ls_item-obj_name = 'ZPACKAGE'.

    lv_filename = zcl_abapgit_filename_logic=>object_to_file(
      is_item = ls_item
      iv_ext  = 'xml' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'package.devc.xml'
      act = lv_filename ).

  ENDMETHOD.

ENDCLASS.
