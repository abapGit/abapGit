CLASS ltcl_run_checks DEFINITION DEFERRED.
CLASS zcl_abapgit_filename_logic DEFINITION LOCAL FRIENDS ltcl_run_checks.

CLASS lcl_memory_settings DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_persist_settings.
ENDCLASS.

CLASS lcl_memory_settings IMPLEMENTATION.
  METHOD zif_abapgit_persist_settings~modify.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_persist_settings~read.
    CREATE OBJECT ro_settings.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_run_checks DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA mo_dot TYPE REF TO zcl_abapgit_dot_abapgit.

    METHODS:
      setup,
      is_obj_def_file FOR TESTING,
      dot_abapgit FOR TESTING RAISING zcx_abapgit_exception,
      file_to_object FOR TESTING RAISING zcx_abapgit_exception,
      object_to_file FOR TESTING RAISING zcx_abapgit_exception,
      file_to_object_package FOR TESTING RAISING zcx_abapgit_exception,
      object_to_file_package FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_run_checks IMPLEMENTATION.

  METHOD setup.

    DATA li_memory TYPE REF TO lcl_memory_settings.

    " Assume for unit tests that starting folder is /src/ with prefix logic
    mo_dot = zcl_abapgit_dot_abapgit=>build_default( ).

    CREATE OBJECT li_memory.
    zcl_abapgit_persist_injector=>set_settings( li_memory ).

  ENDMETHOD.

  METHOD dot_abapgit.

    DATA lv_is_xml TYPE abap_bool.

    zcl_abapgit_filename_logic=>file_to_object(
      EXPORTING
        iv_filename = zif_abapgit_definitions=>c_dot_abapgit
        iv_path     = '/'
        io_dot      = mo_dot
      IMPORTING
        ev_is_xml   = lv_is_xml ).

    " .abapgit.xml is not considered an "XML file" since it does not represent an object (item)
    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = lv_is_xml ).

  ENDMETHOD.

  METHOD is_obj_def_file.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_filename_logic=>is_obj_definition_file( zif_abapgit_definitions=>c_dot_abapgit )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_filename_logic=>is_obj_definition_file( 'ztest.prog.xml' )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_filename_logic=>is_obj_definition_file( 'ztest.prog.json' )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_filename_logic=>is_obj_definition_file( 'ztest.prog.abap' )
      exp = abap_false ).

  ENDMETHOD.

  METHOD file_to_object.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_is_xml TYPE abap_bool.
    DATA lv_is_json TYPE abap_bool.

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

    zcl_abapgit_filename_logic=>file_to_object(
     EXPORTING
       iv_filename = 'ztest(name).w3mi.data,json'
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
      exp = 'ZTEST(NAME)'
      act = ls_item-obj_name ).

    " AFF file
    zcl_abapgit_filename_logic=>file_to_object(
      EXPORTING
        iv_filename = 'ztest.chko.json'
        iv_path     = '/src/'
        iv_devclass = '$PACK'
        io_dot      = mo_dot
      IMPORTING
        es_item     = ls_item
        ev_is_json  = lv_is_json ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'CHKO'
      act = ls_item-obj_type ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'ZTEST'
      act = ls_item-obj_name ).
    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = lv_is_json ).


    " AFF file with namespace
    zcl_abapgit_filename_logic=>file_to_object(
      EXPORTING
        iv_filename = '(abcd)ztest.chko.json'
        iv_path     = '/src/'
        iv_devclass = '$PACK'
        io_dot      = mo_dot
      IMPORTING
        es_item     = ls_item
        ev_is_json  = lv_is_json ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'CHKO'
      act = ls_item-obj_type ).
    cl_abap_unit_assert=>assert_equals(
      exp = '/ABCD/ZTEST'
      act = ls_item-obj_name ).
    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = lv_is_json ).

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

    ls_item-obj_type = 'W3MI'.
    ls_item-obj_name = 'ZTEST(NAME)'.

    lv_filename = zcl_abapgit_filename_logic=>object_to_file(
      is_item  = ls_item
      iv_ext   = 'json' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'ztest(name).w3mi.json'
      act = lv_filename ).

    " AFF object with namespace
    ls_item-obj_type = 'CHKO'.
    ls_item-obj_name = '/TEST/ZTEST'.

    lv_filename = zcl_abapgit_filename_logic=>object_to_file(
      is_item  = ls_item
      iv_ext   = 'json' ).

    cl_abap_unit_assert=>assert_equals(
      exp = '(test)ztest.chko.json'
      act = lv_filename ).

  ENDMETHOD.

  METHOD file_to_object_package.

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

  METHOD object_to_file_package.

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
