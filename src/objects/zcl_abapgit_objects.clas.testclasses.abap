CLASS ltcl_object_types DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      type_supported FOR TESTING RAISING zcx_abapgit_exception,
      not_exist FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_object_types IMPLEMENTATION.

  METHOD type_supported.

    cl_abap_unit_assert=>assert_equals(
      act  = zcl_abapgit_objects=>is_type_supported( 'PROG' )
      exp  = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act  = zcl_abapgit_objects=>is_type_supported( 'ZXYZ' )
      exp  = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act  = zcl_abapgit_objects=>is_type_supported( '' )
      exp  = abap_false ).

  ENDMETHOD.

  METHOD not_exist.

    DATA: ls_item   TYPE zif_abapgit_definitions=>ty_item,
          lv_exists TYPE abap_bool,
          lt_types  TYPE zcl_abapgit_objects=>ty_types_tt.

    FIELD-SYMBOLS: <lv_type> LIKE LINE OF lt_types.


    lt_types = zcl_abapgit_objects=>supported_list( ).

    cl_abap_unit_assert=>assert_not_initial( lt_types ).

    LOOP AT lt_types ASSIGNING <lv_type>.
      CLEAR ls_item.
      ls_item-obj_name = 'ZABAPGIT_FOOBAR'.
      ls_item-obj_type = <lv_type>.
      lv_exists = zcl_abapgit_objects=>exists( ls_item ).

      cl_abap_unit_assert=>assert_equals(
        act  = lv_exists
        exp  = abap_false
        msg  = ls_item-obj_type
        quit = if_aunit_constants=>no ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS ltcl_serialize DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_serialize DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      check
        IMPORTING VALUE(is_item) TYPE zif_abapgit_definitions=>ty_item
        RAISING   zcx_abapgit_exception,
      serialize_tabl FOR TESTING RAISING zcx_abapgit_exception,
      serialize_shlp FOR TESTING RAISING zcx_abapgit_exception,
      serialize_view FOR TESTING RAISING zcx_abapgit_exception,
      serialize_auth FOR TESTING RAISING zcx_abapgit_exception,
      serialize_clas FOR TESTING RAISING zcx_abapgit_exception,
      serialize_intf FOR TESTING RAISING zcx_abapgit_exception,
      serialize_doma FOR TESTING RAISING zcx_abapgit_exception,
      serialize_dtel FOR TESTING RAISING zcx_abapgit_exception,
      serialize_fugr FOR TESTING RAISING zcx_abapgit_exception,
      serialize_msag FOR TESTING RAISING zcx_abapgit_exception,
      serialize_prog FOR TESTING RAISING zcx_abapgit_exception,
      serialize_tran FOR TESTING RAISING zcx_abapgit_exception,
      serialize_ttyp FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS ltcl_serialize IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_serialize IMPLEMENTATION.

  METHOD serialize_shlp.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'SHLP'.
    ls_item-obj_name = 'USER_LOGON'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_view.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'VIEW'.
    ls_item-obj_name = 'VUSR02_HEADER'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_tabl.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'TABL'.
    ls_item-obj_name = 'USR02'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_auth.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'AUTH'.
    ls_item-obj_name = 'AREA'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_clas.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'CLAS'.
    ls_item-obj_name = 'CL_GUI_FRONTEND_SERVICES'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_intf.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.

    ls_item-obj_type = 'INTF'.
    ls_item-obj_name = 'IF_BADI_TADIR_CHANGED'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_doma.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'DOMA'.
    ls_item-obj_name = 'PGMID'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_dtel.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'DTEL'.
    ls_item-obj_name = 'PGMID'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_fugr.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'FUGR'.
    ls_item-obj_name = 'SRFC'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_msag.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'MSAG'.
    ls_item-obj_name = '00'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_prog.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'PROG'.
    ls_item-obj_name = 'SAPLWBABAP'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_tran.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'TRAN'.
    ls_item-obj_name = 'SE38'.

    check( ls_item ).

  ENDMETHOD.

  METHOD serialize_ttyp.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'TTYP'.
    ls_item-obj_name = 'ABAPPROG'.

    check( ls_item ).

  ENDMETHOD.

  METHOD check.

    DATA: ls_files_item TYPE zcl_abapgit_objects=>ty_serialization.

    ls_files_item = zcl_abapgit_objects=>serialize( is_item     = is_item
                                                    iv_language = zif_abapgit_definitions=>c_english ).

    cl_abap_unit_assert=>assert_not_initial( ls_files_item-files ).
    cl_abap_unit_assert=>assert_equals( act = ls_files_item-item
                                        exp = is_item ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_object_ddls_mock DEFINITION FOR TESTING.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras.

  PRIVATE SECTION.
    DATA ms_item TYPE zif_abapgit_definitions=>ty_item.

ENDCLASS.

CLASS ltcl_object_ddls_mock IMPLEMENTATION.

  METHOD constructor.

    ms_item = is_item.

* dummy use of variable
    IF iv_language = 'E'.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_object~is_locked.

    CASE ms_item-obj_name.
      WHEN 'Z_TEST_DDLS'.

        rv_is_locked = abap_true.

      WHEN 'Z_TEST_DDLS2'.

        rv_is_locked = abap_false.

    ENDCASE.

  ENDMETHOD.

  METHOD zif_abapgit_object~get_deserialize_steps. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~get_deserialize_order. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~changed_by. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~get_comparator. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~delete. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~deserialize. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~exists. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~get_metadata. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~jump. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~serialize. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~is_active. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~map_filename_to_object. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_object~map_object_to_filename. "##needed

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_check_objects_locked DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mt_given_items    TYPE zif_abapgit_definitions=>ty_items_tt,
      mv_exception_text TYPE string.

    METHODS:
      throw_excp_if_object_is_locked FOR TESTING RAISING cx_static_check,
      no_excp_if_obj_is_not_locked FOR TESTING RAISING cx_static_check,
      given_locked_object,
      when_check_objects_locked,
      then_exception_shd_be_raised,
      given_not_locked_object,
      then_no_exception_shd_occur,
      given_object
        IMPORTING
          iv_object_name TYPE string.

ENDCLASS.

CLASS zcl_abapgit_objects DEFINITION LOCAL FRIENDS ltcl_check_objects_locked.

CLASS ltcl_check_objects_locked IMPLEMENTATION.

  METHOD throw_excp_if_object_is_locked.

    given_locked_object( ).
    when_check_objects_locked( ).
    then_exception_shd_be_raised( ).

  ENDMETHOD.

  METHOD no_excp_if_obj_is_not_locked.

    given_not_locked_object( ).
    when_check_objects_locked( ).
    then_no_exception_shd_occur( ).

  ENDMETHOD.

  METHOD given_locked_object.

    given_object( 'Z_TEST_DDLS' ).

  ENDMETHOD.


  METHOD when_check_objects_locked.

    DATA: lx_error TYPE REF TO zcx_abapgit_exception.

    TRY.
        zcl_abapgit_objects=>check_objects_locked( iv_language = 'E'
                                                   it_items    = mt_given_items ).

      CATCH zcx_abapgit_exception INTO lx_error.
        mv_exception_text = lx_error->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD then_exception_shd_be_raised.

    cl_abap_unit_assert=>assert_equals(
      exp = |Object DDLS Z_TEST_DDLS is locked. Action not possible.|
      act = mv_exception_text ).

  ENDMETHOD.


  METHOD given_not_locked_object.

    given_object( 'Z_TEST_DDLS2' ).

  ENDMETHOD.


  METHOD then_no_exception_shd_occur.

    cl_abap_unit_assert=>assert_initial( mv_exception_text ).

  ENDMETHOD.


  METHOD given_object.

    CONSTANTS:
      lc_obj_type TYPE string VALUE 'DDLS'.

    DATA:
      ls_item               LIKE LINE OF mt_given_items,
      ls_obj_serializer_map LIKE LINE OF zcl_abapgit_objects=>gt_obj_serializer_map.

    ls_item-obj_type = lc_obj_type.
    ls_item-obj_name = iv_object_name.
    INSERT ls_item INTO TABLE mt_given_items.

    ls_obj_serializer_map-item-obj_type = lc_obj_type.
    ls_obj_serializer_map-item-obj_name = iv_object_name.
    ls_obj_serializer_map-metadata-class = '\CLASS-POOL=ZCL_ABAPGIT_OBJECTS\CLASS=LTCL_OBJECT_DDLS_MOCK'.
    INSERT ls_obj_serializer_map INTO TABLE zcl_abapgit_objects=>gt_obj_serializer_map.

  ENDMETHOD.

ENDCLASS.
