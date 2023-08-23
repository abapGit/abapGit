
CLASS ltcl_serialize DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      check
        IMPORTING iv_top_package TYPE devclass
                  is_obj         TYPE zif_abapgit_definitions=>ty_obj
        RAISING   zcx_abapgit_exception,
      serialize_prog FOR TESTING RAISING zcx_abapgit_exception,
      serialize_doma FOR TESTING RAISING zcx_abapgit_exception,
      serialize_dtel FOR TESTING RAISING zcx_abapgit_exception.


ENDCLASS.


*----------------------------------------------------------------------*
*       CLASS ltcl_serialize IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_serialize IMPLEMENTATION.

  METHOD serialize_prog.

    DATA: ls_obj  TYPE zif_abapgit_definitions=>ty_obj.


    ls_obj-obj_type = 'PROG'.
    ls_obj-obj_name = 'SAPMSSYTIMER'.

    check(
        iv_top_package = 'SABAP_TIMER'
        is_obj       = ls_obj  ).

  ENDMETHOD.



  METHOD serialize_doma.

    DATA: ls_obj  TYPE zif_abapgit_definitions=>ty_obj.


    ls_obj-obj_type = 'DOMA'.
    ls_obj-obj_name = 'PGMID'.

    check(
        iv_top_package = 'SCTS_CAT'
        is_obj       = ls_obj  ).

  ENDMETHOD.

  METHOD serialize_dtel.

    DATA: ls_obj  TYPE zif_abapgit_definitions=>ty_obj.


    ls_obj-obj_type = 'DTEL'.
    ls_obj-obj_name = 'PGMID'.

    check(
        iv_top_package = 'SCTS_CAT'
        is_obj       = ls_obj  ).

  ENDMETHOD.

  METHOD check.

    DATA: ls_files_item TYPE zcl_abapgit_objects=>ty_serialization.

    DATA lt_objs  TYPE zif_abapgit_definitions=>ty_obj_tt.
    DATA lt_web_files_item  TYPE zcl_abapgit_objects_api=>ty_web_files_item_tt.
    DATA lr_web_file_item TYPE REF TO zcl_abapgit_objects_api=>ty_web_file_item.

    INSERT is_obj INTO TABLE lt_objs.

    zcl_abapgit_objects_api=>serialize_web_objects(
      EXPORTING
        iv_top_package    =  iv_top_package
*    is_dot_data       =
        it_objs           = lt_objs
      IMPORTING
        et_web_files_item = lt_web_files_item
    ).


    cl_abap_unit_assert=>assert_not_initial( lt_web_files_item ).

    LOOP AT lt_web_files_item REFERENCE INTO lr_web_file_item.

      cl_abap_unit_assert=>assert_equals( act = lr_web_file_item->item-obj_name
                                          exp = is_obj-obj_name ).

      cl_abap_unit_assert=>assert_equals( act = lr_web_file_item->item-obj_type
                                                exp = is_obj-obj_type ).

      cl_abap_unit_assert=>assert_not_initial( lr_web_file_item->web_file-base64_data ).
      cl_abap_unit_assert=>assert_not_initial( lr_web_file_item->web_file-filename ).
      cl_abap_unit_assert=>assert_not_initial( lr_web_file_item->web_file-path ).

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_file_conv DEFINITION DEFERRED.
CLASS zcl_abapgit_objects_api DEFINITION LOCAL FRIENDS ltcl_file_conv.

CLASS ltcl_file_conv DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:

      check_conv FOR TESTING RAISING zcx_abapgit_exception.



ENDCLASS.

CLASS ltcl_file_conv IMPLEMENTATION.

  METHOD check_conv.

    "Check that after the conversion of the files to web and back the sha1 is equal

    DATA: ls_obj  TYPE zif_abapgit_definitions=>ty_obj.
    DATA: ls_files_item TYPE zcl_abapgit_objects=>ty_serialization.
    DATA lt_objs  TYPE zif_abapgit_definitions=>ty_obj_tt.
    DATA lt_web_files_item  TYPE zcl_abapgit_objects_api=>ty_web_files_item_tt.
    DATA lr_web_file_item TYPE REF TO zcl_abapgit_objects_api=>ty_web_file_item.
    DATA lt_files  TYPE zif_abapgit_definitions=>ty_files_item_tt.
    DATA lt_files_conv  TYPE zif_abapgit_definitions=>ty_files_item_tt.
    DATA lt_remote  TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA lr_file TYPE REF TO  zif_abapgit_definitions=>ty_file_item.
    DATA lt_web_files  TYPE zcl_abapgit_objects_api=>ty_web_files_tt.
    DATA lr_remote TYPE REF TO zif_abapgit_git_definitions=>ty_file.

    ls_obj-obj_type = 'PROG'.
    ls_obj-obj_name = 'SAPMSSYTIMER'.

    INSERT ls_obj INTO TABLE lt_objs.

    zcl_abapgit_objects_api=>serialize_objects(
      EXPORTING
        iv_top_package    =  'SABAP_TIMER'
*    is_dot_data       =
        it_objs           = lt_objs
      IMPORTING
      et_files = lt_files ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

    READ TABLE lt_files REFERENCE INTO lr_file INDEX 1.
    cl_abap_unit_assert=>assert_subrc(
       exp = 0
       act = sy-subrc ).

    cl_abap_unit_assert=>assert_not_initial( lr_file->file-sha1 ).

    INSERT lr_file->* INTO TABLE lt_files_conv.

    lt_web_files_item = zcl_abapgit_objects_api=>conv_files_to_web_files_item( it_files = lt_files_conv ).
    cl_abap_unit_assert=>assert_not_initial( lt_web_files_item ).
    READ TABLE lt_web_files_item REFERENCE INTO lr_web_file_item INDEX 1.
    cl_abap_unit_assert=>assert_subrc(
      exp = 0
      act = sy-subrc ).

    INSERT lr_web_file_item->web_file INTO TABLE lt_web_files.

    lt_remote = zcl_abapgit_objects_api=>conv_web_files_to_remote_files( it_web_files = lt_web_files  ).
    cl_abap_unit_assert=>assert_not_initial( lt_remote ).
    READ TABLE lt_remote REFERENCE INTO lr_remote INDEX 1.
    cl_abap_unit_assert=>assert_subrc(
      exp = 0
      act = sy-subrc ).

    cl_abap_unit_assert=>assert_equals( act = lr_remote->sha1
                                        exp = lr_file->file-sha1 ).

  ENDMETHOD.

ENDCLASS.
