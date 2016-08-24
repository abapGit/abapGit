*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_SAP_PACKAGE
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_package DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sap_package DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: ty_devclass_tt TYPE STANDARD TABLE OF devclass WITH DEFAULT KEY.

    CLASS-METHODS:
      check
        IMPORTING io_log     TYPE REF TO lcl_log
                  it_results TYPE ty_results_tt
                  iv_start   TYPE string
                  iv_top     TYPE devclass,
      list_subpackages IMPORTING iv_package     TYPE devclass
                       RETURNING VALUE(rt_list) TYPE ty_devclass_tt,
      list_superpackages IMPORTING iv_package     TYPE devclass
                         RETURNING VALUE(rt_list) TYPE ty_devclass_tt,
      create_local
        IMPORTING iv_package TYPE devclass
        RAISING   lcx_exception,
      create
        IMPORTING is_package        TYPE scompkdtln
        RETURNING VALUE(ri_package) TYPE REF TO if_package
        RAISING   lcx_exception,
      create_child
        IMPORTING iv_parent TYPE devclass
                  iv_child  TYPE devclass
        RAISING   lcx_exception,
      exists
        IMPORTING iv_package     TYPE devclass
        RETURNING VALUE(rv_bool) TYPE abap_bool.

  PRIVATE SECTION.
    CLASS-METHODS:
      class_to_path
        IMPORTING
          iv_top         TYPE devclass
          iv_start       TYPE string
          iv_package     TYPE devclass
        RETURNING
          VALUE(rv_path) TYPE string.

ENDCLASS.                    "lcl_package DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_package IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sap_package IMPLEMENTATION.

  METHOD class_to_path.

    DATA: lv_len      TYPE i,
          lv_path     TYPE string,
          lv_parentcl TYPE tdevc-parentcl.


    IF iv_top = iv_package.
      rv_path = iv_start.
    ELSE.
      SELECT SINGLE parentcl FROM tdevc INTO lv_parentcl
        WHERE devclass = iv_package.      "#EC CI_SUBRC "#EC CI_GENBUFF
      ASSERT sy-subrc = 0.

      IF lv_parentcl IS INITIAL.
        rv_path = 'error' ##no_text.
      ELSE.
        lv_len = strlen( lv_parentcl ).
        lv_path = iv_package+lv_len.
        IF lv_path(1) = '_'.
          lv_path = lv_path+1.
        ENDIF.
        TRANSLATE lv_path TO LOWER CASE.
        CONCATENATE lv_path '/' INTO lv_path.

        rv_path = class_to_path( iv_top     = iv_top
                                 iv_start   = iv_start
                                 iv_package = lv_parentcl ).

        CONCATENATE rv_path lv_path INTO rv_path.

      ENDIF.

    ENDIF.

  ENDMETHOD.                    "class_to_path

  METHOD check.

    DATA: lv_path TYPE string.

    FIELD-SYMBOLS: <ls_res1> LIKE LINE OF it_results,
                   <ls_res2> LIKE LINE OF it_results.


    IF io_log IS INITIAL.
      RETURN.
    ENDIF.

* check files for one object is in the same folder
    LOOP AT it_results ASSIGNING <ls_res1>
        WHERE NOT obj_type IS INITIAL.
      LOOP AT it_results ASSIGNING <ls_res2>
          WHERE obj_type = <ls_res1>-obj_type
          AND obj_name = <ls_res1>-obj_name
          AND path <> <ls_res1>-path.
        io_log->add( iv_msgv1 = 'Files for object'
                     iv_msgv2 = <ls_res1>-obj_type
                     iv_msgv3 = <ls_res1>-obj_name
                     iv_msgv4 = 'are not placed in the same folder' ) ##no_text.
        EXIT.
      ENDLOOP.
    ENDLOOP.

* check that objects are created in package corresponding to folder
    LOOP AT it_results ASSIGNING <ls_res1>
        WHERE NOT package IS INITIAL AND NOT path IS INITIAL.
      lv_path = class_to_path( iv_top     = iv_top
                               iv_start   = iv_start
                               iv_package = <ls_res1>-package ).
      IF lv_path <> <ls_res1>-path.
        io_log->add( iv_msgv1 = 'Package and path does not match for object,'
                     iv_msgv2 = <ls_res1>-obj_type
                     iv_msgv3 = <ls_res1>-obj_name ) ##no_text.
      ENDIF.
    ENDLOOP.

* check for multiple files with same filename
    LOOP AT it_results ASSIGNING <ls_res1>
        WHERE NOT filename IS INITIAL.
      LOOP AT it_results ASSIGNING <ls_res2>
          WHERE filename = <ls_res1>-filename
          AND path <> <ls_res1>-path.
        io_log->add( iv_msgv1 = 'Multiple files with same filename,'
                     iv_msgv2 = <ls_res1>-filename ) ##no_text.
        EXIT.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.                    "check

  METHOD exists.

    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = iv_package
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5 ).
    rv_bool = boolc( sy-subrc <> 1 ).

  ENDMETHOD.

  METHOD create_child.

    DATA: li_parent TYPE REF TO if_package,
          ls_child  TYPE scompkdtln.


    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = iv_parent
      IMPORTING
        e_package                  = li_parent
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5 ).
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error reading parent package' ).
    ENDIF.

    ls_child-devclass  = iv_child.
    ls_child-ctext     = iv_child.
    ls_child-parentcl  = iv_parent.
    ls_child-component = li_parent->transport_layer.
    ls_child-as4user   = sy-uname.

    create( ls_child ).

  ENDMETHOD.

  METHOD create.

    DATA: lv_err     TYPE string,
          ls_package LIKE is_package.


    ASSERT NOT is_package-devclass IS INITIAL.

    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = is_package-devclass
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5 ).
    IF sy-subrc = 0.
      RETURN. "Package already exists. We assume this is fine
    ENDIF.

    ls_package = is_package.

    cl_package_factory=>create_new_package(
      EXPORTING
        i_reuse_deleted_object     = abap_true
*        i_suppress_dialog          = abap_true " does not exist in 730
      IMPORTING
        e_package                  = ri_package
      CHANGING
        c_package_data             = ls_package
      EXCEPTIONS
        object_already_existing    = 1
        object_just_created        = 2
        not_authorized             = 3
        wrong_name_prefix          = 4
        undefined_name             = 5
        reserved_local_name        = 6
        invalid_package_name       = 7
        short_text_missing         = 8
        software_component_invalid = 9
        layer_invalid              = 10
        author_not_existing        = 11
        component_not_existing     = 12
        component_missing          = 13
        prefix_in_use              = 14
        unexpected_error           = 15
        intern_err                 = 16
        no_access                  = 17
*        invalid_translation_depth  = 18
*        wrong_mainpack_value       = 19
*        superpackage_invalid       = 20
*        error_in_cts_checks        = 21
        OTHERS                     = 18 ).
    IF sy-subrc <> 0.
      lcx_exception=>raise( |Package { is_package-devclass } could not be created| ).
    ENDIF.

    ri_package->save(
*      EXPORTING
*        i_suppress_dialog     = abap_true    " Controls whether popups can be transmitted
      EXCEPTIONS
        object_invalid        = 1
        object_not_changeable = 2
        cancelled_in_corr     = 3
        permission_failure    = 4
        unexpected_error      = 5
        intern_err            = 6
        OTHERS                = 7 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_err.
      lcx_exception=>raise( lv_err ).
    ENDIF.

    ri_package->set_changeable( abap_false ).

  ENDMETHOD.

  METHOD list_superpackages.

    DATA: lt_list     LIKE rt_list,
          lv_parent   TYPE tdevc-parentcl,
          lv_devclass LIKE LINE OF rt_list.


    APPEND iv_package TO rt_list.

    SELECT SINGLE parentcl INTO lv_parent
      FROM tdevc WHERE devclass = iv_package.           "#EC CI_GENBUFF

    IF NOT lv_parent IS INITIAL.
      APPEND lv_parent TO rt_list.
      lt_list = list_superpackages( lv_devclass ).
      APPEND LINES OF lt_list TO rt_list.
    ENDIF.

  ENDMETHOD.

  METHOD list_subpackages.

    DATA: lt_list     LIKE rt_list,
          lv_devclass LIKE LINE OF rt_list.


    SELECT devclass INTO TABLE rt_list
      FROM tdevc WHERE parentcl = iv_package.           "#EC CI_GENBUFF

* note the recursion, since packages are added to the list
    LOOP AT rt_list INTO lv_devclass.
      lt_list = list_subpackages( lv_devclass ).
      APPEND LINES OF lt_list TO rt_list.
    ENDLOOP.

  ENDMETHOD.

  METHOD create_local.

    DATA: ls_package TYPE scompkdtln.


    ls_package-devclass  = iv_package.
    ls_package-ctext     = iv_package.
    ls_package-parentcl  = '$TMP'.
    ls_package-component = 'LOCAL'.
    ls_package-as4user   = sy-uname.

    create( ls_package ).

  ENDMETHOD.                    "create

ENDCLASS.                    "lcl_package IMPLEMENTATION