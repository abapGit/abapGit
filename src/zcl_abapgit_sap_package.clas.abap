CLASS zcl_abapgit_sap_package DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get
        IMPORTING iv_package        TYPE devclass
        RETURNING VALUE(ri_package) TYPE REF TO zif_abapgit_sap_package,
      create
        IMPORTING is_package TYPE scompkdtln
        RAISING   zcx_abapgit_exception,
      create_local
        IMPORTING iv_package TYPE devclass
        RAISING   zcx_abapgit_exception.

    METHODS:
      constructor
        IMPORTING iv_package TYPE devclass.

    INTERFACES: zif_abapgit_sap_package.

    TYPES: BEGIN OF ty_injected,
             package TYPE devclass,
             object  TYPE REF TO zif_abapgit_sap_package,
           END OF ty_injected.

* TODO, isolate this variable?
    CLASS-DATA: gt_injected TYPE STANDARD TABLE OF ty_injected.

  PRIVATE SECTION.
    DATA: mv_package TYPE devclass.

ENDCLASS.



CLASS zcl_abapgit_sap_package IMPLEMENTATION.


  METHOD constructor.
    mv_package = iv_package.
  ENDMETHOD.


  METHOD create.

    DATA: lv_err     TYPE string,
          li_package TYPE REF TO if_package,
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
      " Package already exists. We assume this is fine. Its properties might be changed later at
      " DEVC deserialization.
      RETURN.
    ENDIF.

    ls_package = is_package.

    " Set software component to 'HOME' if none is set at this point.
    " Otherwise SOFTWARE_COMPONENT_INVALID will be raised.
    IF ls_package-dlvunit IS INITIAL.
      ls_package-dlvunit = 'HOME'.
    ENDIF.

    cl_package_factory=>create_new_package(
      EXPORTING
        i_reuse_deleted_object     = abap_true
*        i_suppress_dialog          = abap_true " does not exist in 730
      IMPORTING
        e_package                  = li_package
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
      zcx_abapgit_exception=>raise( |Package { is_package-devclass } could not be created| ).
    ENDIF.

    li_package->save(
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

      " Here we have to delete the package,
      " otherwise it would remain in the memory
      " and cannot created again in this session.
      li_package->delete(
        EXCEPTIONS
          object_not_empty      = 1
          object_not_changeable = 2
          object_invalid        = 3
          intern_err            = 4
          OTHERS                = 5 ).

      zcx_abapgit_exception=>raise( lv_err ).

    ENDIF.

    li_package->set_changeable( abap_false ).

  ENDMETHOD.


  METHOD create_local.

    DATA: ls_package TYPE scompkdtln.


    ls_package-devclass  = iv_package.
    ls_package-ctext     = iv_package.
    ls_package-parentcl  = '$TMP'.
    ls_package-dlvunit   = 'LOCAL'.
    ls_package-as4user   = sy-uname.

    create( ls_package ).

  ENDMETHOD.                    "create


  METHOD get.

    FIELD-SYMBOLS: <ls_injected> LIKE LINE OF gt_injected.

    IF lines( gt_injected ) > 0.
      READ TABLE gt_injected ASSIGNING <ls_injected> WITH KEY package = iv_package.
      ASSERT sy-subrc = 0. " unit test should be in control
      ri_package = <ls_injected>-object.
    ELSE.
      CREATE OBJECT ri_package TYPE zcl_abapgit_sap_package
        EXPORTING
          iv_package = iv_package.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_sap_package~create_child.

    DATA: li_parent TYPE REF TO if_package,
          ls_child  TYPE scompkdtln.


    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = mv_package
      IMPORTING
        e_package                  = li_parent
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error reading parent package' ).
    ENDIF.

    ls_child-devclass  = iv_child.
    ls_child-dlvunit   = li_parent->software_component.
    ls_child-component = li_parent->application_component.
    ls_child-ctext     = iv_child.
    ls_child-parentcl  = mv_package.
    ls_child-pdevclass = li_parent->transport_layer.
    ls_child-as4user   = sy-uname.

    create( ls_child ).

  ENDMETHOD.


  METHOD zif_abapgit_sap_package~exists.

    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = mv_package
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5 ).
    rv_bool = boolc( sy-subrc <> 1 ).

  ENDMETHOD.


  METHOD zif_abapgit_sap_package~list_subpackages.

    DATA: lt_list     LIKE rt_list,
          lv_devclass LIKE LINE OF rt_list.


    SELECT devclass INTO TABLE rt_list
      FROM tdevc WHERE parentcl = mv_package. "#EC CI_GENBUFF "#EC CI_SUBRC

* note the recursion, since packages are added to the list
    LOOP AT rt_list INTO lv_devclass.
      lt_list = get( lv_devclass )->list_subpackages( ).
      APPEND LINES OF lt_list TO rt_list.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_sap_package~list_superpackages.

    DATA: lt_list   LIKE rt_list,
          lv_parent TYPE tdevc-parentcl.


    APPEND mv_package TO rt_list.

    SELECT SINGLE parentcl INTO lv_parent
      FROM tdevc WHERE devclass = mv_package.           "#EC CI_GENBUFF

    IF sy-subrc = 0 AND NOT lv_parent IS INITIAL.
      APPEND lv_parent TO rt_list.
      lt_list = get( lv_parent )->list_superpackages( ).
      APPEND LINES OF lt_list TO rt_list.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_sap_package~read_parent.

    SELECT SINGLE parentcl FROM tdevc INTO rv_parentcl
      WHERE devclass = mv_package.        "#EC CI_SUBRC "#EC CI_GENBUFF
    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~are_changes_recorded_in_tr_req.

    DATA: li_package TYPE REF TO if_package.

    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = mv_package
      IMPORTING
        e_package                  = li_package
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5
        OTHERS                     = 6 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from CL_PACKAGE_FACTORY=>LOAD_PACKAGE { sy-subrc }| ).
    ENDIF.

    rv_are_changes_rec_in_tr_req = li_package->wbo_korr_flag.

  ENDMETHOD.

ENDCLASS.
