CLASS zcl_abapgit_sap_package DEFINITION
    PUBLIC CREATE PRIVATE
    GLOBAL FRIENDS zcl_abapgit_factory.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_package TYPE devclass.

    INTERFACES: zif_abapgit_sap_package.

  PRIVATE SECTION.
    DATA: mv_package TYPE devclass.

    ALIASES:
      create FOR zif_abapgit_sap_package~create,
      create_local FOR zif_abapgit_sap_package~create_local.

ENDCLASS.



CLASS ZCL_ABAPGIT_SAP_PACKAGE IMPLEMENTATION.


  METHOD constructor.
    mv_package = iv_package.
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


  METHOD zif_abapgit_sap_package~create.

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


  METHOD zif_abapgit_sap_package~create_local.

    DATA: ls_package TYPE scompkdtln.


    ls_package-devclass  = mv_package.
    ls_package-ctext     = mv_package.
    ls_package-parentcl  = '$TMP'.
    ls_package-dlvunit   = 'LOCAL'.
    ls_package-as4user   = sy-uname.

    create( ls_package ).

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


  METHOD zif_abapgit_sap_package~get_transport_type.
    DATA: lv_err_prefix TYPE string,
          lv_pkg_name   TYPE e071-obj_name.

    lv_err_prefix = |TRINT_GET_REQUEST_TYPE(R3TR, DEVC, { mv_package })|.
    lv_pkg_name = mv_package.

    CALL FUNCTION 'TRINT_GET_REQUEST_TYPE'
      EXPORTING
        iv_pgmid                   = 'R3TR'
        iv_object                  = 'DEVC'
        iv_obj_name                = lv_pkg_name
      IMPORTING
        ev_request_type            = rs_transport_type-request
        ev_task_type               = rs_transport_type-task
      EXCEPTIONS
        no_request_needed          = 1
        internal_error             = 2
        cts_initialization_failure = 3.

    CASE sy-subrc.
      WHEN 0.
        " OK!

      WHEN 1.
        zcx_abapgit_exception=>raise( |{ lv_err_prefix }: transport is not needed| ).

      WHEN 2.
        zcx_abapgit_exception=>raise( |{ lv_err_prefix }: internal error| ).

      WHEN 3.
        zcx_abapgit_exception=>raise( |{ lv_err_prefix }: failed to initialized CTS| ).

      WHEN OTHERS.
        zcx_abapgit_exception=>raise( |{ lv_err_prefix }: unrecognized return code| ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_sap_package~list_subpackages.

    DATA: lt_list     LIKE rt_list.

    SELECT devclass FROM tdevc
      INTO TABLE lt_list
      WHERE parentcl = mv_package
      ORDER BY PRIMARY KEY.               "#EC CI_SUBRC "#EC CI_GENBUFF

    rt_list = lt_list.
    WHILE lines( lt_list ) > 0.

      SELECT devclass FROM tdevc
        INTO TABLE lt_list
        FOR ALL ENTRIES IN lt_list
        WHERE parentcl = lt_list-table_line
        ORDER BY PRIMARY KEY.             "#EC CI_SUBRC "#EC CI_GENBUFF
      APPEND LINES OF lt_list TO rt_list.

    ENDWHILE.

  ENDMETHOD.


  METHOD zif_abapgit_sap_package~list_superpackages.

    DATA: lt_list   LIKE rt_list,
          lv_parent TYPE tdevc-parentcl.


    APPEND mv_package TO rt_list.

    lv_parent = zif_abapgit_sap_package~read_parent( ).

    IF sy-subrc = 0 AND NOT lv_parent IS INITIAL.
      lt_list = zcl_abapgit_factory=>get_sap_package( lv_parent )->list_superpackages( ).
      APPEND LINES OF lt_list TO rt_list.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_sap_package~read_parent.

    SELECT SINGLE parentcl FROM tdevc INTO rv_parentcl
      WHERE devclass = mv_package.        "#EC CI_SUBRC "#EC CI_GENBUFF
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
