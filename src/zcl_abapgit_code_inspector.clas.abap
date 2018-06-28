CLASS zcl_abapgit_code_inspector DEFINITION
  PUBLIC
  CREATE PROTECTED
  GLOBAL FRIENDS zcl_abapgit_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_code_inspector.

    METHODS:
      constructor
        IMPORTING
          iv_package            TYPE devclass
          iv_check_variant_name TYPE sci_chkv OPTIONAL
        RAISING
          zcx_abapgit_exception.

    CLASS-METHODS:
      validate_check_variant
        IMPORTING
          iv_check_variant_name TYPE sci_chkv
        RAISING
          zcx_abapgit_exception.

  PROTECTED SECTION.
    TYPES:
      ty_tdevc_tt TYPE STANDARD TABLE OF tdevc WITH DEFAULT KEY .

    DATA:
      mv_package TYPE devclass.

    METHODS:
      create_variant
        RETURNING
          VALUE(ro_variant) TYPE REF TO cl_ci_checkvariant
        RAISING
          zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA:
      mv_check_variant_name TYPE sci_chkv,
      mo_inspection         TYPE REF TO cl_ci_inspection.

    METHODS:
      find_all_subpackages
        IMPORTING
          iv_package         TYPE devclass
        RETURNING
          VALUE(rt_packages) TYPE ty_tdevc_tt,

      create_objectset
        RETURNING
          VALUE(ro_set) TYPE REF TO cl_ci_objectset,

      run_inspection
        IMPORTING
          io_inspection  TYPE REF TO cl_ci_inspection
        RETURNING
          VALUE(rt_list) TYPE scit_alvlist,

      create_inspection
        IMPORTING
          io_set               TYPE REF TO cl_ci_objectset
          io_variant           TYPE REF TO cl_ci_checkvariant
        RETURNING
          VALUE(ro_inspection) TYPE REF TO cl_ci_inspection.

ENDCLASS.



CLASS zcl_abapgit_code_inspector IMPLEMENTATION.


  METHOD constructor.

    mv_package = iv_package.
    mv_check_variant_name = iv_check_variant_name.

  ENDMETHOD.


  METHOD create_inspection.

    cl_ci_inspection=>create(
      EXPORTING
        p_user           = sy-uname
        p_name           = ''
      RECEIVING
        p_ref            = ro_inspection
      EXCEPTIONS
        locked           = 1
        error_in_enqueue = 2
        not_authorized   = 3
        OTHERS           = 4 ).
    ASSERT sy-subrc = 0.

    ro_inspection->set(
      p_chkv = io_variant
      p_objs = io_set ).

  ENDMETHOD.


  METHOD create_objectset.

    DATA: lt_objs     TYPE scit_objs,
          lt_packages TYPE ty_tdevc_tt.


    lt_packages = find_all_subpackages( mv_package ).
    IF lines( lt_packages ) = 0.
      RETURN.
    ENDIF.

    SELECT object AS objtype obj_name AS objname
      FROM tadir
      INTO CORRESPONDING FIELDS OF TABLE lt_objs
      FOR ALL ENTRIES IN lt_packages
      WHERE devclass = lt_packages-devclass
      AND delflag = abap_false
      AND pgmid = 'R3TR'.                               "#EC CI_GENBUFF

    ro_set = cl_ci_objectset=>save_from_list( lt_objs ).

  ENDMETHOD.


  METHOD create_variant.

    IF mv_check_variant_name IS INITIAL.
      zcx_abapgit_exception=>raise( |No check variant supplied.| ).
    ENDIF.

    cl_ci_checkvariant=>get_ref(
      EXPORTING
        p_user                   = ''
        p_name                   = mv_check_variant_name
      RECEIVING
        p_ref                    = ro_variant
      EXCEPTIONS
        chkv_not_exists          = 1
        missing_parameter        = 2
        OTHERS                   = 3 ).

    CASE sy-subrc.
      WHEN 1.
        zcx_abapgit_exception=>raise( |Check variant { mv_check_variant_name } doesn't exist| ).
      WHEN 2.
        zcx_abapgit_exception=>raise( |Parameter missing for check variant { mv_check_variant_name }| ).
    ENDCASE.

  ENDMETHOD.


  METHOD find_all_subpackages.

* TODO, in the future, move this method to the ABAPGIT global package class

    DATA: ls_package LIKE LINE OF rt_packages,
          lt_found   LIKE rt_packages,
          lt_sub     LIKE rt_packages.


    SELECT SINGLE * FROM tdevc INTO ls_package WHERE devclass = iv_package.
    ASSERT sy-subrc = 0.
    APPEND ls_package TO rt_packages.

    SELECT * FROM tdevc APPENDING TABLE lt_sub
      WHERE parentcl = ls_package-devclass.

    LOOP AT lt_sub INTO ls_package.
      lt_found = find_all_subpackages( ls_package-devclass ).
      APPEND LINES OF lt_found TO rt_packages.
    ENDLOOP.

  ENDMETHOD.


  METHOD run_inspection.

    io_inspection->run(
      EXCEPTIONS
        invalid_check_version = 1
        OTHERS                = 2 ).
    ASSERT sy-subrc = 0.

    io_inspection->plain_list(
      IMPORTING
        p_list = rt_list ).

  ENDMETHOD.


  METHOD zif_abapgit_code_inspector~run.

    DATA: lo_set     TYPE REF TO cl_ci_objectset,
          lo_variant TYPE REF TO cl_ci_checkvariant.

    lo_set = create_objectset( ).
    lo_variant = create_variant( ).

    mo_inspection = create_inspection(
      io_set     = lo_set
      io_variant = lo_variant ).

    rt_list = run_inspection( mo_inspection ).

  ENDMETHOD.

  METHOD validate_check_variant.

    cl_ci_checkvariant=>get_ref(
      EXPORTING
        p_user                   = ''
        p_name                   = iv_check_variant_name
      EXCEPTIONS
        chkv_not_exists          = 1
        missing_parameter        = 2
        OTHERS                   = 3 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |No valid check variant { iv_check_variant_name  }| ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_code_inspector~get_inspection.
    ro_inspection = mo_inspection.
  ENDMETHOD.

ENDCLASS.
