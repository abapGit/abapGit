class ZCL_ABAPGIT_SYNTAX_CHECK definition
  public
  create public .

public section.

  class-methods RUN
    importing
      !IV_PACKAGE type DEVCLASS
    returning
      value(RT_LIST) type SCIT_ALVLIST .
protected section.

  class-methods CREATE_INSPECTION
    importing
      !IO_SET type ref to CL_CI_OBJECTSET
      !IO_VARIANT type ref to CL_CI_CHECKVARIANT
    returning
      value(RO_INSPECTION) type ref to CL_CI_INSPECTION .
  class-methods CREATE_OBJECTSET
    importing
      !IV_PACKAGE type DEVCLASS
    returning
      value(RO_SET) type ref to CL_CI_OBJECTSET .
  class-methods CREATE_VARIANT
    returning
      value(RO_VARIANT) type ref to CL_CI_CHECKVARIANT .
  class-methods RUN_INSPECTION
    importing
      !IO_INSPECTION type ref to CL_CI_INSPECTION
    returning
      value(RT_LIST) type SCIT_ALVLIST .
private section.
ENDCLASS.



CLASS ZCL_ABAPGIT_SYNTAX_CHECK IMPLEMENTATION.


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
      p_chkv    = io_variant
      p_objs    = io_set
*    p_deldate = CONV sci_deldat( sy-datum + 1 )
      ).

  ENDMETHOD.


  METHOD create_objectset.

    DATA: lt_objs     TYPE scit_objs,
          lt_packages TYPE cl_pak_package_queries=>tt_subpackage_info,
          ls_package  LIKE LINE OF lt_packages,
          ls_obj      LIKE LINE OF lt_objs.


    cl_pak_package_queries=>get_all_subpackages(
      EXPORTING
        im_package             = iv_package
        im_also_local_packages = abap_true
      IMPORTING
        et_subpackages         = lt_packages ).

    ls_package-package = iv_package.
    INSERT ls_package INTO TABLE lt_packages.

* todo
    ls_obj-objtype = 'PROG'.
    ls_obj-objname = 'ZABAPGIT'.
    INSERT ls_obj INTO TABLE lt_objs.

    ro_set = cl_ci_objectset=>save_from_list( lt_objs ).

  ENDMETHOD.


  METHOD create_variant.

    DATA: lt_variant TYPE sci_tstvar,
          ls_variant LIKE LINE OF lt_variant.


    cl_ci_checkvariant=>create(
      EXPORTING
        p_user              = sy-uname
      RECEIVING
        p_ref               = ro_variant
      EXCEPTIONS
        chkv_already_exists = 1
        locked              = 2
        error_in_enqueue    = 3
        not_authorized      = 4
        OTHERS              = 5 ).
    ASSERT sy-subrc = 0.

    ls_variant-testname = 'CL_CI_TEST_SYNTAX_CHECK'.
    INSERT ls_variant INTO TABLE lt_variant.

    ro_variant->set_variant(
      EXPORTING
        p_variant    = lt_variant
      EXCEPTIONS
        not_enqueued = 1
        OTHERS       = 2 ).
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD run.

    DATA: lo_set        TYPE REF TO cl_ci_objectset,
          lo_inspection TYPE REF TO cl_ci_inspection,
          lo_variant    TYPE REF TO cl_ci_checkvariant.


    lo_set = create_objectset( iv_package ).
    lo_variant = create_variant( ).

    lo_inspection = create_inspection(
      io_set     = lo_set
      io_variant = lo_variant ).

    rt_list = run_inspection( lo_inspection ).

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
ENDCLASS.
