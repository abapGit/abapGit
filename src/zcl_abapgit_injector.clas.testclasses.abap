CLASS ltcl_abapgit_tadir_mock DEFINITION FOR TESTING.

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_tadir.

ENDCLASS.

CLASS ltcl_no_dependency_injection DEFINITION FOR TESTING
                              RISK LEVEL HARMLESS
                              DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      no_injection FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_simple_dependency_inject DEFINITION FOR TESTING
                                  RISK LEVEL HARMLESS
                                  DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      setup,
      simple_injection FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_abapgit_tadir_mock IMPLEMENTATION.

  METHOD zif_abapgit_tadir~get_object_package.

  ENDMETHOD.

  METHOD zif_abapgit_tadir~read.

  ENDMETHOD.

  METHOD zif_abapgit_tadir~read_single.

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_no_dependency_injection IMPLEMENTATION.

  METHOD no_injection.

    DATA: li_tadir       TYPE REF TO zif_abapgit_tadir,
          lo_class_descr TYPE REF TO cl_abap_classdescr.

    li_tadir = zcl_abapgit_factory=>get_tadir( ).

    lo_class_descr ?= cl_abap_classdescr=>describe_by_object_ref( li_tadir ).

    cl_abap_unit_assert=>assert_equals(
      exp = '\CLASS=ZCL_ABAPGIT_TADIR'
      act = lo_class_descr->absolute_name ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_simple_dependency_inject IMPLEMENTATION.

  METHOD setup.

    DATA: lo_tadir_mock  TYPE REF TO ltcl_abapgit_tadir_mock.

    CREATE OBJECT lo_tadir_mock.

    zcl_abapgit_injector=>set_tadir( lo_tadir_mock ).

  ENDMETHOD.

  METHOD simple_injection.

    DATA: li_tadir       TYPE REF TO zif_abapgit_tadir,
          lo_class_descr TYPE REF TO cl_abap_classdescr.

    li_tadir = zcl_abapgit_factory=>get_tadir( ).

    lo_class_descr ?= cl_abap_classdescr=>describe_by_object_ref( li_tadir ).

    cl_abap_unit_assert=>assert_equals(
      exp = '\CLASS-POOL=ZCL_ABAPGIT_INJECTOR\CLASS=LTCL_ABAPGIT_TADIR_MOCK'
      act = lo_class_descr->absolute_name ).

  ENDMETHOD.

ENDCLASS.
