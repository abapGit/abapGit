CLASS ltcl_folder_logic_helper DEFINITION FOR TESTING FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: test
      IMPORTING
                iv_starting TYPE string
                iv_top      TYPE devclass
                iv_logic    TYPE string
                iv_package  TYPE devclass
                iv_path     TYPE string
      RAISING   zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_folder_logic_helper IMPLEMENTATION.

  METHOD test.

    DATA: lv_path    TYPE string,
          lv_package TYPE devclass,
          lo_dot     TYPE REF TO zcl_abapgit_dot_abapgit.


    lo_dot = zcl_abapgit_dot_abapgit=>build_default( ).
    lo_dot->set_starting_folder( iv_starting ).
    lo_dot->set_folder_logic( iv_logic ).

    lv_package = zcl_abapgit_folder_logic=>get_instance( )->path_to_package(
      iv_top  = iv_top
      io_dot  = lo_dot
      iv_path = iv_path ).

    lv_path = zcl_abapgit_folder_logic=>get_instance( )->package_to_path(
      iv_top     = iv_top
      io_dot     = lo_dot
      iv_package = iv_package ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_package
      exp = iv_package ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_path
      exp = iv_path ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_folder_logic DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_sap_package.

  PRIVATE SECTION.
    CONSTANTS: c_top TYPE devclass VALUE '$TOP',
               c_src TYPE string VALUE '/src/'.

    METHODS:
      setup,
      prefix1 FOR TESTING RAISING zcx_abapgit_exception,
      prefix2 FOR TESTING RAISING zcx_abapgit_exception,
      prefix_error1 FOR TESTING RAISING zcx_abapgit_exception,
      full1 FOR TESTING RAISING zcx_abapgit_exception,
      full2 FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_folder_logic IMPLEMENTATION.

  METHOD zif_abapgit_sap_package~list_subpackages.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~list_superpackages.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~read_parent.
    rv_parentcl = '$TOP'.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~create_child.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~exists.
    rv_bool = abap_true.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~are_changes_recorded_in_tr_req.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~get_transport_type.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~get_transport_layer.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~create.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~create_local.
    RETURN.
  ENDMETHOD.

  METHOD setup.

    zcl_abapgit_injector=>set_sap_package( iv_package     = '$TOP'
                                           ii_sap_package = me ).

    zcl_abapgit_injector=>set_sap_package( iv_package     = '$TOP_FOO'
                                           ii_sap_package = me ).

    zcl_abapgit_injector=>set_sap_package( iv_package     = '$FOOBAR'
                                           ii_sap_package = me ).

  ENDMETHOD.

  METHOD prefix1.
    ltcl_folder_logic_helper=>test(
      iv_starting = c_src
      iv_top      = c_top
      iv_logic    = zif_abapgit_dot_abapgit=>c_folder_logic-prefix
      iv_package  = c_top
      iv_path     = c_src ).
  ENDMETHOD.

  METHOD prefix2.
    ltcl_folder_logic_helper=>test(
      iv_starting = c_src
      iv_top      = c_top
      iv_logic    = zif_abapgit_dot_abapgit=>c_folder_logic-prefix
      iv_package  = '$TOP_FOO'
      iv_path     = '/src/foo/' ).
  ENDMETHOD.

  METHOD prefix_error1.
* PREFIX mode, top package is $TOP, so all subpackages should be named $TOP_something
    TRY.
        ltcl_folder_logic_helper=>test(
          iv_starting = c_src
          iv_top      = c_top
          iv_logic    = zif_abapgit_dot_abapgit=>c_folder_logic-prefix
          iv_package  = '$FOOBAR'
          iv_path     = '/src/' ).
        cl_abap_unit_assert=>fail( 'Error expected' ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD full1.
    ltcl_folder_logic_helper=>test(
      iv_starting = c_src
      iv_top      = c_top
      iv_logic    = zif_abapgit_dot_abapgit=>c_folder_logic-full
      iv_package  = c_top
      iv_path     = c_src ).
  ENDMETHOD.

  METHOD full2.
    ltcl_folder_logic_helper=>test(
      iv_starting = c_src
      iv_top      = c_top
      iv_logic    = zif_abapgit_dot_abapgit=>c_folder_logic-full
      iv_package  = '$TOP_FOO'
      iv_path     = '/src/top_foo/' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_folder_logic_namespaces DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_sap_package.

  PRIVATE SECTION.
    CONSTANTS: c_top TYPE devclass VALUE '/TEST/TOOLS',
               c_src TYPE string VALUE '/src/'.

    METHODS:
      setup,
      prefix1 FOR TESTING RAISING zcx_abapgit_exception,
      prefix2 FOR TESTING RAISING zcx_abapgit_exception,
      full1 FOR TESTING RAISING zcx_abapgit_exception,
      full2 FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_folder_logic_namespaces IMPLEMENTATION.

  METHOD zif_abapgit_sap_package~list_subpackages.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~list_superpackages.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~are_changes_recorded_in_tr_req.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~read_parent.
    rv_parentcl = c_top.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~create_child.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~exists.
    rv_bool = abap_true.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~get_transport_type.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~get_transport_layer.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~create.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~create_local.
    RETURN.
  ENDMETHOD.

  METHOD setup.

    zcl_abapgit_injector=>set_sap_package( iv_package     = '/TEST/TOOLS'
                                           ii_sap_package = me ).

    zcl_abapgit_injector=>set_sap_package( iv_package     = '/TEST/T1'
                                           ii_sap_package = me ).

    zcl_abapgit_injector=>set_sap_package( iv_package     = '/TEST/TOOLS_T1'
                                           ii_sap_package = me ).

  ENDMETHOD.



  METHOD prefix1.
    ltcl_folder_logic_helper=>test(
      iv_starting = c_src
      iv_top      = c_top
      iv_logic    = zif_abapgit_dot_abapgit=>c_folder_logic-prefix
      iv_package  = c_top
      iv_path     = c_src ).
  ENDMETHOD.

  METHOD prefix2.
    ltcl_folder_logic_helper=>test(
      iv_starting = c_src
      iv_top      = c_top
      iv_logic    = zif_abapgit_dot_abapgit=>c_folder_logic-prefix
      iv_package  = '/TEST/TOOLS_T1'
      iv_path     = '/src/t1/' ).
  ENDMETHOD.

  METHOD full1.
    ltcl_folder_logic_helper=>test(
      iv_starting = c_src
      iv_top      = c_top
      iv_logic    = zif_abapgit_dot_abapgit=>c_folder_logic-full
      iv_package  = c_top
      iv_path     = c_src ).
  ENDMETHOD.

  METHOD full2.
    ltcl_folder_logic_helper=>test(
      iv_starting = c_src
      iv_top      = c_top
      iv_logic    = zif_abapgit_dot_abapgit=>c_folder_logic-full
      iv_package  = '/TEST/T1'
      iv_path     = '/src/#test#t1/' ).
  ENDMETHOD.

ENDCLASS.
