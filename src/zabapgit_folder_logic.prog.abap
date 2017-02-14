*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_FOLDER_LOGIC
*&---------------------------------------------------------------------*

CLASS lcl_folder_logic DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      package_to_path
        IMPORTING
          iv_top         TYPE devclass
          io_dot         TYPE REF TO lcl_dot_abapgit
          iv_package     TYPE devclass
        RETURNING
          VALUE(rv_path) TYPE string,
      path_to_package
        IMPORTING
          iv_top            TYPE devclass
          io_dot            TYPE REF TO lcl_dot_abapgit
          iv_path           TYPE string
        RETURNING
          VALUE(rv_package) TYPE devclass
        RAISING
          lcx_exception.

ENDCLASS.

CLASS lcl_folder_logic IMPLEMENTATION.

  METHOD path_to_package.

    DATA: lv_length TYPE i,
          lv_parent TYPE devclass,
          lv_new    TYPE string,
          lv_path   TYPE string.


    lv_length  = strlen( io_dot->get_starting_folder( ) ).
    lv_path    = iv_path+lv_length.
    lv_parent  = iv_top.
    rv_package = iv_top.

    WHILE lv_path CA '/'.
      SPLIT lv_path AT '/' INTO lv_new lv_path.

      CONCATENATE rv_package '_' lv_new INTO rv_package.
      TRANSLATE rv_package TO UPPER CASE.

      IF lcl_sap_package=>get( rv_package )->exists( ) = abap_false.
        lcl_sap_package=>get( rv_package )->create_child( rv_package ).
      ENDIF.

      lv_parent = rv_package.
    ENDWHILE.

  ENDMETHOD.

  METHOD package_to_path.

    DATA: lv_len      TYPE i,
          lv_path     TYPE string,
          lv_parentcl TYPE tdevc-parentcl.


    IF iv_top = iv_package.
      rv_path = io_dot->get_starting_folder( ).
    ELSE.
      lv_parentcl = lcl_sap_package=>get( iv_package )->read_parent( ).

      IF lv_parentcl IS INITIAL.
        rv_path = 'error' ##no_text.
      ELSE.
        lv_len = strlen( lv_parentcl ).
        lv_path = iv_package+lv_len.
        IF strlen( lv_path ) = 0.
          RETURN. " prevent dump
        ENDIF.
        IF lv_path(1) = '_'.
          lv_path = lv_path+1.
        ENDIF.
        TRANSLATE lv_path TO LOWER CASE.
        CONCATENATE lv_path '/' INTO lv_path.

        rv_path = package_to_path( iv_top     = iv_top
                                   io_dot     = io_dot
                                   iv_package = lv_parentcl ).

        CONCATENATE rv_path lv_path INTO rv_path.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "class_to_path

ENDCLASS.

CLASS ltcl_folder_logic DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PUBLIC SECTION.
    INTERFACES: lif_sap_package.

  PRIVATE SECTION.
    CONSTANTS: lc_top TYPE devclass VALUE '$TOP',
               lc_src TYPE string VALUE '/src/'.

    METHODS:
      setup,
      teardown,
      test_prefix
        IMPORTING
          iv_logic type string
          iv_package TYPE devclass
          iv_path TYPE string
        RAISING lcx_exception,
      prefix1 FOR TESTING RAISING lcx_exception,
      prefix2 FOR TESTING RAISING lcx_exception.

ENDCLASS.                    "ltcl_convert DEFINITION

CLASS ltcl_folder_logic IMPLEMENTATION.

  METHOD lif_sap_package~list_subpackages.
  ENDMETHOD.

  METHOD lif_sap_package~list_superpackages.
  ENDMETHOD.

  METHOD lif_sap_package~read_parent.
    rv_parentcl = '$TOP'.
  ENDMETHOD.

  METHOD lif_sap_package~create_child.
  ENDMETHOD.

  METHOD lif_sap_package~exists.
    rv_bool = abap_true.
  ENDMETHOD.

  METHOD setup.
    FIELD-SYMBOLS: <ls_inject> LIKE LINE OF lcl_sap_package=>gt_injected.

    CLEAR lcl_sap_package=>gt_injected.

    APPEND INITIAL LINE TO lcl_sap_package=>gt_injected ASSIGNING <ls_inject>.
    <ls_inject>-package = '$TOP'.
    <ls_inject>-object  = me.

    APPEND INITIAL LINE TO lcl_sap_package=>gt_injected ASSIGNING <ls_inject>.
    <ls_inject>-package = '$TOP_FOO'.
    <ls_inject>-object  = me.
  ENDMETHOD.

  METHOD teardown.
    CLEAR lcl_sap_package=>gt_injected.
  ENDMETHOD.

  METHOD prefix1.
    test_prefix( iv_logic   = lcl_dot_abapgit=>c_folder_logic-prefix
                 iv_package = lc_top
                 iv_path    = lc_src ).
  ENDMETHOD.

  METHOD prefix2.
    test_prefix( iv_logic   = lcl_dot_abapgit=>c_folder_logic-prefix
                 iv_package = '$TOP_FOO'
                 iv_path    = '/src/foo/' ).
  ENDMETHOD.

  METHOD test_prefix.

    DATA: lv_path    TYPE string,
          lv_package TYPE devclass,
          lo_dot     TYPE REF TO lcl_dot_abapgit.


    lo_dot = lcl_dot_abapgit=>build_default( sy-langu ).
    lo_dot->set_starting_folder( lc_src ).

    lv_package = lcl_folder_logic=>path_to_package(
      iv_top  = lc_top
      io_dot  = lo_dot
      iv_path = iv_path ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_package
      exp = iv_package ).

    lv_path = lcl_folder_logic=>package_to_path(
      iv_top     = lc_top
      io_dot     = lo_dot
      iv_package = iv_package ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_path
      exp = iv_path ).

  ENDMETHOD.

ENDCLASS.
