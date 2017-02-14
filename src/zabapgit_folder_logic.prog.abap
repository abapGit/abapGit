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
          VALUE(rv_path) TYPE string
        RAISING lcx_exception,
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

      CASE io_dot->get_folder_logic( ).
        WHEN lcl_dot_abapgit=>c_folder_logic-full.
          rv_package = lv_new.
          IF iv_top(1) = '$'.
            CONCATENATE '$' rv_package INTO rv_package.
          ENDIF.
        WHEN lcl_dot_abapgit=>c_folder_logic-prefix.
          CONCATENATE rv_package '_' lv_new INTO rv_package.
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.

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
          lv_message  TYPE string,
          lv_parentcl TYPE tdevc-parentcl.


    IF iv_top = iv_package.
      rv_path = io_dot->get_starting_folder( ).
    ELSE.
      lv_parentcl = lcl_sap_package=>get( iv_package )->read_parent( ).

      IF lv_parentcl IS INITIAL.
        lcx_exception=>raise( 'error, expected parent package' ).
      ELSE.
        CASE io_dot->get_folder_logic( ).
          WHEN lcl_dot_abapgit=>c_folder_logic-full.
            lv_len = 0.
            IF iv_package(1) = '$'.
              lv_len = 1.
            ENDIF.
          WHEN lcl_dot_abapgit=>c_folder_logic-prefix.
            lv_len = strlen( lv_parentcl ).
          WHEN OTHERS.
            ASSERT 0 = 1.
        ENDCASE.

        lv_path = iv_package+lv_len.
        IF strlen( lv_path ) = 0.
* if abapGit project is installed in package ZZZ, all subpackages should be named
* ZZZ_something. This will define the folder name in the zip file to be "something",
* similarily with online projects
          lv_message = 'Unexpected package naming(' && iv_package && ')' ##no_text.
          lcx_exception=>raise( lv_message ).
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
      test
        IMPORTING
          iv_logic   TYPE string
          iv_package TYPE devclass
          iv_path    TYPE string
        RAISING lcx_exception,
      prefix1 FOR TESTING RAISING lcx_exception,
      prefix2 FOR TESTING RAISING lcx_exception,
      full1 FOR TESTING RAISING lcx_exception,
      full2 FOR TESTING RAISING lcx_exception.

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
    test( iv_logic   = lcl_dot_abapgit=>c_folder_logic-prefix
          iv_package = lc_top
          iv_path    = lc_src ).
  ENDMETHOD.

  METHOD prefix2.
    test( iv_logic   = lcl_dot_abapgit=>c_folder_logic-prefix
          iv_package = '$TOP_FOO'
          iv_path    = '/src/foo/' ).
  ENDMETHOD.

  METHOD full1.
    test( iv_logic   = lcl_dot_abapgit=>c_folder_logic-full
          iv_package = lc_top
          iv_path    = lc_src ).
  ENDMETHOD.

  METHOD full2.
    test( iv_logic   = lcl_dot_abapgit=>c_folder_logic-full
          iv_package = '$TOP_FOO'
          iv_path    = '/src/top_foo/' ).
  ENDMETHOD.

  METHOD test.

    DATA: lv_path    TYPE string,
          lv_package TYPE devclass,
          lo_dot     TYPE REF TO lcl_dot_abapgit.


    lo_dot = lcl_dot_abapgit=>build_default( sy-langu ).
    lo_dot->set_starting_folder( lc_src ).
    lo_dot->set_folder_logic( iv_logic ).

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
