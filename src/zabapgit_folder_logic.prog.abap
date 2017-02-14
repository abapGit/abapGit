*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_FOLDER_LOGIC
*&---------------------------------------------------------------------*

CLASS lcl_folder_logic DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      class_to_path
        IMPORTING
          iv_top         TYPE devclass
          iv_start       TYPE string
          iv_package     TYPE devclass
        RETURNING
          VALUE(rv_path) TYPE string.

    CLASS-METHODS
      path_to_package
        IMPORTING iv_top            TYPE devclass
                  iv_start          TYPE string
                  iv_path           TYPE string
        RETURNING VALUE(rv_package) TYPE devclass
        RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_folder_logic IMPLEMENTATION.

  METHOD path_to_package.

    DATA: lv_length TYPE i,
          lv_parent TYPE devclass,
          lv_new    TYPE string,
          lv_path   TYPE string.


    lv_length = strlen( iv_start ).
    lv_path = iv_path+lv_length.
    lv_parent = iv_top.
    rv_package = iv_top.

    WHILE lv_path CA '/'.
      SPLIT lv_path AT '/' INTO lv_new lv_path.

      CONCATENATE rv_package '_' lv_new INTO rv_package.
      TRANSLATE rv_package TO UPPER CASE.

      IF lcl_sap_package=>exists( rv_package ) = abap_false.
        lcl_sap_package=>create_child( iv_parent = lv_parent
                                       iv_child  = rv_package ).
      ENDIF.

      lv_parent = rv_package.
    ENDWHILE.

  ENDMETHOD.

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
        IF strlen( lv_path ) = 0.
          RETURN. " prevent dump
        ENDIF.
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

ENDCLASS.
