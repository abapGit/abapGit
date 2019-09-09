CLASS zcl_abapgit_excluded_package DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ty_excluded_packages TYPE string .
    TYPES ty_t_excluded_packages TYPE string_table .

    CONSTANTS:
      c_separator TYPE c LENGTH 1 VALUE ';' ##NO_TEXT.

    METHODS get_packages
      IMPORTING
        !iv_excluded_packages TYPE ty_excluded_packages
      RETURNING
        VALUE(rt_r_packages)  TYPE rseloption .
    METHODS to_table
      IMPORTING
        !iv_excluded_packages TYPE ty_excluded_packages
      RETURNING
        VALUE(rt_packages)    TYPE ty_t_excluded_packages .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_excluded_package IMPLEMENTATION.


  METHOD get_packages.

    DATA: lt_packages   TYPE ty_t_excluded_packages,
          ls_r_packages TYPE rsdsselopt,
          lt_r_packages TYPE rseloption.

    FIELD-SYMBOLS: <lv_packages> TYPE ty_excluded_packages.

    lt_packages = to_table( iv_excluded_packages ).

    WHILE lt_packages IS NOT INITIAL.

      CLEAR: lt_r_packages.

      LOOP AT lt_packages ASSIGNING <lv_packages> WHERE table_line CN ' _0'.

        ls_r_packages-sign = 'I'.
        ls_r_packages-option = 'EQ'.
        ls_r_packages-low = <lv_packages>.

        APPEND: ls_r_packages TO rt_r_packages,
                ls_r_packages TO lt_r_packages.

      ENDLOOP.

      CLEAR: lt_packages.

      SELECT devclass
          FROM tdevc
          INTO TABLE lt_packages
          WHERE parentcl IN lt_r_packages.

    ENDWHILE.

    SORT rt_r_packages ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_r_packages.

  ENDMETHOD.


  METHOD to_table.

    SPLIT iv_excluded_packages
      AT zcl_abapgit_excluded_package=>c_separator
      INTO TABLE rt_packages.

    SORT rt_packages.
    DELETE ADJACENT DUPLICATES FROM rt_packages.

  ENDMETHOD.
ENDCLASS.
