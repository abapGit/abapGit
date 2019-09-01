class ZCL_ABAPGIT_EXCLUDED_PACKAGE definition
  public
  final
  create public .

public section.

  types TY_EXCLUDED_PACKAGES type STRING .
  types TY_T_EXCLUDED_PACKAGES type STRING_TABLE .

  constants:
    c_separator TYPE c LENGTH 1 value ';' ##NO_TEXT.

  methods GET_PACKAGES
    importing
      !IV_EXCLUDED_PACKAGES type TY_EXCLUDED_PACKAGES
    returning
      value(RT_R_PACKAGES) type RSELOPTION .
  methods TO_TABLE
    importing
      !IV_EXCLUDED_PACKAGES type TY_EXCLUDED_PACKAGES
    returning
      value(RT_PACKAGES) type TY_T_EXCLUDED_PACKAGES .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_EXCLUDED_PACKAGE IMPLEMENTATION.


  METHOD get_packages.

    DATA: lt_packages   TYPE ty_t_excluded_packages,
          ls_r_packages TYPE rsdsselopt,
          lt_r_packages TYPE rseloption.

    FIELD-SYMBOLS: <lv_packages> TYPE ty_excluded_packages.

    lt_packages = to_table( iv_excluded_packages ).

    WHILE lt_packages IS NOT INITIAL.

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
