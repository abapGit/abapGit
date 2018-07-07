CLASS zcl_abapgit_factory DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_injector.

  PUBLIC SECTION.

    CLASS-METHODS:
      get_tadir
        RETURNING
          VALUE(ri_tadir) TYPE REF TO zif_abapgit_tadir,

      get_sap_package
        IMPORTING
          iv_package            TYPE devclass
        RETURNING
          VALUE(ri_sap_package) TYPE REF TO zif_abapgit_sap_package,

      get_code_inspector
        IMPORTING
          iv_package               TYPE devclass
          iv_check_variant_name    TYPE sci_chkv
        RETURNING
          VALUE(ri_code_inspector) TYPE REF TO zif_abapgit_code_inspector
        RAISING
          zcx_abapgit_exception,

      get_syntax_check
        IMPORTING
          iv_package             TYPE devclass
        RETURNING
          VALUE(ri_syntax_check) TYPE REF TO zif_abapgit_code_inspector
        raising
          zcx_abapgit_exception.


  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_sap_package,
        package  TYPE devclass,
        instance TYPE REF TO zif_abapgit_sap_package,
      END OF ty_sap_package,
      tty_sap_package TYPE HASHED TABLE OF ty_sap_package
                      WITH UNIQUE KEY package,

      BEGIN OF ty_code_inspector,
        package            TYPE devclass,
        check_variant_name TYPE sci_chkv,
        instance           TYPE REF TO zif_abapgit_code_inspector,
      END OF ty_code_inspector,
      tty_code_inspector TYPE HASHED TABLE OF ty_code_inspector
                         WITH UNIQUE KEY package check_variant_name,
      BEGIN OF ty_syntax_check,
        package  TYPE devclass,
        instance TYPE REF TO zif_abapgit_code_inspector,
      END OF ty_syntax_check,
      tty_syntax_check TYPE HASHED TABLE OF ty_syntax_check
                       WITH UNIQUE KEY package.

    CLASS-DATA:
      mi_tadir          TYPE REF TO zif_abapgit_tadir,
      mt_sap_package    TYPE tty_sap_package,
      mt_code_inspector TYPE tty_code_inspector,
      mt_syntax_check   TYPE tty_syntax_check.

ENDCLASS.



CLASS zcl_abapgit_factory IMPLEMENTATION.

  METHOD get_tadir.

    IF mi_tadir IS INITIAL.
      CREATE OBJECT mi_tadir TYPE zcl_abapgit_tadir.
    ENDIF.

    ri_tadir = mi_tadir.

  ENDMETHOD.

  METHOD get_sap_package.

    DATA: ls_sap_package TYPE ty_sap_package.
    FIELD-SYMBOLS: <ls_sap_package> TYPE ty_sap_package.

    READ TABLE mt_sap_package ASSIGNING <ls_sap_package>
                              WITH TABLE KEY package = iv_package.
    IF sy-subrc <> 0.

      ls_sap_package-package = iv_package.
      CREATE OBJECT ls_sap_package-instance TYPE zcl_abapgit_sap_package
        EXPORTING
          iv_package = iv_package.

      INSERT ls_sap_package
             INTO TABLE mt_sap_package
             ASSIGNING <ls_sap_package>.

    ENDIF.

    ri_sap_package = <ls_sap_package>-instance.

  ENDMETHOD.

  METHOD get_code_inspector.

    DATA: ls_code_inspector LIKE LINE OF mt_code_inspector.
    FIELD-SYMBOLS: <ls_code_inspector> TYPE zcl_abapgit_factory=>ty_code_inspector.

    READ TABLE mt_code_inspector ASSIGNING <ls_code_inspector>
                                 WITH TABLE KEY package            = iv_package
                                                check_variant_name = iv_check_variant_name.
    IF sy-subrc <> 0.
      ls_code_inspector-package = iv_package.
      ls_code_inspector-check_variant_name = iv_check_variant_name.

      CREATE OBJECT ls_code_inspector-instance TYPE zcl_abapgit_code_inspector
        EXPORTING
          iv_package            = iv_package
          iv_check_variant_name = iv_check_variant_name.

      INSERT ls_code_inspector
             INTO TABLE mt_code_inspector
             ASSIGNING <ls_code_inspector>.

    ENDIF.

    ri_code_inspector = <ls_code_inspector>-instance.

  ENDMETHOD.

  METHOD get_syntax_check.

    DATA: ls_syntax_check LIKE LINE OF mt_syntax_check.
    FIELD-SYMBOLS: <ls_syntax_check> TYPE zcl_abapgit_factory=>ty_syntax_check.

    READ TABLE mt_syntax_check ASSIGNING <ls_syntax_check>
                               WITH TABLE KEY package = iv_package.
    IF sy-subrc <> 0.
      ls_syntax_check-package =  iv_package.

      CREATE OBJECT ls_syntax_check-instance TYPE zcl_abapgit_syntax_check
        EXPORTING
          iv_package = iv_package.

      INSERT ls_syntax_check
             INTO TABLE mt_syntax_check
             ASSIGNING <ls_syntax_check>.

    ENDIF.

    ri_syntax_check = <ls_syntax_check>-instance.

  ENDMETHOD.

ENDCLASS.
