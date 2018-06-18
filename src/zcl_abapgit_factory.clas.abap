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
          VALUE(ri_sap_package) TYPE REF TO zif_abapgit_sap_package.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_sap_package,
        package  TYPE devclass,
        instance TYPE REF TO zif_abapgit_sap_package,
      END OF ty_sap_package,
      tty_sap_package TYPE HASHED TABLE OF ty_sap_package
                      WITH UNIQUE KEY package.

    CLASS-DATA:
      mi_tadir       TYPE REF TO zif_abapgit_tadir,
      mt_sap_package TYPE tty_sap_package.

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

ENDCLASS.
