CLASS zcl_abapgit_injector DEFINITION
  PUBLIC
  CREATE PRIVATE
  FOR TESTING .

  PUBLIC SECTION.

    CLASS-METHODS:
      set_tadir
        IMPORTING
          !ii_tadir TYPE REF TO zif_abapgit_tadir,

      set_sap_package
        IMPORTING
          iv_package     TYPE devclass
          ii_sap_package TYPE REF TO zif_abapgit_sap_package.

ENDCLASS.



CLASS zcl_abapgit_injector IMPLEMENTATION.

  METHOD set_tadir.

    zcl_abapgit_factory=>mi_tadir = ii_tadir.

  ENDMETHOD.

  METHOD set_sap_package.

    DATA: ls_sap_package TYPE zcl_abapgit_factory=>ty_sap_package.
    FIELD-SYMBOLS: <ls_sap_package> TYPE zcl_abapgit_factory=>ty_sap_package.

    READ TABLE zcl_abapgit_factory=>mt_sap_package
         ASSIGNING <ls_sap_package>
         WITH TABLE KEY package = iv_package.

    IF sy-subrc <> 0.

      ls_sap_package-package = iv_package.
      INSERT ls_sap_package
             INTO TABLE zcl_abapgit_factory=>mt_sap_package
             ASSIGNING <ls_sap_package>.

    ENDIF.

    <ls_sap_package>-instance = ii_sap_package.

  ENDMETHOD.

ENDCLASS.
