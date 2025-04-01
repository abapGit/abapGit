CLASS zcl_abapgit_factory DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_injector .

  PUBLIC SECTION.

    CLASS-METHODS get_tadir
      RETURNING
        VALUE(ri_tadir) TYPE REF TO zif_abapgit_tadir .
    CLASS-METHODS get_sap_package
      IMPORTING
        !iv_package           TYPE devclass
      RETURNING
        VALUE(ri_sap_package) TYPE REF TO zif_abapgit_sap_package .
    CLASS-METHODS get_cts_api
      RETURNING
        VALUE(ri_cts_api) TYPE REF TO zif_abapgit_cts_api .
    CLASS-METHODS get_default_transport
      RETURNING
        VALUE(ri_default_transport) TYPE REF TO zif_abapgit_default_transport.
    CLASS-METHODS get_environment
      RETURNING
        VALUE(ri_environment) TYPE REF TO zif_abapgit_environment .
    CLASS-METHODS get_longtexts
      RETURNING
        VALUE(ri_longtexts) TYPE REF TO zif_abapgit_longtexts .
    CLASS-METHODS get_lxe_texts
      RETURNING
        VALUE(ri_lxe_texts) TYPE REF TO zif_abapgit_lxe_texts .
    CLASS-METHODS get_sap_namespace
      RETURNING
        VALUE(ri_namespace) TYPE REF TO zif_abapgit_sap_namespace .
    CLASS-METHODS get_sap_report
      RETURNING
        VALUE(ri_report) TYPE REF TO zif_abapgit_sap_report.
    CLASS-METHODS get_function_module
      RETURNING
        VALUE(ri_function_module) TYPE REF TO zif_abapgit_function_module.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_sap_package,
        package  TYPE devclass,
        instance TYPE REF TO zif_abapgit_sap_package,
      END OF ty_sap_package .
    TYPES:
      ty_sap_packages TYPE HASHED TABLE OF ty_sap_package
                                    WITH UNIQUE KEY package .

    CLASS-DATA gi_tadir TYPE REF TO zif_abapgit_tadir .
    CLASS-DATA gt_sap_package TYPE ty_sap_packages .
    CLASS-DATA gi_cts_api TYPE REF TO zif_abapgit_cts_api .
    CLASS-DATA gi_environment TYPE REF TO zif_abapgit_environment .
    CLASS-DATA gi_longtext TYPE REF TO zif_abapgit_longtexts .
    CLASS-DATA gi_lxe_texts TYPE REF TO zif_abapgit_lxe_texts .
    CLASS-DATA gi_sap_namespace TYPE REF TO zif_abapgit_sap_namespace .
    CLASS-DATA gi_sap_report TYPE REF TO zif_abapgit_sap_report.
    CLASS-DATA gi_function_module TYPE REF TO zif_abapgit_function_module.
    CLASS-DATA gi_default_transport TYPE REF TO zif_abapgit_default_transport .
ENDCLASS.



CLASS zcl_abapgit_factory IMPLEMENTATION.


  METHOD get_cts_api.
    IF gi_cts_api IS NOT BOUND.
      CREATE OBJECT gi_cts_api TYPE zcl_abapgit_cts_api.
    ENDIF.

    ri_cts_api = gi_cts_api.
  ENDMETHOD.


  METHOD get_default_transport.

    IF gi_default_transport IS NOT BOUND.
      CREATE OBJECT gi_default_transport TYPE zcl_abapgit_default_transport.
    ENDIF.

    ri_default_transport = gi_default_transport.

  ENDMETHOD.


  METHOD get_environment.
    IF gi_environment IS NOT BOUND.
      CREATE OBJECT gi_environment TYPE zcl_abapgit_environment.
    ENDIF.
    ri_environment = gi_environment.
  ENDMETHOD.


  METHOD get_function_module.

    IF gi_function_module IS INITIAL.
      CREATE OBJECT gi_function_module TYPE zcl_abapgit_function_module.
    ENDIF.

    ri_function_module = gi_function_module.

  ENDMETHOD.


  METHOD get_longtexts.

    IF gi_longtext IS NOT BOUND.
      CREATE OBJECT gi_longtext TYPE zcl_abapgit_longtexts.
    ENDIF.
    ri_longtexts = gi_longtext.

  ENDMETHOD.


  METHOD get_lxe_texts.

    IF gi_lxe_texts IS NOT BOUND.
      CREATE OBJECT gi_lxe_texts TYPE zcl_abapgit_lxe_texts.
    ENDIF.
    ri_lxe_texts = gi_lxe_texts.

  ENDMETHOD.


  METHOD get_sap_namespace.

    IF gi_sap_namespace IS NOT BOUND.
      CREATE OBJECT gi_sap_namespace TYPE zcl_abapgit_sap_namespace.
    ENDIF.

    ri_namespace = gi_sap_namespace.

  ENDMETHOD.


  METHOD get_sap_package.

    DATA: ls_sap_package TYPE ty_sap_package.
    FIELD-SYMBOLS: <ls_sap_package> TYPE ty_sap_package.

    READ TABLE gt_sap_package ASSIGNING <ls_sap_package>
                              WITH TABLE KEY package = iv_package.
    IF sy-subrc <> 0.

      ls_sap_package-package = iv_package.
      CREATE OBJECT ls_sap_package-instance TYPE zcl_abapgit_sap_package
        EXPORTING
          iv_package = iv_package.

      INSERT ls_sap_package
             INTO TABLE gt_sap_package
             ASSIGNING <ls_sap_package>.

    ENDIF.

    ri_sap_package = <ls_sap_package>-instance.

  ENDMETHOD.


  METHOD get_sap_report.

    IF gi_sap_report IS NOT BOUND.
      CREATE OBJECT gi_sap_report TYPE zcl_abapgit_sap_report.
    ENDIF.

    ri_report = gi_sap_report.

  ENDMETHOD.


  METHOD get_tadir.

    IF gi_tadir IS INITIAL.
      CREATE OBJECT gi_tadir TYPE zcl_abapgit_tadir.
    ENDIF.

    ri_tadir = gi_tadir.

  ENDMETHOD.
ENDCLASS.
