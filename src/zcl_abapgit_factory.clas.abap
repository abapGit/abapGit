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
    CLASS-METHODS get_code_inspector
      IMPORTING
        !iv_package              TYPE devclass
      RETURNING
        VALUE(ri_code_inspector) TYPE REF TO zif_abapgit_code_inspector
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_stage_logic
      RETURNING
        VALUE(ri_logic) TYPE REF TO zif_abapgit_stage_logic .
    CLASS-METHODS get_cts_api
      RETURNING
        VALUE(ri_cts_api) TYPE REF TO zif_abapgit_cts_api .
    CLASS-METHODS get_environment
      RETURNING
        VALUE(ri_environment) TYPE REF TO zif_abapgit_environment .
    CLASS-METHODS get_longtexts
      RETURNING
        VALUE(ri_longtexts) TYPE REF TO zif_abapgit_longtexts .
    CLASS-METHODS get_http_agent
      RETURNING
        VALUE(ri_http_agent) TYPE REF TO zif_abapgit_http_agent .
    CLASS-METHODS get_lxe_texts
      RETURNING
        VALUE(ri_lxe_texts) TYPE REF TO zif_abapgit_lxe_texts .
    CLASS-METHODS get_sap_namespace
      RETURNING
        VALUE(ri_namespace) TYPE REF TO zif_abapgit_sap_namespace .
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
    TYPES:
      BEGIN OF ty_code_inspector_pack,
        package  TYPE devclass,
        instance TYPE REF TO zif_abapgit_code_inspector,
      END OF ty_code_inspector_pack .
    TYPES:
      ty_code_inspector_packs TYPE HASHED TABLE OF ty_code_inspector_pack
                                       WITH UNIQUE KEY package .

    CLASS-DATA gi_tadir TYPE REF TO zif_abapgit_tadir .
    CLASS-DATA gt_sap_package TYPE ty_sap_packages .
    CLASS-DATA gt_code_inspector TYPE ty_code_inspector_packs .
    CLASS-DATA gi_stage_logic TYPE REF TO zif_abapgit_stage_logic .
    CLASS-DATA gi_cts_api TYPE REF TO zif_abapgit_cts_api .
    CLASS-DATA gi_environment TYPE REF TO zif_abapgit_environment .
    CLASS-DATA gi_longtext TYPE REF TO zif_abapgit_longtexts .
    CLASS-DATA gi_http_agent TYPE REF TO zif_abapgit_http_agent .
    CLASS-DATA gi_lxe_texts TYPE REF TO zif_abapgit_lxe_texts .
    CLASS-DATA gi_sap_namespace TYPE REF TO zif_abapgit_sap_namespace .
ENDCLASS.



CLASS ZCL_ABAPGIT_FACTORY IMPLEMENTATION.


  METHOD get_code_inspector.

    DATA: ls_code_inspector LIKE LINE OF gt_code_inspector.
    FIELD-SYMBOLS: <ls_code_inspector> TYPE ty_code_inspector_pack.

    READ TABLE gt_code_inspector ASSIGNING <ls_code_inspector>
      WITH TABLE KEY package = iv_package.
    IF sy-subrc <> 0.
      ls_code_inspector-package = iv_package.

      CREATE OBJECT ls_code_inspector-instance TYPE zcl_abapgit_code_inspector
        EXPORTING
          iv_package = iv_package.

      INSERT ls_code_inspector
             INTO TABLE gt_code_inspector
             ASSIGNING <ls_code_inspector>.

    ENDIF.

    ri_code_inspector = <ls_code_inspector>-instance.

  ENDMETHOD.


  METHOD get_cts_api.
    IF gi_cts_api IS NOT BOUND.
      CREATE OBJECT gi_cts_api TYPE zcl_abapgit_cts_api.
    ENDIF.

    ri_cts_api = gi_cts_api.
  ENDMETHOD.


  METHOD get_environment.
    IF gi_environment IS NOT BOUND.
      CREATE OBJECT gi_environment TYPE zcl_abapgit_environment.
    ENDIF.
    ri_environment = gi_environment.
  ENDMETHOD.


  METHOD get_http_agent.

    IF gi_http_agent IS BOUND.
      ri_http_agent = gi_http_agent.
    ELSE.
      ri_http_agent = zcl_abapgit_http_agent=>create( ).
    ENDIF.

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


  METHOD get_stage_logic.

    IF gi_stage_logic IS INITIAL.
      CREATE OBJECT gi_stage_logic
        TYPE zcl_abapgit_stage_logic.
    ENDIF.

    ri_logic = gi_stage_logic.

  ENDMETHOD.


  METHOD get_tadir.

    IF gi_tadir IS INITIAL.
      CREATE OBJECT gi_tadir TYPE zcl_abapgit_tadir.
    ENDIF.

    ri_tadir = gi_tadir.

  ENDMETHOD.
ENDCLASS.
