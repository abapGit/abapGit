CLASS zcl_abapgit_abap_language_vers DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_allowed_abap_langu_version TYPE c LENGTH 1 .

    METHODS get_abap_language_vers_by_objt
      IMPORTING
        !iv_object_type                      TYPE trobjtype
        !io_repo                             TYPE REF TO zif_abapgit_repo
        !iv_package                          TYPE devclass
      RETURNING
        VALUE(rv_allowed_abap_langu_version) TYPE ty_allowed_abap_langu_version .
    METHODS is_import_allowed
      IMPORTING
        !io_repo          TYPE REF TO zif_abapgit_repo
        !iv_package       TYPE devclass
      RETURNING
        VALUE(rv_allowed) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_allowed_abap_langu_version,
        standard             TYPE ty_allowed_abap_langu_version VALUE space,
        standard_source_code TYPE ty_allowed_abap_langu_version VALUE 'X',
        key_user             TYPE ty_allowed_abap_langu_version VALUE '2',
        cloud_development    TYPE ty_allowed_abap_langu_version VALUE '5',
      END OF c_allowed_abap_langu_version .

    METHODS get_abap_language_vers_by_devc
      IMPORTING
        !iv_package                     TYPE devclass
      RETURNING
        VALUE(rv_abap_language_version) TYPE string .
    METHODS get_abap_language_vers_by_repo
      IMPORTING
        !io_repo                        TYPE REF TO zif_abapgit_repo
      RETURNING
        VALUE(rv_abap_language_version) TYPE string .
ENDCLASS.



CLASS ZCL_ABAPGIT_ABAP_LANGUAGE_VERS IMPLEMENTATION.


  METHOD get_abap_language_vers_by_devc.

    DATA lv_abap_lang_version_swc TYPE string.
    DATA lo_abap_language_version_cfg TYPE REF TO object.

    TRY.
        CREATE OBJECT lo_abap_language_version_cfg TYPE ('IF_ABAP_LANGUAGE_VERSION_CFG').

        CALL METHOD ('CL_ABAP_LANGUAGE_VERSION_CFG')=>('GET_INSTANCE')
          RECEIVING
            ro_instance = lo_abap_language_version_cfg.

        CALL METHOD lo_abap_language_version_cfg->('IF_ABAP_LANGUAGE_VERSION_CFG~GET_PACKAGE_DEFAULT_VERSION')
          EXPORTING
            iv_package_name             = iv_package
          RECEIVING
            rv_default_language_version = lv_abap_lang_version_swc.

        CASE lv_abap_lang_version_swc.
          WHEN  c_allowed_abap_langu_version-standard.
            rv_abap_language_version = zif_abapgit_dot_abapgit=>c_abap_language_version-standard.
          WHEN c_allowed_abap_langu_version-standard_source_code.
            rv_abap_language_version = zif_abapgit_dot_abapgit=>c_abap_language_version-standard.
          WHEN c_allowed_abap_langu_version-key_user.
            rv_abap_language_version = zif_abapgit_dot_abapgit=>c_abap_language_version-key_user.
          WHEN c_allowed_abap_langu_version-cloud_development.
            rv_abap_language_version = zif_abapgit_dot_abapgit=>c_abap_language_version-cloud_development.
          WHEN OTHERS.
            rv_abap_language_version = zif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
        ENDCASE.
      CATCH cx_root.
        rv_abap_language_version = zif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
    ENDTRY.
  ENDMETHOD.


  METHOD get_abap_language_vers_by_objt.

    DATA lo_abap_language_version TYPE REF TO object.

    TRY.

        CREATE OBJECT lo_abap_language_version TYPE ('IF_ABAP_LANGUAGE_VERSION').

        CALL METHOD ('CL_ABAP_LANGUAGE_VERSION')=>('GET_INSTANCE')
          RECEIVING
            ro_instance = lo_abap_language_version.

        CALL METHOD lo_abap_language_version->('IF_ABAP_LANGUAGE_VERSION~GET_DEFAULT_VERSION')
          EXPORTING
            iv_object_type     = iv_object_type
            iv_package_name    = iv_package
          RECEIVING
            rv_default_version = rv_allowed_abap_langu_version.

      CATCH cx_root.
        rv_allowed_abap_langu_version =  zcl_abapgit_abap_language_vers=>c_allowed_abap_langu_version-standard.
    ENDTRY.

  ENDMETHOD.


  METHOD get_abap_language_vers_by_repo.
    rv_abap_language_version = io_repo->get_dot_abapgit( )->get_abap_language_version( ).
    IF rv_abap_language_version IS INITIAL.
      rv_abap_language_version = zif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
    ENDIF.
  ENDMETHOD.


  METHOD is_import_allowed.

    CASE get_abap_language_vers_by_repo( io_repo ).
      WHEN zif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
        rv_allowed = abap_true.
        RETURN.
      WHEN OTHERS.
        IF get_abap_language_vers_by_repo( io_repo ) = get_abap_language_vers_by_devc( iv_package ).
          rv_allowed = abap_true.
          RETURN.
        ELSEIF get_abap_language_vers_by_devc( iv_package ) = zif_abapgit_dot_abapgit=>c_abap_language_version-undefined AND
               get_abap_language_vers_by_repo( io_repo )    = zif_abapgit_dot_abapgit=>c_abap_language_version-standard.
          rv_allowed = abap_true.
          RETURN.
        ELSE.
          rv_allowed = abap_false.
          RETURN.
        ENDIF.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
