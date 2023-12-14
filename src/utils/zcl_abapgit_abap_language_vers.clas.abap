CLASS zcl_abapgit_abap_language_vers DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS:
      c_any_abap_language_version TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version VALUE '*',
      c_no_abap_language_version  TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version VALUE '-',
      c_feature_flag              TYPE string VALUE 'ALAV'.

    METHODS constructor
      IMPORTING
        !io_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit.

    METHODS get_abap_language_vers_by_objt
      IMPORTING
        !iv_object_type                      TYPE trobjtype
        !iv_package                          TYPE devclass
      RETURNING
        VALUE(rv_allowed_abap_langu_version) TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version.

    METHODS get_repo_abap_language_version
      RETURNING
        VALUE(rv_abap_language_version) TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version.

    METHODS is_import_allowed
      IMPORTING
        !iv_package       TYPE devclass
      RETURNING
        VALUE(rv_allowed) TYPE abap_bool.

    CLASS-METHODS check_abap_language_version
      IMPORTING
        !iv_abap_language_version TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version
        !is_item                  TYPE zif_abapgit_definitions=>ty_item
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit.

    " Depends on experimental feature flag and repo setting
    DATA mv_has_abap_language_vers TYPE abap_bool.

    METHODS get_default_abap_language_vers
      IMPORTING
        !iv_object_type                 TYPE trobjtype
      RETURNING
        VALUE(rv_abap_language_version) TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version.

    METHODS get_abap_language_vers_by_devc
      IMPORTING
        !iv_package                     TYPE devclass
      RETURNING
        VALUE(rv_abap_language_version) TYPE string.

    METHODS get_abap_language_vers_by_repo
      RETURNING
        VALUE(rv_abap_language_version) TYPE string.

    CLASS-METHODS get_description
      IMPORTING
        !iv_abap_language_version TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version
      RETURNING
        VALUE(rv_description)     TYPE string.

ENDCLASS.



CLASS zcl_abapgit_abap_language_vers IMPLEMENTATION.


  METHOD check_abap_language_version.

    " Check if ABAP language version matches repository setting
    IF is_item-abap_language_version IS NOT INITIAL AND iv_abap_language_version <> is_item-abap_language_version.
      zcx_abapgit_exception=>raise(
        |Object { is_item-obj_type } { is_item-obj_name } has { get_description( iv_abap_language_version ) }| &&
        | but repository is set to { get_description( is_item-abap_language_version ) }| ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    mo_dot_abapgit = io_dot_abapgit.

    IF zcl_abapgit_feature=>is_enabled( c_feature_flag ) = abap_false.
      mv_has_abap_language_vers = abap_undefined.
    ELSEIF get_abap_language_vers_by_repo( ) = zif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
      mv_has_abap_language_vers = abap_undefined.
    ELSEIF get_abap_language_vers_by_repo( ) = zif_abapgit_dot_abapgit=>c_abap_language_version-ignore.
      mv_has_abap_language_vers = abap_false.
    ELSE.
      mv_has_abap_language_vers = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD get_abap_language_vers_by_devc.

    DATA lv_class TYPE string.
    DATA lv_abap_lang_version_devc TYPE string.
    DATA lo_abap_language_version_cfg TYPE REF TO object.

    lv_class = 'CL_ABAP_LANGUAGE_VERSION_CFG'.

    TRY.
        CALL METHOD (lv_class)=>('GET_INSTANCE')
          RECEIVING
            ro_instance = lo_abap_language_version_cfg.

        " For non-existing packages, GET_PACKAGE_DEFAULT_VERSION returns "standard"
        " but we want to return "undefined" in this case to allow any new packages
        IF zcl_abapgit_factory=>get_sap_package( iv_package )->exists( ) = abap_true.
          CALL METHOD lo_abap_language_version_cfg->('IF_ABAP_LANGUAGE_VERSION_CFG~GET_PACKAGE_DEFAULT_VERSION')
            EXPORTING
              iv_package_name             = iv_package
            RECEIVING
              rv_default_language_version = lv_abap_lang_version_devc.
        ELSE.
          lv_abap_lang_version_devc = '-'.
        ENDIF.

        CASE lv_abap_lang_version_devc.
          WHEN zif_abapgit_aff_types_v1=>co_abap_language_version-standard.
            rv_abap_language_version = zif_abapgit_dot_abapgit=>c_abap_language_version-standard.
          WHEN zif_abapgit_aff_types_v1=>co_abap_language_version-key_user.
            rv_abap_language_version = zif_abapgit_dot_abapgit=>c_abap_language_version-key_user.
          WHEN zif_abapgit_aff_types_v1=>co_abap_language_version-cloud_development.
            rv_abap_language_version = zif_abapgit_dot_abapgit=>c_abap_language_version-cloud_development.
          WHEN OTHERS.
            rv_abap_language_version = zif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
        ENDCASE.

      CATCH cx_root.
        rv_abap_language_version = zif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
    ENDTRY.

  ENDMETHOD.


  METHOD get_abap_language_vers_by_objt.

    DATA lv_class TYPE string.
    DATA lo_abap_language_version TYPE REF TO object.

    IF mv_has_abap_language_vers = abap_undefined.
      rv_allowed_abap_langu_version = c_any_abap_language_version.
    ELSEIF mv_has_abap_language_vers = abap_false.
      rv_allowed_abap_langu_version = c_no_abap_language_version.
    ELSE. " abap_true

      lv_class = 'CL_ABAP_LANGUAGE_VERSION'.

      TRY.
          CALL METHOD (lv_class)=>('GET_INSTANCE')
            RECEIVING
              ro_version_handler = lo_abap_language_version.

          CALL METHOD lo_abap_language_version->('IF_ABAP_LANGUAGE_VERSION~GET_DEFAULT_VERSION')
            EXPORTING
              iv_object_type     = iv_object_type
              iv_package         = iv_package
            RECEIVING
              rv_default_version = rv_allowed_abap_langu_version.

        CATCH cx_root.
          rv_allowed_abap_langu_version = get_default_abap_language_vers( iv_object_type ).
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD get_abap_language_vers_by_repo.
    rv_abap_language_version = mo_dot_abapgit->get_abap_language_version( ).
    IF rv_abap_language_version IS INITIAL.
      rv_abap_language_version = zif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
    ENDIF.
  ENDMETHOD.


  METHOD get_default_abap_language_vers.

    IF zcl_abapgit_factory=>get_environment( )->is_sap_cloud_platform( ) = abap_true.
      " On BTP, default to ABAP for Cloud Development
      rv_abap_language_version = zif_abapgit_aff_types_v1=>co_abap_language_version_cloud-cloud_development.
    ELSE.
      " Differentiate between source code object and non-source code objects
      CASE iv_object_type.
        WHEN 'BDEF' OR 'CLAS' OR 'FUGR' OR 'FUGS' OR 'INTF' OR 'PROG' OR 'TYPE'.
          rv_abap_language_version = zif_abapgit_aff_types_v1=>co_abap_language_version_src-standard.
        WHEN OTHERS.
          rv_abap_language_version = zif_abapgit_aff_types_v1=>co_abap_language_version-standard.
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD get_description.

    CASE iv_abap_language_version.
      WHEN zif_abapgit_aff_types_v1=>co_abap_language_version-standard
        OR zif_abapgit_aff_types_v1=>co_abap_language_version_src-standard.
        rv_description = 'Standard ABAP'.
      WHEN zif_abapgit_aff_types_v1=>co_abap_language_version-key_user
        OR zif_abapgit_aff_types_v1=>co_abap_language_version_src-key_user.
        rv_description = 'ABAP for Key Users'.
      WHEN zif_abapgit_aff_types_v1=>co_abap_language_version-cloud_development
        OR zif_abapgit_aff_types_v1=>co_abap_language_version_src-cloud_development.
        rv_description = 'ABAP for Cloud Development'.
      WHEN OTHERS.
        rv_description = 'Undefined'.
    ENDCASE.

    rv_description = |ABAP language version "{ rv_description }"|.

  ENDMETHOD.


  METHOD get_repo_abap_language_version.

    DATA lv_abap_language_version TYPE string.

    IF mv_has_abap_language_vers <> abap_undefined. " abap_true or abap_false
      lv_abap_language_version = mo_dot_abapgit->get_abap_language_version( ).
    ENDIF.

    CASE lv_abap_language_version.
      WHEN zif_abapgit_dot_abapgit=>c_abap_language_version-standard.
        rv_abap_language_version = zif_abapgit_aff_types_v1=>co_abap_language_version_src-standard.
      WHEN zif_abapgit_dot_abapgit=>c_abap_language_version-key_user.
        rv_abap_language_version = zif_abapgit_aff_types_v1=>co_abap_language_version_src-key_user.
      WHEN zif_abapgit_dot_abapgit=>c_abap_language_version-cloud_development.
        rv_abap_language_version = zif_abapgit_aff_types_v1=>co_abap_language_version_src-cloud_development.
      WHEN zif_abapgit_dot_abapgit=>c_abap_language_version-ignore.
        rv_abap_language_version = c_no_abap_language_version.
      WHEN OTHERS. " undefined or feature off
        rv_abap_language_version = c_any_abap_language_version.
    ENDCASE.

  ENDMETHOD.


  METHOD is_import_allowed.

    DATA lv_package_version TYPE string.

    lv_package_version = get_abap_language_vers_by_devc( iv_package ).

    CASE get_abap_language_vers_by_repo( ).
      WHEN zif_abapgit_dot_abapgit=>c_abap_language_version-undefined
        OR zif_abapgit_dot_abapgit=>c_abap_language_version-ignore.
        rv_allowed = abap_true.
      WHEN OTHERS.
        IF get_abap_language_vers_by_repo( ) = lv_package_version.
          " allow packages that match repo setting
          rv_allowed = abap_true.
        ELSEIF lv_package_version = zif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
          " always allow new packages
          rv_allowed = abap_true.
        ELSE.
          rv_allowed = abap_false.
        ENDIF.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
