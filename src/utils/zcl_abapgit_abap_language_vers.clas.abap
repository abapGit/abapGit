CLASS zcl_abapgit_abap_language_vers DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS c_feature_flag TYPE string VALUE 'ALAV'.

    METHODS constructor
      IMPORTING
        !io_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit.

    METHODS get_abap_language_version
      IMPORTING
        !iv_object_type                      TYPE trobjtype
        !iv_package                          TYPE devclass
        !iv_serialize_flag                   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_allowed_abap_langu_version) TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version.

    METHODS is_import_allowed
      IMPORTING
        !iv_package       TYPE devclass
      RETURNING
        VALUE(rv_allowed) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit.

    " Depends on experimental feature flag and repo setting
    DATA mv_feature_enabled TYPE abap_bool.

    METHODS get_default_abap_language_vers
      IMPORTING
        !iv_object_type                      TYPE trobjtype
      RETURNING
        VALUE(rv_default_abap_langu_version) TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version.

    METHODS get_abap_language_vers_by_devc
      IMPORTING
        !iv_package                     TYPE devclass
      RETURNING
        VALUE(rv_abap_language_version) TYPE string.

    METHODS get_abap_language_vers_by_repo
      RETURNING
        VALUE(rv_abap_language_version) TYPE string.

ENDCLASS.



CLASS zcl_abapgit_abap_language_vers IMPLEMENTATION.


  METHOD constructor.

    DATA lo_settings TYPE REF TO zcl_abapgit_settings.

    mo_dot_abapgit = io_dot_abapgit.

    lo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).

    IF lo_settings->is_feature_enabled( zcl_abapgit_abap_language_vers=>c_feature_flag ) = abap_false.
      mv_feature_enabled = abap_false.
    ELSEIF get_abap_language_vers_by_repo( ) = zif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
      mv_feature_enabled = abap_false.
    ELSE.
      mv_feature_enabled = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD get_abap_language_version.

    DATA lv_class TYPE string.
    DATA lo_abap_language_version TYPE REF TO object.

    IF mv_feature_enabled = abap_false.
      rv_allowed_abap_langu_version = get_default_abap_language_vers( iv_object_type ).
    ELSEIF iv_serialize_flag = abap_true.
      CLEAR rv_allowed_abap_langu_version.
    ELSE.

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


  METHOD get_abap_language_vers_by_devc.

    DATA lv_class TYPE string.
    DATA lv_abap_lang_version_devc TYPE string.
    DATA lo_abap_language_version_cfg TYPE REF TO object.

    lv_class = 'CL_ABAP_LANGUAGE_VERSION_CFG'.

    TRY.

        CALL METHOD (lv_class)=>('GET_INSTANCE')
          RECEIVING
            ro_instance = lo_abap_language_version_cfg.

        CALL METHOD lo_abap_language_version_cfg->('IF_ABAP_LANGUAGE_VERSION_CFG~GET_PACKAGE_DEFAULT_VERSION')
          EXPORTING
            iv_package_name             = iv_package
          RECEIVING
            rv_default_language_version = lv_abap_lang_version_devc.

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


  METHOD get_abap_language_vers_by_repo.
    rv_abap_language_version = mo_dot_abapgit->get_abap_language_version( ).
    IF rv_abap_language_version IS INITIAL.
      rv_abap_language_version = zif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
    ENDIF.
  ENDMETHOD.


  METHOD get_default_abap_language_vers.

    IF zcl_abapgit_factory=>get_environment( )->is_sap_cloud_platform( ) = abap_true.
      " On BTP, default to ABAP for Cloud Development
      rv_default_abap_langu_version = zif_abapgit_aff_types_v1=>co_abap_language_version_cloud-cloud_development.
    ELSE.
      " Differentiate between source code object and non-source code objects
      CASE iv_object_type.
        WHEN 'BDEF' OR 'CLAS' OR 'FUGR' OR 'FUGS' OR 'INTF' OR 'PROG'.
          rv_default_abap_langu_version = zif_abapgit_aff_types_v1=>co_abap_language_version_src-standard.
        WHEN OTHERS.
          rv_default_abap_langu_version = zif_abapgit_aff_types_v1=>co_abap_language_version-standard.
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD is_import_allowed.

    CASE get_abap_language_vers_by_repo( ).
      WHEN zif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
        rv_allowed = abap_true.
      WHEN OTHERS.
        IF get_abap_language_vers_by_repo( ) = get_abap_language_vers_by_devc( iv_package ).
          rv_allowed = abap_true.
        ELSEIF
        get_abap_language_vers_by_devc( iv_package ) = zif_abapgit_dot_abapgit=>c_abap_language_version-undefined AND
        get_abap_language_vers_by_repo( )            = zif_abapgit_dot_abapgit=>c_abap_language_version-standard.
          rv_allowed = abap_true.
        ELSE.
          rv_allowed = abap_false.
        ENDIF.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
