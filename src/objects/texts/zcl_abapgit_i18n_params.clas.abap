CLASS zcl_abapgit_i18n_params DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA ms_params TYPE zif_abapgit_definitions=>ty_i18n_params READ-ONLY .

    CLASS-METHODS new_english
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_i18n_params .
    CLASS-METHODS new
      IMPORTING
        !iv_main_language TYPE spras OPTIONAL
        !iv_main_language_only TYPE abap_bool DEFAULT abap_false
        !it_translation_langs TYPE zif_abapgit_definitions=>ty_languages OPTIONAL
        !iv_use_lxe TYPE abap_bool DEFAULT abap_false
        !is_params TYPE zif_abapgit_definitions=>ty_i18n_params OPTIONAL
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_i18n_params .
    METHODS constructor
      IMPORTING
        !iv_main_language TYPE spras OPTIONAL
        !iv_main_language_only TYPE abap_bool DEFAULT abap_false
        !it_translation_langs TYPE zif_abapgit_definitions=>ty_languages OPTIONAL
        !iv_use_lxe TYPE abap_bool DEFAULT abap_false
        !is_params TYPE zif_abapgit_definitions=>ty_i18n_params OPTIONAL .
    METHODS is_lxe_applicable
      RETURNING
        VALUE(rv_yes) TYPE abap_bool .
    METHODS build_language_filter
      RETURNING
        VALUE(rt_language_filter) TYPE zif_abapgit_environment=>ty_system_language_filter .

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_language_filter TYPE zif_abapgit_environment=>ty_system_language_filter.

    CLASS-METHODS iso_langs_to_lang_filter
      IMPORTING
        it_iso_filter      TYPE zif_abapgit_definitions=>ty_languages
      RETURNING
        VALUE(rt_language_filter) TYPE zif_abapgit_environment=>ty_system_language_filter.

ENDCLASS.



CLASS ZCL_ABAPGIT_I18N_PARAMS IMPLEMENTATION.


  METHOD build_language_filter.
    IF mt_language_filter IS INITIAL.
      " translation_languages are includes, system langs are excludes, so the do not interfere
      IF ms_params-translation_languages IS NOT INITIAL.
        mt_language_filter = iso_langs_to_lang_filter( ms_params-translation_languages ).
      ELSE.
        mt_language_filter = zcl_abapgit_factory=>get_environment( )->get_system_language_filter( ).
      ENDIF.
    ENDIF.
    rt_language_filter = mt_language_filter.
  ENDMETHOD.


  METHOD constructor.
    IF is_params IS NOT INITIAL.
      ms_params = is_params.
    ELSE.
      ms_params-main_language         = iv_main_language.
      ms_params-main_language_only    = iv_main_language_only.
      ms_params-translation_languages = it_translation_langs.
      ms_params-use_lxe               = iv_use_lxe.
    ENDIF.
    ASSERT ms_params-main_language IS NOT INITIAL.
  ENDMETHOD.


  METHOD iso_langs_to_lang_filter.

    DATA lv_laiso LIKE LINE OF it_iso_filter.
    DATA lv_langu TYPE sy-langu.
    DATA ls_range LIKE LINE OF rt_language_filter.

    ls_range-sign = 'I'.
    ls_range-option = 'EQ'.

    LOOP AT it_iso_filter INTO lv_laiso.

      cl_i18n_languages=>sap2_to_sap1(
        EXPORTING
          im_lang_sap2  = lv_laiso
        RECEIVING
          re_lang_sap1  = lv_langu
        EXCEPTIONS
          no_assignment = 1
          OTHERS        = 2 ).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      ls_range-low = lv_langu.
      APPEND ls_range TO rt_language_filter.

    ENDLOOP.

  ENDMETHOD.


  METHOD is_lxe_applicable.

    rv_yes = boolc( ms_params-main_language_only = abap_false AND
       ms_params-use_lxe = abap_true AND
       ms_params-translation_languages IS NOT INITIAL ).

  ENDMETHOD.


  METHOD new.
    CREATE OBJECT ro_instance
      EXPORTING
        iv_main_language      = iv_main_language
        iv_main_language_only = iv_main_language_only
        it_translation_langs  = it_translation_langs
        iv_use_lxe            = iv_use_lxe
        is_params             = is_params.
  ENDMETHOD.


  METHOD new_english.
    CREATE OBJECT ro_instance
      EXPORTING
        iv_main_language = zif_abapgit_definitions=>c_english.
  ENDMETHOD.
ENDCLASS.
