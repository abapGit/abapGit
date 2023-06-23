CLASS zcl_abapgit_i18n_params DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS new
      IMPORTING
        !iv_main_language      TYPE spras OPTIONAL
        !iv_main_language_only TYPE abap_bool DEFAULT abap_false
        !it_translation_langs  TYPE zif_abapgit_definitions=>ty_languages OPTIONAL
        !iv_use_lxe            TYPE abap_bool DEFAULT abap_false
        !is_params             TYPE zif_abapgit_definitions=>ty_i18n_params OPTIONAL
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_i18n_params.
    METHODS constructor
      IMPORTING
        !iv_main_language      TYPE spras OPTIONAL
        !iv_main_language_only TYPE abap_bool DEFAULT abap_false
        !it_translation_langs  TYPE zif_abapgit_definitions=>ty_languages OPTIONAL
        !iv_use_lxe            TYPE abap_bool DEFAULT abap_false
        !is_params             TYPE zif_abapgit_definitions=>ty_i18n_params OPTIONAL.

    DATA ms_params TYPE zif_abapgit_definitions=>ty_i18n_params READ-ONLY.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_I18N_PARAMS IMPLEMENTATION.


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


  METHOD new.
    CREATE OBJECT ro_instance
      EXPORTING
        iv_main_language      = iv_main_language
        iv_main_language_only = iv_main_language_only
        it_translation_langs  = it_translation_langs
        iv_use_lxe            = iv_use_lxe
        is_params             = is_params.
  ENDMETHOD.
ENDCLASS.
