FUNCTION z_abapgit_serialize_batch.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_OBJECTS) TYPE  XSTRING
*"     VALUE(IV_ABAP_LANGUAGE_VERS) TYPE  UCCHECK
*"     VALUE(IV_LANGUAGE) TYPE  SY-LANGU
*"     VALUE(IV_SUPPRESS_PO_COMMENTS) TYPE  CHAR1
*"     VALUE(IT_TRANSLATION_LANGS) TYPE  TFPLAISO
*"     VALUE(IV_USE_LXE) TYPE  CHAR1
*"  EXPORTING
*"     VALUE(EV_RESULT) TYPE  XSTRING
*"----------------------------------------------------------------------
  " Serializes several objects in one task, see ZCL_ABAPGIT_SERIALIZE

  DATA: lt_objects     TYPE zcl_abapgit_serialize=>ty_parallel_objects,
        lt_results     TYPE zcl_abapgit_serialize=>ty_parallel_results,
        ls_result      LIKE LINE OF lt_results,
        lx_error       TYPE REF TO zcx_abapgit_exception,
        ls_i18n_params TYPE zif_abapgit_definitions=>ty_i18n_params.

  FIELD-SYMBOLS <ls_object> LIKE LINE OF lt_objects.

  " Without objects the task would return no results, and its objects would look deleted
  IMPORT data = lt_objects FROM DATA BUFFER iv_objects. "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

  ls_i18n_params-main_language         = iv_language.
  ls_i18n_params-suppress_po_comments  = iv_suppress_po_comments.
  ls_i18n_params-use_lxe               = iv_use_lxe.
  ls_i18n_params-translation_languages = it_translation_langs.

  LOOP AT lt_objects ASSIGNING <ls_object>.
    CLEAR ls_result.
    ls_result-path                                 = <ls_object>-tadir-path.
    ls_result-file_item-item-obj_type              = <ls_object>-tadir-object.
    ls_result-file_item-item-obj_name              = <ls_object>-tadir-obj_name.
    ls_result-file_item-item-devclass              = <ls_object>-tadir-devclass.
    ls_result-file_item-item-srcsystem             = <ls_object>-tadir-srcsystem.
    ls_result-file_item-item-origlang              = <ls_object>-tadir-masterlang.
    ls_result-file_item-item-abap_language_version = iv_abap_language_vers.

    " TADIR rows of transportable objects remain until the transport is released
    IF zcl_abapgit_objects=>exists( ls_result-file_item-item ) = abap_false.
      ls_result-not_found = abap_true.
    ELSE.
      ls_i18n_params-main_language_only = <ls_object>-main_language_only.
      TRY.
          ls_result-file_item = zcl_abapgit_objects=>serialize(
            is_item        = ls_result-file_item-item
            io_i18n_params = zcl_abapgit_i18n_params=>new( is_params = ls_i18n_params ) ).
        CATCH zcx_abapgit_exception INTO lx_error.
          ls_result-error = lx_error->get_text( ).
      ENDTRY.
    ENDIF.

    APPEND ls_result TO lt_results.
  ENDLOOP.

  EXPORT data = lt_results TO DATA BUFFER ev_result.

ENDFUNCTION.
