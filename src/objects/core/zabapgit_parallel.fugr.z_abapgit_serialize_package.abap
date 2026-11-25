FUNCTION z_abapgit_serialize_package.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_PACKAGE) TYPE  DEVCLASS
*"     VALUE(IV_FOLDER_LOGIC) TYPE  STRING DEFAULT 'PREFIX'
*"     VALUE(IV_MAIN_LANG_ONLY) TYPE  FLAG DEFAULT ''
*"     VALUE(IV_SHOW_LOG) TYPE  FLAG DEFAULT ''
*"  EXPORTING
*"     VALUE(EV_XSTRING) TYPE  XSTRING
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA:
    lx_error          TYPE REF TO zcx_abapgit_exception,
    lv_text           TYPE c LENGTH 200,
    ls_local_settings TYPE zif_abapgit_persistence=>ty_repo-local_settings,
    lo_dot_abapgit    TYPE REF TO zcl_abapgit_dot_abapgit,
    ls_result         TYPE zcl_abapgit_zip=>ty_export_result.

  TRY.
      ls_local_settings-main_language_only = iv_main_lang_only.

      lo_dot_abapgit = zcl_abapgit_dot_abapgit=>build_default( ).
      lo_dot_abapgit->set_folder_logic( iv_folder_logic ).

      ls_result = zcl_abapgit_zip=>export(
       is_local_settings = ls_local_settings
       iv_package        = iv_package
       iv_show_log       = iv_show_log
       io_dot_abapgit    = lo_dot_abapgit ).

      ev_xstring = ls_result-xstr.

    CATCH zcx_abapgit_exception INTO lx_error.
      lv_text = lx_error->get_text( ).
      MESSAGE s000(oo) RAISING error WITH
        lv_text+0(50)
        lv_text+50(50)
        lv_text+100(50)
        lv_text+150(50).
  ENDTRY.

ENDFUNCTION.
