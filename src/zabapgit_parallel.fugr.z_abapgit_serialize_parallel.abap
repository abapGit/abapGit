FUNCTION z_abapgit_serialize_parallel.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_OBJ_TYPE) TYPE  TADIR-OBJECT
*"     VALUE(IV_OBJ_NAME) TYPE  TADIR-OBJ_NAME
*"     VALUE(IV_DEVCLASS) TYPE  TADIR-DEVCLASS
*"     VALUE(IV_LANGUAGE) TYPE  SY-LANGU
*"     VALUE(IV_PATH) TYPE  STRING
*"  EXPORTING
*"     VALUE(EV_RESULT) TYPE  XSTRING
*"     VALUE(EV_PATH) TYPE  STRING
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item,
        lx_error TYPE REF TO zcx_abapgit_exception,
        lv_text  TYPE c LENGTH 200,
        lt_files TYPE zcl_abapgit_objects=>ty_serialization.


  TRY.
      ls_item-obj_type = iv_obj_type.
      ls_item-obj_name = iv_obj_name.
      ls_item-devclass = iv_devclass.

      lt_files = zcl_abapgit_objects=>serialize(
        is_item     = ls_item
        iv_language = iv_language ).

      EXPORT data = lt_files TO DATA BUFFER ev_result.
      ev_path = iv_path.

    CATCH zcx_abapgit_exception INTO lx_error.
      lv_text = lx_error->get_text( ).
      MESSAGE s000(oo) RAISING error WITH
        lv_text+0(50)
        lv_text+50(50)
        lv_text+100(50)
        lv_text+150(50).
  ENDTRY.

ENDFUNCTION.
