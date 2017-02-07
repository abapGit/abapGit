*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_SERVICES_DB
*&---------------------------------------------------------------------*

CLASS lcl_services_db DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-METHODS delete
      IMPORTING is_key TYPE lcl_persistence_db=>ty_content
      RAISING   lcx_exception lcx_cancel.

    CLASS-METHODS update
      IMPORTING is_content TYPE lcl_persistence_db=>ty_content
      RAISING   lcx_exception.

ENDCLASS. "lcl_services_db

CLASS lcl_services_db IMPLEMENTATION.

  METHOD delete.

    DATA: lv_answer TYPE c LENGTH 1.

    ASSERT is_key-type IS NOT INITIAL.
    ASSERT is_key-value IS NOT INITIAL.

    lv_answer = lcl_popups=>popup_to_confirm(
      titlebar              = 'Warning'
      text_question         = 'Delete?'
      text_button_1         = 'Ok'
      icon_button_1         = 'ICON_DELETE'
      text_button_2         = 'Cancel'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = abap_false ).                 "#EC NOTEXT

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE lcx_cancel.
    ENDIF.

    lcl_app=>db( )->delete(
      iv_type  = is_key-type
      iv_value = is_key-value ).

    COMMIT WORK.

  ENDMETHOD.  " delete

  METHOD update.

    ASSERT is_content-type IS NOT INITIAL.
    ASSERT is_content-value IS NOT INITIAL.

    lcl_app=>db( )->update(
      iv_type  = is_content-type
      iv_value = is_content-value
      iv_data  = is_content-data_str ).

    COMMIT WORK.

  ENDMETHOD.  "update

ENDCLASS. "lcl_services_db
