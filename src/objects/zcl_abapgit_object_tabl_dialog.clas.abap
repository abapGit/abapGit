CLASS zcl_abapgit_object_tabl_dialog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_message TYPE string.
    INTERFACES: zif_abapgit_comparison_result.

  PRIVATE SECTION.
    DATA mv_message TYPE string.
    DATA mv_halt TYPE string.

ENDCLASS.



CLASS zcl_abapgit_object_tabl_dialog IMPLEMENTATION.


  METHOD constructor.
    mv_message = iv_message.
  ENDMETHOD.


  METHOD zif_abapgit_comparison_result~is_result_complete_halt.
    rv_response = mv_halt.
  ENDMETHOD.


  METHOD zif_abapgit_comparison_result~show_confirmation_dialog.

    DATA lv_answer TYPE string.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Warning'
        text_question         = mv_message
        text_button_1         = 'Abort'
        icon_button_1         = 'ICON_CANCEL'
        text_button_2         = 'Pull anyway'
        icon_button_2         = 'ICON_OKAY'
        default_button        = '2'
        display_cancel_button = abap_false
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.                        "#EC NOTEXT
    IF sy-subrc <> 0 OR lv_answer = 1.
      mv_halt = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
