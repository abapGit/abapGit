CLASS zcl_abapgit_password_dialog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS popup
      IMPORTING
        !iv_repo_url TYPE string
      CHANGING
        !cv_user     TYPE string
        !cv_pass     TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_PASSWORD_DIALOG IMPLEMENTATION.


  METHOD popup.

    DATA lv_gui_is_available TYPE abap_bool.

    lv_gui_is_available = zcl_abapgit_ui_factory=>get_gui_functions( )->gui_is_available( ).

    IF lv_gui_is_available = abap_true.
      PERFORM password_popup
        IN PROGRAM (sy-cprog)
        USING iv_repo_url
        CHANGING cv_user cv_pass.
    ELSE.
      "check if ZCL_ABAPGIT_DEFAULT_AUTH_INFO exists (part of ADT_Backend project)
      TRY.
          CALL METHOD ('ZCL_ABAPGIT_DEFAULT_AUTH_INFO')=>('GET_USER')
            RECEIVING
              rv_user = cv_user.
          CATCH cx_root.
            RETURN.
      ENDTRY.
      TRY.
          CALL METHOD ('ZCL_ABAPGIT_DEFAULT_AUTH_INFO')=>('GET_PASSWORD')
            RECEIVING
              rv_password = cv_pass.
        CATCH cx_root.
          "check if old version with typo in method name exists
          TRY.
              CALL METHOD ('ZCL_ABAPGIT_DEFAULT_AUTH_INFO')=>('GET_PASSOWORD')
                RECEIVING
                  rv_password = cv_pass.
            CATCH cx_root.
              RETURN.
          ENDTRY.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
