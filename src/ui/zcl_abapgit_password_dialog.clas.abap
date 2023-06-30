class ZCL_ABAPGIT_PASSWORD_DIALOG definition
  public
  final
  create public .

public section.

  class-methods POPUP
    importing
      !IV_REPO_URL type STRING
    changing
      !CV_USER type STRING
      !CV_PASS type STRING .

    METHODS unset_no_dialog.
    METHODS set_no_dialog.

  PROTECTED SECTION.
  PRIVATE SECTION.
  class-data GV_NO_DIALOG type ABAP_BOOL .

ENDCLASS.



CLASS ZCL_ABAPGIT_PASSWORD_DIALOG IMPLEMENTATION.

  METHOD UNSET_NO_DIALOG.
    GV_NO_DIALOG = ABAP_FALSE.
  ENDMETHOD.
  
  METHOD SET_NO_DIALOG.
    GV_NO_DIALOG = ABAP_TRUE.
  ENDMETHOD.
  
  METHOD popup.

    IF zcl_abapgit_ui_factory=>get_frontend_services( )->gui_is_available( ) = abap_true AND GV_NO_DIALOG IS INITIAL.
      PERFORM password_popup
        IN PROGRAM (sy-cprog)
        USING iv_repo_url
        CHANGING cv_user cv_pass.
    ELSE.
      "Extract user credentials from the environment...
      "Class ZCL_ABAPGIT_DEFAULT_AUTH_INFO is part of https://github.com/abapGit/ADT_Backend.
      "It stores the credentials of a private repository as long as the session exists.
      "Usually this class should belong to abapGit core and a refactoring is recommended.
      "As a temporary solution - and to avoid a DYNPRO_SEND_IN_BACKGROUND dump - a generic
      "call of the getter methods for username and password is implemented by PR#2635.
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