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

    PERFORM password_popup
      IN PROGRAM (sy-cprog)
      USING iv_repo_url
      CHANGING cv_user cv_pass.

  ENDMETHOD.
ENDCLASS.
