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
        !cv_pass     TYPE string
      RAISING
        zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS call_frontend_auth_helper
      IMPORTING
        iv_repo_url TYPE string
      CHANGING
        cv_username TYPE string
        cv_password TYPE string
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_password_dialog IMPLEMENTATION.


  METHOD popup.

    DATA lv_gui_is_available TYPE abap_bool.

    lv_gui_is_available = zcl_abapgit_ui_factory=>get_gui_functions( )->gui_is_available( ).

    IF lv_gui_is_available = abap_true.
      IF zcl_abapgit_persistence_user=>get_instance( )->get_settings( )-use_frontend_git_auth = abap_false.
        PERFORM password_popup
          IN PROGRAM (sy-cprog)
          USING iv_repo_url
          CHANGING cv_user cv_pass.
      ELSE.
        call_frontend_auth_helper(
          EXPORTING
            iv_repo_url = iv_repo_url
          CHANGING
            cv_username = cv_user
            cv_password = cv_pass ).
      ENDIF.
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

  METHOD call_frontend_auth_helper.
    DATA: ls_control       TYPE ole2_object,
          lv_result        TYPE string,
          lv_function_call TYPE string,
          lt_code          TYPE STANDARD TABLE OF string,
          lv_vbscript      TYPE string,
          lt_lines         TYPE STANDARD TABLE OF string,
          lv_key           TYPE string,
          lv_value         TYPE string.
    FIELD-SYMBOLS: <lv_line> TYPE string.

    CREATE OBJECT ls_control 'MSScriptControl.ScriptControl'.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Could not create ScriptControl for external authentication' ).
    ENDIF.

    SET PROPERTY OF ls_control 'AllowUI' = 1.
    SET PROPERTY OF ls_control 'Language' = 'VBScript'.

    APPEND `Option Explicit` TO lt_code.
    APPEND `Function FillCredentials(User, Url)` TO lt_code.
    APPEND `  Dim shell` TO lt_code.
    APPEND `  Dim params` TO lt_code.
    APPEND `  Dim command` TO lt_code.
    APPEND `  Dim exec` TO lt_code.
    APPEND `  Set shell = CreateObject("Wscript.Shell")` TO lt_code.
    APPEND `  params = "url=" & Url & vbCrLf & "username=" & User & vbCrLf` TO lt_code.
    APPEND `  command = "powershell.exe -Command ""echo '" & params & "' | git.exe credential fill"""` TO lt_code.
    APPEND `  Set exec = shell.exec(command)` TO lt_code.
    APPEND `  Do While True` TO lt_code.
    APPEND `    Select Case exec.Status` TO lt_code.
    APPEND `      Case 1` TO lt_code.
    APPEND `        FillCredentials = exec.StdOut.ReadAll` TO lt_code.
    APPEND `        Exit Do` TO lt_code.
    APPEND `      Case 2` TO lt_code.
    APPEND `        Exit Do` TO lt_code.
    APPEND `      Case 0` TO lt_code.
    APPEND `    End Select` TO lt_code.
    APPEND `  Loop` TO lt_code.
    APPEND `End Function` TO lt_code.

    CONCATENATE LINES OF lt_code INTO lv_vbscript SEPARATED BY cl_abap_char_utilities=>cr_lf.

    CALL METHOD OF ls_control 'AddCode'
      EXPORTING
        #1 = lv_vbscript.

    IF sy-subrc <> 0.
      FREE OBJECT ls_control.
      zcx_abapgit_exception=>raise( 'Error compiling VBScript for external authentication' ).
    ENDIF.

    lv_function_call = |FillCredentials("{ cv_username }", "{ iv_repo_url }")|.

    CALL METHOD OF ls_control 'Eval' = lv_result
      EXPORTING
        #1 = lv_function_call.

    IF sy-subrc = 0.
      SPLIT lv_result AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.
      LOOP AT lt_lines ASSIGNING <lv_line>.
        SPLIT <lv_line> AT '=' INTO lv_key lv_value.
        IF sy-subrc = 0.
          CASE lv_key.
            WHEN 'username'.
              cv_username = lv_value.
            WHEN 'password'.
              cv_password = lv_value.
          ENDCASE.
        ENDIF.
      ENDLOOP.
    ELSE.
      FREE OBJECT ls_control.
      zcx_abapgit_exception=>raise( 'Error calling FillCredentials in VBScript' ).
    ENDIF.

    IF cv_username IS INITIAL OR cv_password IS INITIAL.
      FREE OBJECT ls_control.
      zcx_abapgit_exception=>raise( 'No login details acquired using external authentication' ).
    ENDIF.

    FREE OBJECT ls_control.
  ENDMETHOD.
ENDCLASS.
