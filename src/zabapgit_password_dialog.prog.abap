*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PASSWORD_DIALOG
*&---------------------------------------------------------------------*
TABLES sscrfields.

SELECTION-SCREEN BEGIN OF SCREEN 1002 TITLE s_title.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) s_url FOR FIELD p_url.
PARAMETERS: p_url  TYPE string LOWER CASE VISIBLE LENGTH 40 ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) s_user FOR FIELD p_user.
PARAMETERS: p_user TYPE string LOWER CASE VISIBLE LENGTH 40 ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) s_pass FOR FIELD p_pass.
PARAMETERS: p_pass TYPE string LOWER CASE VISIBLE LENGTH 40 ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_en2fa TYPE abap_bool DEFAULT abap_false USER-COMMAND u1 MODIF ID m1 AS CHECKBOX.
SELECTION-SCREEN COMMENT 4(6) s_2fat FOR FIELD p_2fat MODIF ID m1.
SELECTION-SCREEN POSITION 12.
PARAMETERS: p_2fat TYPE string LOWER CASE VISIBLE LENGTH 40 MODIF ID m1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN END OF SCREEN 1002.

*-----------------------------------------------------------------------
* LCL_PASSWORD_DIALOG
*-----------------------------------------------------------------------
CLASS lcl_password_dialog DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES:
      gty_mode TYPE i.
    CONSTANTS:
      dynnr TYPE char4 VALUE '1002',
      BEGIN OF gc_modes,
        user_pass        TYPE gty_mode VALUE 1,
        user_pass_2fa    TYPE gty_mode VALUE 2,
        unlock_2fa_token TYPE gty_mode VALUE 3,
      END OF gc_modes.

    CLASS-METHODS popup
      IMPORTING
        iv_repo_url     TYPE string
        iv_mode         TYPE gty_mode DEFAULT gc_modes-user_pass
      EXPORTING
        ev_delete_token TYPE abap_bool
      CHANGING
        cv_user         TYPE string
        cv_pass         TYPE string
        cv_2fa_token    TYPE string.

    CLASS-METHODS on_screen_init.
    CLASS-METHODS on_screen_output.
    CLASS-METHODS on_screen_event
      IMPORTING
        iv_ucomm TYPE syucomm.

  PRIVATE SECTION.
    CLASS-DATA:
      mv_confirm      TYPE abap_bool,
      gv_mode         TYPE gty_mode,
      gv_delete_token TYPE abap_bool.

ENDCLASS. "lcl_password_dialog DEFINITION

CLASS lcl_password_dialog IMPLEMENTATION.

  METHOD popup.

    CLEAR p_pass.
    p_url      = iv_repo_url.
    p_user     = cv_user.
    mv_confirm = abap_false.
    gv_mode    = iv_mode.
    gv_delete_token = abap_false.

    CALL SELECTION-SCREEN dynnr STARTING AT 5 5 ENDING AT 60 8.

    IF mv_confirm = abap_true.
      cv_user = p_user.
      cv_pass = p_pass.
      cv_2fa_token = p_2fat.
    ELSE.
      CLEAR: cv_user, cv_pass, cv_2fa_token.
    ENDIF.

    ev_delete_token = gv_delete_token.

    CLEAR: p_url, p_user, p_pass, p_2fat.

  ENDMETHOD.  "popup

  METHOD on_screen_init.
    s_title = 'Login'     ##NO_TEXT.
    s_url   = 'Repo URL'  ##NO_TEXT.
    s_user  = 'User'      ##NO_TEXT.
    s_pass  = 'Password'  ##NO_TEXT.
    s_2fat  = '2FA'       ##NO_TEXT.
  ENDMETHOD.  "on_screen_init

  METHOD on_screen_output.
    DATA lt_ucomm TYPE TABLE OF sy-ucomm.

    ASSERT sy-dynnr = dynnr.

    CLEAR p_2fat.

    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'P_URL'.
          screen-input       = '0'.
          screen-intensified = '1'.
          screen-display_3d  = '0'.
          MODIFY SCREEN.

        WHEN 'P_PASS'.
          screen-invisible   = '1'.
          MODIFY SCREEN.

        WHEN 'P_USER'.
          IF gv_mode = gc_modes-unlock_2fa_token.
            screen-input = 0.
            MODIFY SCREEN.
          ENDIF.

        WHEN 'P_2FAT'.
          IF p_en2fa = abap_true.
            screen-input = '1'.
          ELSE.
            screen-input = '0'.
            p_2fat = 'Two factor authentication token' ##NO_TEXT.
          ENDIF.
          MODIFY SCREEN.

      ENDCASE.

      IF screen-group1 = 'M1' AND gv_mode <> gc_modes-user_pass_2fa.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

    IF gv_mode = gc_modes-unlock_2fa_token.
      s_title = 'Unlock two factor authentication token' ##NO_TEXT.
      sscrfields-functxt_01 = 'Remove token' ##NO_TEXT.
    ELSE.
      s_title = 'Login'.
      CLEAR sscrfields-functxt_01.
    ENDIF.

    " Program RSSYSTDB, GUI Status %_CSP
    PERFORM set_pf_status IN PROGRAM rsdbrunt IF FOUND.
    APPEND 'NONE' TO lt_ucomm.  "Button Check
    APPEND 'SPOS' TO lt_ucomm.  "Save as Variant

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = sy-pfkey
      TABLES
        p_exclude = lt_ucomm.

    IF p_user IS NOT INITIAL AND p_pass IS NOT INITIAL AND p_en2fa = abap_true.
      SET CURSOR FIELD 'P_2FAT'.
    ELSEIF p_user IS NOT INITIAL.
      SET CURSOR FIELD 'P_PASS'.
    ENDIF.

  ENDMETHOD.  "on_screen_output

  METHOD on_screen_event.
    DATA: lv_answer TYPE c.
    ASSERT sy-dynnr = dynnr.

    " CRET   - F8
    " OTHERS - simulate Enter press
    CASE iv_ucomm.
      WHEN 'FC01'. " Delete two factor code
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question = 'Do you really want to remove this two factor access token?'
          IMPORTING
            answer        = lv_answer.
        IF lv_answer = '1'.
          gv_delete_token = abap_true.
          LEAVE TO SCREEN 0.
        ENDIF.
      WHEN 'CRET'.
        mv_confirm = abap_true.
        LEAVE TO SCREEN 0.
      WHEN OTHERS. "TODO REFACTOR !!! A CLUTCH !
        " This will work unless any new specific logic appear
        " for other commands. The problem is that the password dialog
        " does not have Enter event (or I don't know how to activate it ;)
        " so Enter issues previous command from previous screen
        " But for now this works :) Fortunately Esc produces another flow
*        mv_confirm = abap_true.
*        LEAVE TO SCREEN 0.
    ENDCASE.

  ENDMETHOD.  "on_screen_event

ENDCLASS. " lcl_password_dialog IMPLEMENTATION
