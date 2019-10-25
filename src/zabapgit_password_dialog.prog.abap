*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PASSWORD_DIALOG
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF SCREEN 1002 TITLE s_title.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) s_url FOR FIELD p_url.
PARAMETERS: p_url TYPE string LOWER CASE VISIBLE LENGTH 40 ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) s_user FOR FIELD p_user.
PARAMETERS: p_user TYPE string LOWER CASE VISIBLE LENGTH 40 ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) s_pass FOR FIELD p_pass.
PARAMETERS: p_pass TYPE string LOWER CASE VISIBLE LENGTH 40 ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF SCREEN 1002.

*-----------------------------------------------------------------------
* LCL_PASSWORD_DIALOG
*-----------------------------------------------------------------------
CLASS lcl_password_dialog DEFINITION FINAL.

**************
* This class will remain local in the report
**************

  PUBLIC SECTION.
    CONSTANTS c_dynnr TYPE char4 VALUE '1002'.

    CLASS-METHODS popup
      IMPORTING
        iv_repo_url TYPE string
      CHANGING
        cv_user     TYPE string
        cv_pass     TYPE string.

    CLASS-METHODS on_screen_init.
    CLASS-METHODS on_screen_output.
    CLASS-METHODS on_screen_event
      IMPORTING
        iv_ucomm TYPE sy-ucomm.

  PRIVATE SECTION.
    CLASS-DATA gv_confirm TYPE abap_bool.
    CLASS-METHODS enrich_title_by_hostname
      IMPORTING
        iv_repo_url TYPE string.

ENDCLASS.

CLASS lcl_password_dialog IMPLEMENTATION.

  METHOD popup.

    CLEAR p_pass.
    p_url      = iv_repo_url.
    p_user     = cv_user.
    gv_confirm = abap_false.


    enrich_title_by_hostname( iv_repo_url ).

    CALL SELECTION-SCREEN c_dynnr STARTING AT 5 5 ENDING AT 60 8.

    IF gv_confirm = abap_true.
      cv_user = p_user.
      cv_pass = p_pass.
    ELSE.
      CLEAR: cv_user, cv_pass.
    ENDIF.

    CLEAR: p_url, p_user, p_pass.

  ENDMETHOD.

  METHOD on_screen_init.
    s_title = 'Login'     ##NO_TEXT.
    s_url   = 'Repo URL'  ##NO_TEXT.
    s_user  = 'User'      ##NO_TEXT.
    s_pass  = 'Password'  ##NO_TEXT.
  ENDMETHOD.

  METHOD on_screen_output.
    DATA lt_ucomm TYPE TABLE OF sy-ucomm.

    ASSERT sy-dynnr = c_dynnr.

    LOOP AT SCREEN.
      IF screen-name = 'P_URL'.
        screen-input       = '0'.
        screen-intensified = '1'.
        screen-display_3d  = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'P_PASS'.
        screen-invisible   = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

    " Program RSSYSTDB, GUI Status %_CSP
    PERFORM set_pf_status IN PROGRAM rsdbrunt IF FOUND.
    APPEND 'NONE' TO lt_ucomm.  "Button Check
    APPEND 'SPOS' TO lt_ucomm.  "Save as Variant

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = sy-pfkey
      TABLES
        p_exclude = lt_ucomm.

    IF p_user IS NOT INITIAL.
      SET CURSOR FIELD 'P_PASS'.
    ENDIF.

  ENDMETHOD.

  METHOD on_screen_event.
    ASSERT sy-dynnr = c_dynnr.

    " CRET   - F8
    " OTHERS - simulate Enter press
    CASE iv_ucomm.
      WHEN 'CRET'.
        gv_confirm = abap_true.
      WHEN OTHERS. "TODO REFACTOR !!! A CLUTCH !
        " This will work unless any new specific logic appear
        " for other commands. The problem is that the password dialog
        " does not have Enter event (or I don't know how to activate it ;)
        " so Enter issues previous command from previous screen
        " But for now this works :) Fortunately Esc produces another flow
        gv_confirm = abap_true.
        LEAVE TO SCREEN 0.
    ENDCASE.

  ENDMETHOD.


  METHOD enrich_title_by_hostname.

    DATA lv_host TYPE string.

    FIND REGEX 'https?://([^/^:]*)' IN iv_repo_url SUBMATCHES lv_host.
    IF lv_host IS NOT INITIAL AND lv_host <> space.
      CLEAR s_title.
      CONCATENATE 'Login:' lv_host INTO s_title IN CHARACTER MODE SEPARATED BY space.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
