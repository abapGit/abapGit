*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PASSWORD_DIALOG
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF SCREEN 1002 TITLE sc_title.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(18) sc_url FOR FIELD p_url.
PARAMETERS: p_url TYPE string LOWER CASE VISIBLE LENGTH 60 ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(18) sc_user FOR FIELD p_user.
PARAMETERS: p_user TYPE string LOWER CASE VISIBLE LENGTH 60 ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(18) sc_pass FOR FIELD p_pass.
PARAMETERS: p_pass TYPE c LENGTH 255 LOWER CASE VISIBLE LENGTH 60 ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(18) sc_cmnt FOR FIELD p_cmnt.
PARAMETERS: p_cmnt TYPE c LENGTH 255 LOWER CASE VISIBLE LENGTH 60 ##SEL_WRONG.
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

    CONSTANTS c_dynnr TYPE c LENGTH 4 VALUE '1002'.

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

    DATA ls_position TYPE zif_abapgit_popups=>ty_popup_position.

    CLEAR p_pass.
    p_url      = iv_repo_url.
    p_user     = cv_user.
    gv_confirm = abap_false.

    p_cmnt = 'Press F1 for Help'.

    enrich_title_by_hostname( iv_repo_url ).

    ls_position = zcl_abapgit_popups=>center(
      iv_width  = 65
      iv_height = 7 ).

    CALL SELECTION-SCREEN c_dynnr
      STARTING AT ls_position-start_column ls_position-start_row
      ENDING AT ls_position-end_column ls_position-end_row.

    IF gv_confirm = abap_true.
      cv_user = p_user.
      cv_pass = p_pass.
    ELSE.
      CLEAR: cv_user, cv_pass.
    ENDIF.

    CLEAR: p_url, p_user, p_pass.

  ENDMETHOD.

  METHOD on_screen_init.
    sc_title = 'Login'.
    sc_url   = 'Repo URL'.
    sc_user  = 'User'.
    sc_pass  = 'Password or Token'.
    sc_cmnt  = 'Note'.
  ENDMETHOD.

  METHOD on_screen_output.

    DATA lt_ucomm TYPE TABLE OF sy-ucomm.

    ASSERT sy-dynnr = c_dynnr.

    LOOP AT SCREEN.
      IF screen-name = 'P_URL' OR screen-name = 'P_CMNT'.
        screen-input       = '0'.
        screen-intensified = '1'.
        screen-display_3d  = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'P_CMNT' OR screen-name = 'SC_CMNT'.
        screen-active    = '1'.
        screen-invisible = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'P_PASS'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

    APPEND 'PICK' TO lt_ucomm.

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = 'DETL'
        p_program = 'RSPFPAR'
      TABLES
        p_exclude = lt_ucomm.

    IF p_user IS NOT INITIAL.
      SET CURSOR FIELD 'P_PASS'.
    ENDIF.

  ENDMETHOD.

  METHOD on_screen_event.

    ASSERT sy-dynnr = c_dynnr.

    CASE iv_ucomm.
      WHEN 'OK'. " Enter
        gv_confirm = abap_true.
        LEAVE TO SCREEN 0.
      WHEN 'HELP'. " F1
        TRY.
            zcl_abapgit_services_abapgit=>open_abapgit_wikipage( 'guide-authentication.html' ).
          CATCH zcx_abapgit_exception ##NO_HANDLER.
        ENDTRY.
      WHEN OTHERS. " Escape
        gv_confirm = abap_false.
        LEAVE TO SCREEN 0.
    ENDCASE.

  ENDMETHOD.


  METHOD enrich_title_by_hostname.

    DATA lv_host TYPE string.

    FIND REGEX 'https?://([^/^:]*)' IN iv_repo_url SUBMATCHES lv_host.
    IF lv_host IS NOT INITIAL AND lv_host <> space.
      CLEAR sc_title.
      CONCATENATE 'Login:' lv_host INTO sc_title IN CHARACTER MODE SEPARATED BY space.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


FORM password_popup
      USING
        pv_repo_url TYPE string
      CHANGING
        cv_user     TYPE string
        cv_pass     TYPE string.

  lcl_password_dialog=>popup(
    EXPORTING
      iv_repo_url     = pv_repo_url
    CHANGING
      cv_user         = cv_user
      cv_pass         = cv_pass ).

ENDFORM.
