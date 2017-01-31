*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_FORMS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  run
*&---------------------------------------------------------------------*
FORM run.

  DATA: lx_exception TYPE REF TO lcx_exception,
        lv_ind       TYPE t000-ccnocliind.


  SELECT SINGLE ccnocliind FROM t000 INTO lv_ind
    WHERE mandt = sy-mandt.
  IF sy-subrc = 0
      AND lv_ind <> ' '
      AND lv_ind <> '1'. " check changes allowed
    WRITE: / 'Wrong client, changes to repository objects not allowed'. "#EC NOTEXT
    RETURN.
  ENDIF.

  TRY.
      lcl_migrations=>run( ).
      lcl_app=>run( ).
    CATCH lcx_exception INTO lx_exception.
      MESSAGE lx_exception->mv_text TYPE 'E'.
  ENDTRY.

ENDFORM.                    "run

*&---------------------------------------------------------------------*
*&      Form  branch_popup
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TT_FIELDS      text
*      -->PV_CODE        text
*      -->CS_ERROR       text
*      -->CV_SHOW_POPUP  text
*      -->RAISING        text
*      -->LCX_EXCEPTION  text
*      -->##CALLED       text
*      -->##NEEDED       text
*----------------------------------------------------------------------*
FORM branch_popup TABLES   tt_fields TYPE ty_sval_tt
                  USING    pv_code TYPE clike
                  CHANGING cs_error TYPE svale
                           cv_show_popup TYPE c
                  RAISING lcx_exception ##called ##needed.
* called dynamically from function module POPUP_GET_VALUES_USER_BUTTONS

  DATA: lv_url          TYPE string,
        lx_error        TYPE REF TO lcx_exception,
        ls_package_data TYPE scompkdtln,
        ls_branch       TYPE lcl_git_branch_list=>ty_git_branch,
        lv_create       TYPE boolean.

  FIELD-SYMBOLS: <ls_furl>    LIKE LINE OF tt_fields,
                 <ls_fbranch> LIKE LINE OF tt_fields.


  CLEAR cs_error.

  IF pv_code = 'COD1'.
    cv_show_popup = abap_true.

    READ TABLE tt_fields ASSIGNING <ls_furl> WITH KEY tabname = 'ABAPTXT255'.
    IF sy-subrc <> 0 OR <ls_furl>-value IS INITIAL.
      MESSAGE 'Fill URL' TYPE 'S' DISPLAY LIKE 'E'.         "#EC NOTEXT
      RETURN.
    ENDIF.
    lv_url = <ls_furl>-value.

    TRY.
        ls_branch = lcl_popups=>branch_list_popup( lv_url ).
      CATCH lcx_exception INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
    IF ls_branch IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE tt_fields ASSIGNING <ls_fbranch> WITH KEY tabname = 'TEXTL'.
    ASSERT sy-subrc = 0.
    <ls_fbranch>-value = ls_branch-name.

  ELSEIF pv_code = 'COD2'.
    cv_show_popup = abap_true.

    lcl_popups=>popup_to_create_package( IMPORTING es_package_data = ls_package_data
                                                   ev_create       = lv_create ).
    IF lv_create = abap_false.
      RETURN.
    ENDIF.

    lcl_sap_package=>create( ls_package_data ).
    COMMIT WORK.

    READ TABLE tt_fields ASSIGNING <ls_fbranch> WITH KEY tabname = 'TDEVC'.
    ASSERT sy-subrc = 0.
    <ls_fbranch>-value = ls_package_data-devclass.
  ENDIF.

ENDFORM.                    "branch_popup

FORM package_popup TABLES   tt_fields TYPE ty_sval_tt
                   USING    pv_code TYPE clike
                   CHANGING cs_error TYPE svale
                            cv_show_popup TYPE c
                   RAISING  lcx_exception ##called ##needed.
* called dynamically from function module POPUP_GET_VALUES_USER_BUTTONS

  DATA: ls_package_data TYPE scompkdtln,
        lv_create       TYPE boolean.

  FIELD-SYMBOLS: <ls_fbranch> LIKE LINE OF tt_fields.

  CLEAR cs_error.

  IF pv_code = 'COD1'.
    cv_show_popup = abap_true.

    lcl_popups=>popup_to_create_package( IMPORTING es_package_data = ls_package_data
                                                   ev_create       = lv_create ).
    IF lv_create = abap_false.
      RETURN.
    ENDIF.

    lcl_sap_package=>create( ls_package_data ).
    COMMIT WORK.

    READ TABLE tt_fields ASSIGNING <ls_fbranch> WITH KEY tabname = 'TDEVC'.
    ASSERT sy-subrc = 0.
    <ls_fbranch>-value = ls_package_data-devclass.
  ENDIF.
ENDFORM.                    "package_popup

FORM output.
  DATA: lt_ucomm TYPE TABLE OF sy-ucomm.
  PERFORM set_pf_status IN PROGRAM rsdbrunt IF FOUND.

  APPEND 'CRET' TO lt_ucomm.  "Button Execute

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = lt_ucomm.
ENDFORM.

FORM exit RAISING lcx_exception.
  CASE sy-ucomm.
    WHEN 'CBAC'.  "Back
      IF lcl_app=>gui( )->back( ) IS INITIAL.
        LEAVE TO SCREEN 1001.
      ENDIF.
  ENDCASE.
ENDFORM.
