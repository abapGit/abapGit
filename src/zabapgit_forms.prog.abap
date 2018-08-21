*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_FORMS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  run
*&---------------------------------------------------------------------*
FORM run.

  DATA: lx_exception TYPE REF TO zcx_abapgit_exception,
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
      zcl_abapgit_migrations=>run( ).
      PERFORM open_gui.
    CATCH zcx_abapgit_exception INTO lx_exception.
      MESSAGE lx_exception TYPE 'E'.
  ENDTRY.

ENDFORM.                    "run

FORM open_gui RAISING zcx_abapgit_exception.

  IF sy-batch = abap_true.
    zcl_abapgit_background=>run( ).
  ELSE.

    IF zcl_abapgit_persist_settings=>get_instance( )->read( )->get_show_default_repo( ) = abap_false.
      " Don't show the last seen repo at startup
      zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( || ).
    ENDIF.

    zcl_abapgit_gui=>get_instance( )->go_home( ).
    CALL SELECTION-SCREEN 1001. " trigger screen
  ENDIF.

ENDFORM.

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
*      -->zcx_abapgit_exception  text
*      -->##CALLED       text
*      -->##NEEDED       text
*----------------------------------------------------------------------*
FORM branch_popup TABLES   tt_fields TYPE zif_abapgit_definitions=>ty_sval_tt
                  USING    pv_code TYPE clike
                  CHANGING cs_error TYPE svale
                           cv_show_popup TYPE c
                  RAISING zcx_abapgit_exception ##called ##needed.
* called dynamically from function module POPUP_GET_VALUES_USER_BUTTONS

  DATA: lx_error  TYPE REF TO zcx_abapgit_exception,
        li_popups TYPE REF TO zif_abapgit_popups.

  TRY.
      li_popups = zcl_abapgit_ui_factory=>get_popups( ).
      li_popups->branch_popup_callback(
        EXPORTING
          iv_code       = pv_code
        CHANGING
          ct_fields     = tt_fields[]
          cs_error      = cs_error
          cv_show_popup = cv_show_popup ).

    CATCH zcx_abapgit_exception INTO lx_error.
      MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.                    "branch_popup

FORM package_popup TABLES   tt_fields TYPE zif_abapgit_definitions=>ty_sval_tt
                   USING    pv_code TYPE clike
                   CHANGING cs_error TYPE svale
                            cv_show_popup TYPE c
                   RAISING  zcx_abapgit_exception ##called ##needed.
* called dynamically from function module POPUP_GET_VALUES_USER_BUTTONS

  DATA: lx_error  TYPE REF TO zcx_abapgit_exception,
        li_popups TYPE REF TO zif_abapgit_popups.

  TRY.
      li_popups = zcl_abapgit_ui_factory=>get_popups( ).
      li_popups->package_popup_callback(
        EXPORTING
          iv_code       = pv_code
        CHANGING
          ct_fields     = tt_fields[]
          cs_error      = cs_error
          cv_show_popup = cv_show_popup ).

    CATCH zcx_abapgit_exception INTO lx_error.
      MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.                    "package_popup

FORM output.
  DATA: lt_ucomm TYPE TABLE OF sy-ucomm,
        lx_error TYPE REF TO zcx_abapgit_exception.

  PERFORM set_pf_status IN PROGRAM rsdbrunt IF FOUND.

  APPEND 'CRET' TO lt_ucomm.  "Button Execute

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = lt_ucomm.

ENDFORM.

FORM exit RAISING zcx_abapgit_exception.
  CASE sy-ucomm.
    WHEN 'CBAC'.  "Back
      IF zcl_abapgit_gui=>get_instance( )->back( ) IS INITIAL.
        LEAVE TO SCREEN 1001.
      ENDIF.
  ENDCASE.
ENDFORM.

FORM password_popup
      USING
        iv_repo_url TYPE string
      CHANGING
        cv_user     TYPE string
        cv_pass     TYPE string.

  lcl_password_dialog=>popup(
    EXPORTING
      iv_repo_url     = iv_repo_url
    CHANGING
      cv_user         = cv_user
      cv_pass         = cv_pass ).

ENDFORM.
