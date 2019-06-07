*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_FORMS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  run
*&---------------------------------------------------------------------*
FORM run.

  DATA: lx_exception TYPE REF TO zcx_abapgit_exception,
        lv_ind       TYPE t000-ccnocliind.

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

    zcl_abapgit_services_abapgit=>prepare_gui_startup( ).
    zcl_abapgit_ui_factory=>get_gui( )->go_home( ).
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
  DATA: lt_ucomm TYPE TABLE OF sy-ucomm.

  PERFORM set_pf_status IN PROGRAM rsdbrunt IF FOUND.

  APPEND 'CRET' TO lt_ucomm.  "Button Execute
  APPEND 'SPOS' TO lt_ucomm.  "Button Save

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = lt_ucomm.

ENDFORM.

FORM exit RAISING zcx_abapgit_exception.
  CASE sy-ucomm.
    WHEN 'CBAC'.  "Back
      IF zcl_abapgit_ui_factory=>get_gui( )->back( ) = abap_true. " end of stack
        zcl_abapgit_ui_factory=>get_gui( )->free( ). " Graceful shutdown
      ELSE.
        LEAVE TO SCREEN 1001.
      ENDIF.
    WHEN 'CEND'.
      PERFORM on_exit_pressed.
  ENDCASE.
ENDFORM.

FORM on_exit_pressed RAISING zcx_abapgit_exception.
  IF zcl_abapgit_persist_settings=>get_instance( )->read( )->get_show_default_repo( ) = abap_false.
    " Don't show the last seen repo at startup
    zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( || ).
  ENDIF.
  zcl_abapgit_ui_factory=>get_gui( )->go_home( ).
  LEAVE TO SCREEN 1001.
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

FORM remove_toolbar USING pv_dynnr TYPE char4.

  DATA: ls_header               TYPE rpy_dyhead,
        lt_containers           TYPE dycatt_tab,
        lt_fields_to_containers TYPE dyfatc_tab,
        lt_flow_logic           TYPE swydyflow.

  CALL FUNCTION 'RPY_DYNPRO_READ'
    EXPORTING
      progname             = sy-cprog
      dynnr                = pv_dynnr
    IMPORTING
      header               = ls_header
    TABLES
      containers           = lt_containers
      fields_to_containers = lt_fields_to_containers
      flow_logic           = lt_flow_logic
    EXCEPTIONS
      cancelled            = 1
      not_found            = 2
      permission_error     = 3
      OTHERS               = 4.
  IF sy-subrc IS NOT INITIAL.
    RETURN. " Ignore errors, just exit
  ENDIF.

  IF ls_header-no_toolbar = abap_true.
    RETURN. " No change required
  ENDIF.

  ls_header-no_toolbar = abap_true.

  CALL FUNCTION 'RPY_DYNPRO_INSERT'
    EXPORTING
      header                 = ls_header
      suppress_exist_checks  = abap_true
    TABLES
      containers             = lt_containers
      fields_to_containers   = lt_fields_to_containers
      flow_logic             = lt_flow_logic
    EXCEPTIONS
      cancelled              = 1
      already_exists         = 2
      program_not_exists     = 3
      not_executed           = 4
      missing_required_field = 5
      illegal_field_value    = 6
      field_not_allowed      = 7
      not_generated          = 8
      illegal_field_position = 9
      OTHERS                 = 10.
  IF sy-subrc <> 2 AND sy-subrc <> 0.
    RETURN. " Ignore errors, just exit
  ENDIF.

ENDFORM.
