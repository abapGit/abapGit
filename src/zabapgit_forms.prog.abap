*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_FORMS
*&---------------------------------------------------------------------*

CLASS lcl_startup DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS prepare_gui_startup
      RAISING
        zcx_abapgit_exception.
  PRIVATE SECTION.
    CLASS-METHODS set_start_repo_from_package
      IMPORTING
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS get_package_from_adt
      RETURNING
        VALUE(rv_package) TYPE devclass.

    CLASS-METHODS check_sapgui
      RAISING
        zcx_abapgit_exception.
ENDCLASS.

CLASS lcl_startup IMPLEMENTATION.

  METHOD check_sapgui.

    CONSTANTS:
      lc_hide_sapgui_hint TYPE string VALUE '2'.

    DATA:
      lv_answer           TYPE char1,
      ls_settings         TYPE zif_abapgit_definitions=>ty_s_user_settings,
      li_user_persistence TYPE REF TO zif_abapgit_persist_user.

    li_user_persistence = zcl_abapgit_persistence_user=>get_instance( ).

    ls_settings = li_user_persistence->get_settings( ).

    IF ls_settings-hide_sapgui_hint = abap_true.
      RETURN.
    ENDIF.

    IF zcl_abapgit_ui_factory=>get_frontend_services( )->is_sapgui_for_java( ) = abap_false.
      RETURN.
    ENDIF.

    lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
                    iv_titlebar              = 'Not supported SAPGUI'
                    iv_text_question         = 'SAPGUI for Java is not supported! There might be some issues.'
                    iv_text_button_1         = 'Got it'
                    iv_icon_button_1         = |{ icon_okay }|
                    iv_text_button_2         = 'Hide'
                    iv_icon_button_2         = |{ icon_set_state }|
                    iv_display_cancel_button = abap_false ).

    IF lv_answer = lc_hide_sapgui_hint.
      ls_settings-hide_sapgui_hint = abap_true.
      li_user_persistence->set_settings( ls_settings ).
    ENDIF.

  ENDMETHOD.

  METHOD prepare_gui_startup.
    DATA: lv_repo_key    TYPE zif_abapgit_persistence=>ty_value,
          lv_package     TYPE devclass,
          lv_package_adt TYPE devclass.

    check_sapgui( ).

    IF zcl_abapgit_persist_factory=>get_settings( )->read( )->get_show_default_repo( ) = abap_false.
      " Don't show the last seen repo at startup
      zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( || ).
    ENDIF.

    " We have three special cases for gui startup
    "   - open a specific repo by repo key
    "   - open a specific repo by package name
    "   - open a specific repo by package name provided by ADT
    " These overrule the last shown repo

    GET PARAMETER ID zif_abapgit_definitions=>c_spagpa_param_repo_key FIELD lv_repo_key.
    GET PARAMETER ID zif_abapgit_definitions=>c_spagpa_param_package  FIELD lv_package.
    lv_package_adt = get_package_from_adt( ).

    IF lv_repo_key IS NOT INITIAL.

      SET PARAMETER ID zif_abapgit_definitions=>c_spagpa_param_repo_key FIELD ''.
      zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( lv_repo_key ).

    ELSEIF lv_package IS NOT INITIAL.

      SET PARAMETER ID zif_abapgit_definitions=>c_spagpa_param_package FIELD ''.
      set_start_repo_from_package( lv_package ).

    ELSEIF lv_package_adt IS NOT INITIAL.

      set_start_repo_from_package( lv_package_adt ).

    ENDIF.
  ENDMETHOD.

  METHOD set_start_repo_from_package.
    DATA: lo_repo          TYPE REF TO zcl_abapgit_repo,
          lt_r_package     TYPE RANGE OF devclass,
          ls_r_package     LIKE LINE OF lt_r_package,
          lt_superpackages TYPE zif_abapgit_sap_package=>ty_devclass_tt,
          li_package       TYPE REF TO zif_abapgit_sap_package,
          lt_repo_list     TYPE zif_abapgit_repo_srv=>ty_repo_list.

    FIELD-SYMBOLS: <lo_repo>         TYPE LINE OF zif_abapgit_repo_srv=>ty_repo_list,
                   <lv_superpackage> LIKE LINE OF lt_superpackages.

    li_package = zcl_abapgit_factory=>get_sap_package( iv_package ).

    IF li_package->exists( ) = abap_false.
      RETURN.
    ENDIF.

    ls_r_package-sign   = 'I'.
    ls_r_package-option = 'EQ'.
    ls_r_package-low    = iv_package.
    INSERT ls_r_package INTO TABLE lt_r_package.

    " Also consider superpackages. E.g. when some open $abapgit_ui, abapGit repo
    " should be found via package $abapgit
    lt_superpackages = li_package->list_superpackages( ).
    LOOP AT lt_superpackages ASSIGNING <lv_superpackage>.
      ls_r_package-low = <lv_superpackage>.
      INSERT ls_r_package INTO TABLE lt_r_package.
    ENDLOOP.

    lt_repo_list = zcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_repo_list ASSIGNING <lo_repo>.

      IF <lo_repo>->get_package( ) IN lt_r_package.
        lo_repo ?= <lo_repo>.
        EXIT.
      ENDIF.

    ENDLOOP.

    IF lo_repo IS BOUND.
      zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( lo_repo->get_key( ) ).
    ENDIF.
  ENDMETHOD.

  METHOD get_package_from_adt.

    DATA: ls_item    TYPE zif_abapgit_definitions=>ty_item,
          lr_context TYPE REF TO data,
          lt_fields  TYPE tihttpnvp.

    FIELD-SYMBOLS: <lg_context>    TYPE any,
                   <lv_parameters> TYPE string,
                   <ls_field>      LIKE LINE OF lt_fields.


    ls_item-obj_type = 'CLAS'.
    ls_item-obj_name = 'CL_ADT_GUI_INTEGRATION_CONTEXT'.

    IF zcl_abapgit_objects=>exists( ls_item ) = abap_false.
      " ADT is not supported in this NW release
      RETURN.
    ENDIF.

    TRY.
        CREATE DATA lr_context TYPE ('CL_ADT_GUI_INTEGRATION_CONTEXT=>TY_CONTEXT_INFO').

        ASSIGN lr_context->* TO <lg_context>.
        ASSERT sy-subrc = 0.

        CALL METHOD (ls_item-obj_name)=>read_context
          RECEIVING
            result = <lg_context>.

        ASSIGN COMPONENT 'PARAMETERS'
               OF STRUCTURE <lg_context>
               TO <lv_parameters>.
        ASSERT sy-subrc = 0.

        lt_fields = cl_http_utility=>string_to_fields( cl_http_utility=>unescape_url( <lv_parameters> ) ).

        READ TABLE lt_fields ASSIGNING <ls_field>
                             WITH KEY name = 'p_package_name'.
        IF sy-subrc = 0.
          rv_package = <ls_field>-value.

          " We want to open the repo just once. Therefore we delete the parameters
          " and initialize the ADT context.
          CLEAR <lv_parameters>.
          CALL METHOD (ls_item-obj_name)=>initialize_instance
            EXPORTING
              context_info = <lg_context>.

        ENDIF.

      CATCH cx_root.
        " Some problems with dynamic ADT access.
        " Let's ignore it for now and fail silently
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

*******************************************************

FORM run.

  DATA lx_exception TYPE REF TO zcx_abapgit_exception.
  DATA lx_not_found TYPE REF TO zcx_abapgit_not_found.

  TRY.
      IF zcl_abapgit_auth=>is_allowed( zif_abapgit_auth=>c_authorization-startup ) = abap_false.
        zcx_abapgit_exception=>raise( 'No authorization to start abapGit' ).
      ENDIF.

      zcl_abapgit_migrations=>run( ).
      PERFORM open_gui.
    CATCH zcx_abapgit_exception INTO lx_exception.
      MESSAGE lx_exception TYPE 'E'.
    CATCH zcx_abapgit_not_found INTO lx_not_found.
      MESSAGE lx_not_found TYPE 'E'.
  ENDTRY.

ENDFORM.                    "run

FORM open_gui RAISING zcx_abapgit_exception.

  DATA lv_action TYPE string.
  DATA lv_mode   TYPE tabname.

  IF sy-batch = abap_true.
    zcl_abapgit_background=>run( ).
  ELSE.

* https://docs.abapgit.org/user-guide/reference/database-util.html#emergency-mode
    GET PARAMETER ID 'DBT' FIELD lv_mode.
    CASE lv_mode.
      WHEN 'ZABAPGIT'.
        lv_action = zif_abapgit_definitions=>c_action-go_db.
      WHEN OTHERS.
        lv_action = zif_abapgit_definitions=>c_action-go_home.
    ENDCASE.

    lcl_startup=>prepare_gui_startup( ).
    zcl_abapgit_ui_factory=>get_gui( )->go_home( lv_action ).
    CALL SELECTION-SCREEN 1001. " trigger screen

  ENDIF.

ENDFORM.

FORM output.

  DATA: lx_error TYPE REF TO zcx_abapgit_exception,
        lt_ucomm TYPE TABLE OF sy-ucomm.

  PERFORM set_pf_status IN PROGRAM rsdbrunt IF FOUND.

  APPEND 'CRET' TO lt_ucomm.  "Button Execute
  APPEND 'SPOS' TO lt_ucomm.  "Button Save

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = lt_ucomm.

  " For variant maintenance we have to omit this because
  " it instantiates controls and hides maintenance screens.
  IF zcl_abapgit_factory=>get_environment( )->is_variant_maintenance( ) = abap_false.
    TRY.
        zcl_abapgit_ui_factory=>get_gui( )->set_focus( ).
      CATCH zcx_abapgit_exception INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDIF.

ENDFORM.

FORM exit.

  DATA lx_error TYPE REF TO zcx_abapgit_exception.

  " The exit logic should only be applied for our 'main' selection screen 1001.
  " All other selection-screens are called as popups and shouldn't influence
  " the gui navigation as it would lead to inpredictable behaviour like dumps.
  IF sy-dynnr <> 1001.
    RETURN.
  ENDIF.

  TRY.
      CASE sy-ucomm.
        WHEN 'CBAC' OR 'CCAN'.  "Back & Escape
          IF zcl_abapgit_ui_factory=>get_gui( )->back( iv_graceful = abap_true ) = abap_true. " end of stack
            zcl_abapgit_ui_factory=>get_gui( )->free( ). " Graceful shutdown
          ELSE.
            LEAVE TO SCREEN 1001.
          ENDIF.
      ENDCASE.
    CATCH zcx_abapgit_exception INTO lx_error.
      MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.

FORM adjust_toolbar USING pv_dynnr TYPE sy-dynnr.

  DATA: ls_header               TYPE rpy_dyhead,
        lt_containers           TYPE dycatt_tab,
        lt_fields_to_containers TYPE dyfatc_tab,
        lt_flow_logic           TYPE swydyflow,
        lv_no_toolbar           LIKE ls_header-no_toolbar.

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

  " Remove toolbar on html screen but re-insert toolbar for variant maintenance.
  " Because otherwise important buttons are missing and variant maintenance is not possible.
  lv_no_toolbar = boolc( zcl_abapgit_factory=>get_environment(
                                           )->is_variant_maintenance( ) = abap_false ).

  IF ls_header-no_toolbar = lv_no_toolbar.
    RETURN. " No change required
  ENDIF.

  ls_header-no_toolbar = lv_no_toolbar.

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
