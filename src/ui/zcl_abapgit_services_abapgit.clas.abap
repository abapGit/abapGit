CLASS zcl_abapgit_services_abapgit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS: c_abapgit_repo     TYPE string   VALUE 'https://github.com/larshp/abapGit'     ##NO_TEXT,
               c_abapgit_homepage TYPE string   VALUE 'http://www.abapgit.org'                ##NO_TEXT,
               c_abapgit_wikipage TYPE string   VALUE 'http://docs.abapgit.org'               ##NO_TEXT,
               c_abapgit_package  TYPE devclass VALUE '$ABAPGIT'                              ##NO_TEXT,
               c_abapgit_url      TYPE string   VALUE 'https://github.com/larshp/abapGit.git' ##NO_TEXT,
               c_abapgit_tcode    TYPE tcode    VALUE `ZABAPGIT`                              ##NO_TEXT.

    CLASS-METHODS open_abapgit_homepage
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS open_abapgit_wikipage
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS open_abapgit_changelog
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS install_abapgit
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS is_installed
      RETURNING
        VALUE(rv_devclass) TYPE tadir-devclass .
    CLASS-METHODS prepare_gui_startup
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS do_install
      IMPORTING iv_title   TYPE c
                iv_text    TYPE c
                iv_url     TYPE string
                iv_package TYPE devclass
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS set_start_repo_from_package
      IMPORTING
        iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS get_package_from_adt
      RETURNING
        VALUE(rv_package) TYPE devclass.
    CLASS-METHODS check_sapgui
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_services_abapgit IMPLEMENTATION.


  METHOD check_sapgui.

    CONSTANTS:
      lc_hide_sapgui_hint TYPE string VALUE '2' ##NO_TEXT.

    DATA:
      lv_answer           TYPE char1,
      ls_settings         TYPE zif_abapgit_definitions=>ty_s_user_settings,
      lo_user_persistence TYPE REF TO zif_abapgit_persist_user.

    lo_user_persistence = zcl_abapgit_persistence_user=>get_instance( ).

    ls_settings = lo_user_persistence->get_settings( ).

    IF ls_settings-hide_sapgui_hint = abap_true.
      RETURN.
    ENDIF.

    IF zcl_abapgit_ui_factory=>get_gui_functions( )->is_sapgui_for_java( ) = abap_false.
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
      lo_user_persistence->set_settings( ls_settings ).
    ENDIF.

  ENDMETHOD.


  METHOD do_install.

    DATA: lo_repo   TYPE REF TO zcl_abapgit_repo_online,
          lv_answer TYPE c LENGTH 1.


    lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = iv_title
      iv_text_question         = iv_text
      iv_text_button_1         = 'Continue'
      iv_text_button_2         = 'Cancel'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false ).              "#EC NOTEXT

    IF lv_answer <> '1'.
      RETURN.
    ENDIF.

    IF abap_false = zcl_abapgit_repo_srv=>get_instance( )->is_repo_installed(
        iv_url              = iv_url
        iv_target_package   = iv_package ).

      zcl_abapgit_factory=>get_sap_package( iv_package )->create_local( ).

      lo_repo = zcl_abapgit_repo_srv=>get_instance( )->new_online(
        iv_url         = iv_url
        iv_branch_name = 'refs/heads/master'
        iv_package     = iv_package ) ##NO_TEXT.

      zcl_abapgit_services_repo=>gui_deserialize( lo_repo ).

      zcl_abapgit_services_repo=>toggle_favorite( lo_repo->get_key( ) ).
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD get_package_from_adt.

    DATA: ls_item    TYPE zif_abapgit_definitions=>ty_item,
          lr_context TYPE REF TO data,
          lt_fields  TYPE tihttpnvp.


    FIELD-SYMBOLS: <ls_context>    TYPE any,
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

        ASSIGN lr_context->* TO <ls_context>.
        ASSERT sy-subrc = 0.

        CALL METHOD ('CL_ADT_GUI_INTEGRATION_CONTEXT')=>read_context
          RECEIVING
            result = <ls_context>.

        ASSIGN COMPONENT 'PARAMETERS'
               OF STRUCTURE <ls_context>
               TO <lv_parameters>.
        ASSERT sy-subrc = 0.

        lt_fields = cl_http_utility=>string_to_fields(
                        cl_http_utility=>unescape_url(
                            <lv_parameters> ) ).

        READ TABLE lt_fields ASSIGNING <ls_field>
                             WITH KEY name = 'p_package_name'.
        IF sy-subrc = 0.
          rv_package = <ls_field>-value.

          " We want to open the repo just once. Therefore we delete the parameters
          " and initialize the ADT context.
          CLEAR <lv_parameters>.
          CALL METHOD ('CL_ADT_GUI_INTEGRATION_CONTEXT')=>initialize_instance
            EXPORTING
              context_info = <ls_context>.

        ENDIF.

      CATCH cx_root.
        " Some problems with dynamic ADT access.
        " Let's ignore it for now and fail silently
    ENDTRY.

  ENDMETHOD.


  METHOD install_abapgit.

    CONSTANTS lc_title TYPE c LENGTH 40 VALUE 'Install abapGit'.
    DATA lv_text       TYPE c LENGTH 100.

    IF NOT is_installed( ) IS INITIAL.
      lv_text = 'Seems like abapGit package is already installed. No changes to be done'.
      zcl_abapgit_ui_factory=>get_popups( )->popup_to_inform(
        iv_titlebar     = lc_title
        iv_text_message = lv_text ).
      RETURN.
    ENDIF.

    lv_text = |Confirm to install current version of abapGit to package { c_abapgit_package }|.

    do_install( iv_title   = lc_title
                iv_text    = lv_text
                iv_url     = c_abapgit_url
                iv_package = c_abapgit_package ).

  ENDMETHOD.


  METHOD is_installed.

    SELECT SINGLE devclass FROM tadir INTO rv_devclass
      WHERE object = 'TRAN' AND obj_name = c_abapgit_tcode.

  ENDMETHOD.


  METHOD open_abapgit_changelog.

    cl_gui_frontend_services=>execute(
      EXPORTING document = c_abapgit_repo && '/blob/master/changelog.txt'
      EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Opening page in external browser failed.' ).
    ENDIF.

  ENDMETHOD.


  METHOD open_abapgit_homepage.

    cl_gui_frontend_services=>execute(
      EXPORTING document = c_abapgit_homepage
      EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Opening page in external browser failed.' ).
    ENDIF.

  ENDMETHOD.


  METHOD open_abapgit_wikipage.

    cl_gui_frontend_services=>execute(
      EXPORTING document = c_abapgit_wikipage
      EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Opening page in external browser failed.' ).
    ENDIF.

  ENDMETHOD.


  METHOD prepare_gui_startup.

    DATA: lv_repo_key    TYPE zif_abapgit_persistence=>ty_value,
          lv_package     TYPE devclass,
          lv_package_adt TYPE devclass.

    check_sapgui( ).

    IF zcl_abapgit_persist_settings=>get_instance( )->read( )->get_show_default_repo( ) = abap_false.
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
          lo_package       TYPE REF TO zif_abapgit_sap_package,
          lt_repo_list     TYPE zif_abapgit_definitions=>ty_repo_ref_tt.

    FIELD-SYMBOLS: <lo_repo>         TYPE LINE OF zif_abapgit_definitions=>ty_repo_ref_tt,
                   <lv_superpackage> LIKE LINE OF lt_superpackages.

    lo_package = zcl_abapgit_factory=>get_sap_package( iv_package ).

    IF lo_package->exists( ) = abap_false.
      RETURN.
    ENDIF.

    ls_r_package-sign   = 'I'.
    ls_r_package-option = 'EQ'.
    ls_r_package-low    = iv_package.
    INSERT ls_r_package INTO TABLE lt_r_package.

    " Also consider superpackages. E.g. when some open $abapgit_ui, abapGit repo
    " should be found via package $abapgit
    lt_superpackages = lo_package->list_superpackages( ).
    LOOP AT lt_superpackages ASSIGNING <lv_superpackage>.
      ls_r_package-low = <lv_superpackage>.
      INSERT ls_r_package INTO TABLE lt_r_package.
    ENDLOOP.

    lt_repo_list = zcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_repo_list ASSIGNING <lo_repo>.

      IF <lo_repo>->get_package( ) IN lt_r_package.
        lo_repo = <lo_repo>.
        EXIT.
      ENDIF.

    ENDLOOP.

    IF lo_repo IS BOUND.
      zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( lo_repo->get_key( ) ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
