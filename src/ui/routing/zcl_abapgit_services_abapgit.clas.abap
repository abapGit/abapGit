CLASS zcl_abapgit_services_abapgit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_abapgit_repo TYPE string VALUE 'https://github.com/abapGit/abapGit' ##NO_TEXT.
    CONSTANTS c_abapgit_homepage TYPE string VALUE 'https://www.abapgit.org' ##NO_TEXT.
    CONSTANTS c_abapgit_wikipage TYPE string VALUE 'https://docs.abapgit.org' ##NO_TEXT.
    CONSTANTS c_dotabap_homepage TYPE string VALUE 'https://dotabap.org' ##NO_TEXT.
    CONSTANTS c_abapgit_class TYPE seoclsname VALUE `ZCX_ABAPGIT_EXCEPTION` ##NO_TEXT.
    CONSTANTS c_changelog_path TYPE string VALUE '/blob/main/changelog.txt' ##NO_TEXT.

    CLASS-METHODS open_abapgit_homepage
      IMPORTING
        iv_page TYPE string OPTIONAL
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS open_abapgit_wikipage
      IMPORTING
        iv_page TYPE string OPTIONAL
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS open_dotabap_homepage
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS open_abapgit_changelog
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS is_installed
      RETURNING
        VALUE(rv_devclass) TYPE tadir-devclass .
    CLASS-METHODS prepare_gui_startup
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_abapgit_tcode
      RETURNING
        VALUE(rv_tcode) TYPE tcode .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS set_start_repo_from_package
      IMPORTING
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_package_from_adt
      RETURNING
        VALUE(rv_package) TYPE devclass .
    CLASS-METHODS check_sapgui
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS open_url_in_browser
      IMPORTING
        !iv_url TYPE string
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_services_abapgit IMPLEMENTATION.


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


  METHOD get_abapgit_tcode.
    CONSTANTS: lc_report_tcode_hex TYPE x VALUE '80'.
    DATA: lt_tcodes TYPE STANDARD TABLE OF tcode.

    SELECT tcode
      FROM tstc
      INTO TABLE lt_tcodes
      WHERE pgmna = sy-cprog
        AND cinfo = lc_report_tcode_hex.

    IF lines( lt_tcodes ) > 0.
      READ TABLE lt_tcodes INDEX 1 INTO rv_tcode.
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

        CALL METHOD ('CL_ADT_GUI_INTEGRATION_CONTEXT')=>read_context
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
          CALL METHOD ('CL_ADT_GUI_INTEGRATION_CONTEXT')=>initialize_instance
            EXPORTING
              context_info = <lg_context>.

        ENDIF.

      CATCH cx_root.
        " Some problems with dynamic ADT access.
        " Let's ignore it for now and fail silently
    ENDTRY.

  ENDMETHOD.


  METHOD is_installed.

    SELECT SINGLE devclass FROM tadir INTO rv_devclass
      WHERE pgmid = 'R3TR'
      AND object = 'CLAS'
      AND obj_name = c_abapgit_class.

  ENDMETHOD.


  METHOD open_abapgit_changelog.
    open_url_in_browser( |{ c_abapgit_repo }{ c_changelog_path }| ).
  ENDMETHOD.


  METHOD open_abapgit_homepage.
    open_url_in_browser( |{ c_abapgit_homepage }/{ iv_page }| ).
  ENDMETHOD.


  METHOD open_abapgit_wikipage.
    open_url_in_browser( |{ c_abapgit_wikipage }/{ iv_page }| ).
  ENDMETHOD.


  METHOD open_dotabap_homepage.
    open_url_in_browser( c_dotabap_homepage ).
  ENDMETHOD.


  METHOD open_url_in_browser.
    DATA lx_error TYPE REF TO zcx_abapgit_exception.

    TRY.
        zcl_abapgit_ui_factory=>get_frontend_services( )->execute( iv_document = iv_url ).
      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abapgit_exception=>raise( iv_text     = 'Opening page in external browser failed.'
                                      ix_previous = lx_error ).
    ENDTRY.
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
ENDCLASS.
