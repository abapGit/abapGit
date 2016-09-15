*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_SERVICES_ABAPGIT
*&---------------------------------------------------------------------*

CLASS lcl_services_abapgit DEFINITION FINAL.
  PUBLIC SECTION.

    CONSTANTS c_abapgit_homepage TYPE string VALUE 'http://www.abapgit.org' ##NO_TEXT.
    CONSTANTS c_package_abapgit  TYPE devclass VALUE '$ABAPGIT'.
    CONSTANTS c_package_plugins  TYPE devclass VALUE '$ABAPGIT_PLUGINS'.
    CONSTANTS c_abapgit_url      TYPE string VALUE 'https://github.com/larshp/abapGit.git'.
    CONSTANTS c_plugins_url      TYPE string VALUE 'https://github.com/larshp/abapGit-plugins.git'.

    CLASS-METHODS open_abapgit_homepage
      RAISING lcx_exception.

    CLASS-METHODS install_abapgit
      RETURNING VALUE(rv_cancel) TYPE abap_bool
      RAISING lcx_exception.

    CLASS-METHODS needs_installation
      RETURNING VALUE(rv_not_completely_installed) TYPE abap_bool.

ENDCLASS. "lcl_services_abapgit

CLASS lcl_services_abapgit IMPLEMENTATION.

  METHOD open_abapgit_homepage.

    cl_gui_frontend_services=>execute(
      EXPORTING document = c_abapgit_homepage
      EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'Opening page in external browser failed.' ).
    ENDIF.

  ENDMETHOD.  "open_abapgit_homepage

  METHOD install_abapgit.

    DATA lv_text            TYPE c LENGTH 100.
    DATA lv_answer          TYPE c LENGTH 1.
    DATA lo_repo            TYPE REF TO lcl_repo_online.
    DATA lv_url             TYPE string.
    DATA lv_target_package  TYPE devclass.

    lv_text = |Installing current version ABAPGit to package { c_package_abapgit } |
           && |and plugins to { c_package_plugins }|.

    lv_answer = lcl_popups=>popup_to_confirm(
      titlebar              = 'Install abapGit'
      text_question         = lv_text
      text_button_1         = 'Continue'
      text_button_2         = 'Cancel'
      default_button        = '2'
      display_cancel_button = abap_false
    ).  "#EC NOTEXT

    IF lv_answer <> '1'.
      rv_cancel = abap_true.
      RETURN. ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ENDIF.

    DO 2 TIMES.
      CASE sy-index.
        WHEN 1.
          lv_url            = c_abapgit_url.
          lv_target_package = c_package_abapgit.
        WHEN 2.
          lv_url            = c_plugins_url.
          lv_target_package = c_package_plugins.
      ENDCASE.

      IF abap_false = lcl_app=>repo_srv( )->is_repo_installed(
          iv_url              = lv_url
          iv_target_package   = lv_target_package ).

        lcl_sap_package=>create_local( lv_target_package ).

        lo_repo = lcl_app=>repo_srv( )->new_online(
          iv_url         = lv_url
          iv_branch_name = 'refs/heads/master' "TODO replace with HEAD ?
          iv_package     = lv_target_package ) ##NO_TEXT.

        lo_repo->status( ). " check for errors
        lo_repo->deserialize( ).
      ENDIF.
    ENDDO.

    COMMIT WORK.

  ENDMETHOD.  "install_abapgit

  METHOD needs_installation.

    TRY.
        IF lcl_app=>repo_srv( )->is_repo_installed( c_abapgit_url ) = abap_false
            OR lcl_app=>repo_srv( )->is_repo_installed( c_plugins_url ) = abap_false.
          rv_not_completely_installed = abap_true.
        ENDIF.
      CATCH lcx_exception.
        " cannot be installed anyway in this case, e.g. no connection
        rv_not_completely_installed = abap_false.
    ENDTRY.

  ENDMETHOD.                    "needs_installation

ENDCLASS. "lcl_services_abapgit