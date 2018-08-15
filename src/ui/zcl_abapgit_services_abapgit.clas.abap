CLASS zcl_abapgit_services_abapgit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_abapgit_homepage TYPE string VALUE 'http://www.abapgit.org' ##NO_TEXT.
    CONSTANTS c_abapgit_wikipage TYPE string VALUE 'http://docs.abapgit.org' ##NO_TEXT.
    CONSTANTS c_package_abapgit TYPE devclass VALUE '$ABAPGIT' ##NO_TEXT.
    CONSTANTS c_abapgit_url TYPE string VALUE 'https://github.com/larshp/abapGit.git' ##NO_TEXT.

    CLASS-METHODS open_abapgit_homepage
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS open_abapgit_wikipage
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS install_abapgit
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
    CLASS-METHODS is_installed
      RETURNING
        VALUE(rv_installed) TYPE abap_bool .
  PRIVATE SECTION.

    CLASS-METHODS do_install
      IMPORTING iv_title   TYPE c
                iv_text    TYPE c
                iv_url     TYPE string
                iv_package TYPE devclass
      RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_services_abapgit IMPLEMENTATION.


  METHOD do_install.

    DATA: lo_repo   TYPE REF TO zcl_abapgit_repo_online,
          lv_answer TYPE c LENGTH 1.


    lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = iv_title
      iv_text_question         = iv_text
      iv_text_button_1         = 'Continue'
      iv_text_button_2         = 'Cancel'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false ).                 "#EC NOTEXT

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


  METHOD install_abapgit.

    CONSTANTS lc_title TYPE c LENGTH 40 VALUE 'Install abapGit'.
    DATA lv_text       TYPE c LENGTH 100.

    IF is_installed( ) = abap_true.
      lv_text = 'Seems like abapGit package is already installed. No changes to be done'.
      zcl_abapgit_ui_factory=>get_popups( )->popup_to_inform(
        iv_titlebar              = lc_title
        iv_text_message          = lv_text ).
      RETURN.
    ENDIF.

    lv_text = |Confirm to install current version of abapGit to package { c_package_abapgit }|.

    do_install( iv_title   = lc_title
                iv_text    = lv_text
                iv_url     = c_abapgit_url
                iv_package = c_package_abapgit ).

  ENDMETHOD.

  METHOD is_installed.

    TRY.
        rv_installed = zcl_abapgit_repo_srv=>get_instance( )->is_repo_installed( c_abapgit_url ).
        " TODO, alternative checks for presence in the system
      CATCH zcx_abapgit_exception.
        " cannot be installed anyway in this case, e.g. no connection
        rv_installed = abap_false.
    ENDTRY.

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
ENDCLASS.
