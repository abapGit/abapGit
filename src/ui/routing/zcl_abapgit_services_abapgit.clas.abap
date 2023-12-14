CLASS zcl_abapgit_services_abapgit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_abapgit_repo TYPE string VALUE 'https://github.com/abapGit/abapGit' ##NO_TEXT.
    CONSTANTS c_abapgit_homepage TYPE string VALUE 'https://www.abapgit.org' ##NO_TEXT.
    CONSTANTS c_abapgit_wikipage TYPE string VALUE 'https://docs.abapgit.org' ##NO_TEXT.
    CONSTANTS c_dotabap_homepage TYPE string VALUE 'https://dotabap.org' ##NO_TEXT.
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
    CLASS-METHODS get_abapgit_tcode
      RETURNING
        VALUE(rv_tcode) TYPE tcode .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS open_url_in_browser
      IMPORTING
        !iv_url TYPE string
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_services_abapgit IMPLEMENTATION.


  METHOD get_abapgit_tcode.
    CONSTANTS lc_report_tcode_hex TYPE x VALUE '80'.
    DATA lt_tcodes TYPE STANDARD TABLE OF tcode.

    TRY.
        SELECT tcode
          FROM ('TSTC')
          INTO TABLE lt_tcodes
          WHERE pgmna = sy-cprog
          AND cinfo = lc_report_tcode_hex
          ORDER BY tcode.
      CATCH cx_sy_dynamic_osql_error.
* ABAP Cloud/Steampunk compatibility
        RETURN.
    ENDTRY.

    IF lines( lt_tcodes ) > 0.
      READ TABLE lt_tcodes INDEX 1 INTO rv_tcode.
    ENDIF.
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

ENDCLASS.
