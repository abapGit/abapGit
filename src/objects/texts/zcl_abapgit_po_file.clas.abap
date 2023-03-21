CLASS zcl_abapgit_po_file DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_i18n_file.

    METHODS constructor
      IMPORTING
        iv_lang TYPE laiso.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_lang TYPE laiso.
ENDCLASS.



CLASS ZCL_ABAPGIT_PO_FILE IMPLEMENTATION.


  METHOD constructor.
    mv_lang = iv_lang.
  ENDMETHOD.


  METHOD zif_abapgit_i18n_file~ext.
    rv_ext = 'po'.
  ENDMETHOD.


  METHOD zif_abapgit_i18n_file~lang.
    rv_lang = mv_lang.
  ENDMETHOD.


  METHOD zif_abapgit_i18n_file~render.
    rv_data = zcl_abapgit_convert=>string_to_xstring_utf8( |Hello { mv_lang }| ).
  ENDMETHOD.
ENDCLASS.
