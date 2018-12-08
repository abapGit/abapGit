CLASS zcl_abapgit_string_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS string_to_xstring
      IMPORTING
        iv_str         TYPE string
      RETURNING
        VALUE(rv_xstr) TYPE xstring.

    CLASS-METHODS base64_to_xstring
      IMPORTING
        iv_base64      TYPE string
      RETURNING
        VALUE(rv_xstr) TYPE xstring.

    CLASS-METHODS bintab_to_xstring
      IMPORTING
        it_bintab      TYPE lvc_t_mime
        iv_size        TYPE i
      RETURNING
        VALUE(rv_xstr) TYPE xstring.

    CLASS-METHODS xstring_to_bintab
      IMPORTING
        iv_xstr   TYPE xstring
      EXPORTING
        ev_size   TYPE i
        et_bintab TYPE lvc_t_mime.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_STRING_UTILS IMPLEMENTATION.


  METHOD base64_to_xstring.

    CALL FUNCTION 'SSFC_BASE64_DECODE'
      EXPORTING
        b64data = iv_base64
      IMPORTING
        bindata = rv_xstr
      EXCEPTIONS
        OTHERS  = 1.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD bintab_to_xstring.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = iv_size
      IMPORTING
        buffer       = rv_xstr
      TABLES
        binary_tab   = it_bintab
      EXCEPTIONS
        failed       = 1 ##FM_SUBRC_OK.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD string_to_xstring.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = iv_str
      IMPORTING
        buffer = rv_xstr
      EXCEPTIONS
        OTHERS = 1.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD xstring_to_bintab.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = iv_xstr
    IMPORTING
      output_length = ev_size
    TABLES
      binary_tab    = et_bintab.

  ENDMETHOD.
ENDCLASS.
