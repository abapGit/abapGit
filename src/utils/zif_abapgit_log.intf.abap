INTERFACE zif_abapgit_log
  PUBLIC .


  METHODS add
    IMPORTING
      !iv_msg  TYPE csequence
      !iv_type TYPE symsgty DEFAULT 'E'
      !iv_rc   TYPE balsort OPTIONAL .
  METHODS add_error
    IMPORTING
      !iv_msg TYPE csequence .
  METHODS add_info
    IMPORTING
      !iv_msg TYPE csequence .
  METHODS add_warning
    IMPORTING
      !iv_msg TYPE csequence .
  METHODS clear .
  METHODS count
    RETURNING
      VALUE(rv_count) TYPE i .
  METHODS has_rc
    IMPORTING
      !iv_rc        TYPE balsort
    RETURNING
      VALUE(rv_yes) TYPE abap_bool .
  METHODS show
    IMPORTING
      !iv_header_text TYPE csequence DEFAULT 'Log' .
  METHODS to_html
    RETURNING
      VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
  METHODS write .
ENDINTERFACE.
