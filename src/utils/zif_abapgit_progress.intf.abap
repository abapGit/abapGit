INTERFACE zif_abapgit_progress
  PUBLIC .

  METHODS show
    IMPORTING
      iv_current TYPE i
      iv_text    TYPE csequence .

ENDINTERFACE.
