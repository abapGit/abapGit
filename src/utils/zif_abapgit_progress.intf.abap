INTERFACE zif_abapgit_progress
  PUBLIC .


  METHODS show
    IMPORTING
      !iv_current TYPE i
      !iv_text    TYPE csequence .
  METHODS set_total
    IMPORTING
      !iv_total TYPE i .
  METHODS off .
ENDINTERFACE.
