CLASS zcl_abapgit_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS is_binary
      IMPORTING
        !iv_data      TYPE xstring
      RETURNING
        VALUE(rv_yes) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_UTILS IMPLEMENTATION.


  METHOD is_binary.

    DATA: lv_len TYPE i,
          lv_idx TYPE i,
          lv_x   TYPE x.

    lv_len = xstrlen( iv_data ).
    IF lv_len = 0.
      RETURN.
    ENDIF.

    IF lv_len > 100.
      lv_len = 100.
    ENDIF.

    " Simple char range test
    " stackoverflow.com/questions/277521/how-to-identify-the-file-content-as-ascii-or-binary
    DO lv_len TIMES. " I'm sure there is more efficient way ...
      lv_idx = sy-index - 1.
      lv_x = iv_data+lv_idx(1).

      IF NOT ( lv_x BETWEEN 9 AND 13 OR lv_x BETWEEN 32 AND 126 ).
        rv_yes = abap_true.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.
ENDCLASS.
