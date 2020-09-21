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
    CLASS-METHODS extract_author_data
      IMPORTING
        !iv_author TYPE string
      EXPORTING
        !ev_author  TYPE zif_abapgit_definitions=>ty_commit-author
        !ev_email   TYPE zif_abapgit_definitions=>ty_commit-email
        !ev_time    TYPE zif_abapgit_definitions=>ty_commit-time
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_UTILS IMPLEMENTATION.


  METHOD extract_author_data.

    " unix time stamps are in same time zone, so ignore the zone
    FIND REGEX zif_abapgit_definitions=>c_author_regex IN iv_author
      SUBMATCHES
      ev_author
      ev_email
      ev_time.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error author regex value='{ iv_author }'| ).
    ENDIF.

  ENDMETHOD.


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
