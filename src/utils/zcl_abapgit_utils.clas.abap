CLASS zcl_abapgit_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS is_binary
      IMPORTING
        !iv_data            TYPE xstring
      RETURNING
        VALUE(rv_is_binary) TYPE abap_bool .
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

    " Previously we did a simple char range test described here
    " stackoverflow.com/questions/277521/how-to-identify-the-file-content-as-ascii-or-binary
    " but this is insufficient if the data contains german umlauts and other special characters.
    " Therefore we adopted another algorithm, which is similarily used by AL11
    " RSWATCH0 / GUESS_FILE_TYPE
    " We count non-printable characters if there are more than XX% it's binary.

    CONSTANTS:
      lc_binary_threshold TYPE i VALUE 10,
      lc_bytes_to_check   TYPE i VALUE 1000.

    DATA: lv_string_data           TYPE string,
          lv_printable_chars_count TYPE i,
          lv_percentage            TYPE i,
          lv_data                  TYPE xstring,
          lv_xlen                  TYPE i.

    lv_xlen = xstrlen( iv_data ).
    IF lv_xlen = 0.
      RETURN.
    ENDIF.

    lv_xlen = nmin(
                val1 = lv_xlen
                val2 = lc_bytes_to_check ).

    lv_data = iv_data(lv_xlen).

    lv_string_data = zcl_abapgit_convert=>xstring_to_string_utf8( lv_data ).

    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_string_data WITH space.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_string_data WITH space.

    FIND ALL OCCURRENCES OF REGEX '[^[:print:]]' IN lv_string_data MATCH COUNT lv_printable_chars_count.
    lv_percentage = lv_printable_chars_count * 100 / strlen( lv_string_data ).
    rv_is_binary = boolc( lv_percentage > lc_binary_threshold ).

  ENDMETHOD.

ENDCLASS.
