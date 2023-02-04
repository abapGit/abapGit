CLASS zcl_abapgit_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS is_binary
      IMPORTING
        !iv_data            TYPE xstring
      RETURNING
        VALUE(rv_is_binary) TYPE abap_bool.
    CLASS-METHODS is_valid_email
      IMPORTING
        iv_email        TYPE string
      RETURNING
        VALUE(rv_valid) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_utils IMPLEMENTATION.


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

    TRY.
        lv_string_data = zcl_abapgit_convert=>xstring_to_string_utf8( lv_data ).
      CATCH zcx_abapgit_exception.
        " Contains data that does not convert to UTF-8 so consider it binary
        rv_is_binary = abap_true.
        RETURN.
    ENDTRY.

    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_string_data WITH space.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_string_data WITH space.

    FIND ALL OCCURRENCES OF REGEX '[^[:print:]]' IN lv_string_data MATCH COUNT lv_printable_chars_count.
    lv_percentage = lv_printable_chars_count * 100 / strlen( lv_string_data ).
    rv_is_binary = boolc( lv_percentage > lc_binary_threshold ).

  ENDMETHOD.


  METHOD is_valid_email.

    " Email address validation (RFC 5322)
    " https://www.oreilly.com/library/view/regular-expressions-cookbook/9781449327453/ch04s01.html
    CONSTANTS lc_email_regex TYPE string VALUE
      '[\w!#$%&*+/=?`{|}~^-]+(?:\.[\w!#$%&*+/=?`{|}~^-]+)*@(?:[A-Za-z0-9-]+\.)+[A-Za-z]{2,6}'.

    IF iv_email IS INITIAL.
      rv_valid = abap_true.
    ELSE.
      FIND REGEX lc_email_regex IN iv_email.
      rv_valid = boolc( sy-subrc = 0 ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
