CLASS zcl_abapgit_hash DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS adler32
      IMPORTING
        !iv_xstring        TYPE xstring
      RETURNING
        VALUE(rv_checksum) TYPE zif_abapgit_git_definitions=>ty_adler32 .
    CLASS-METHODS sha1
      IMPORTING
        !iv_type       TYPE zif_abapgit_git_definitions=>ty_type
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_git_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS sha1_commit
      IMPORTING
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_git_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS sha1_tree
      IMPORTING
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_git_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS sha1_tag
      IMPORTING
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_git_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS sha1_blob
      IMPORTING
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_git_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS sha1_raw
      IMPORTING
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_git_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS sha1_string
      IMPORTING
        !iv_data       TYPE string
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_git_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_hash IMPLEMENTATION.


  METHOD adler32.

    CONSTANTS: lc_adler TYPE i VALUE 65521,
               lc_max_b TYPE i VALUE 1800000000.

    DATA: lv_index TYPE i,
          lv_a     TYPE i VALUE 1,
          lv_b     TYPE i VALUE 0,
          lv_x     TYPE x LENGTH 2,
          lv_ca    TYPE c LENGTH 4,
          lv_cb    TYPE c LENGTH 4,
          lv_char8 TYPE c LENGTH 8.


    DO xstrlen( iv_xstring ) TIMES.
      lv_index = sy-index - 1.

      lv_a = lv_a + iv_xstring+lv_index(1).
      lv_b = lv_b + lv_a.

* delay the MOD operation until the integer might overflow
* articles describe 5552 additions are allowed, but this assumes unsigned integers
* instead of allowing a fixed number of additions before running MOD, then
* just compare value of lv_b, this is 1 operation less than comparing and adding
      IF lv_b > lc_max_b.
        lv_a = lv_a MOD lc_adler.
        lv_b = lv_b MOD lc_adler.
      ENDIF.
    ENDDO.

    lv_a = lv_a MOD lc_adler.
    lv_b = lv_b MOD lc_adler.

    lv_x = lv_a.
    lv_ca = lv_x.

    lv_x = lv_b.
    lv_cb = lv_x.

    CONCATENATE lv_cb lv_ca INTO lv_char8.

    rv_checksum = lv_char8.

  ENDMETHOD.


  METHOD sha1.

    DATA: lv_len     TYPE i,
          lv_char10  TYPE c LENGTH 10,
          lv_string  TYPE string,
          lv_xstring TYPE xstring.


    lv_len = xstrlen( iv_data ).
    lv_char10 = lv_len.
    CONDENSE lv_char10.
    CONCATENATE iv_type lv_char10 INTO lv_string SEPARATED BY space.
    lv_xstring = zcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

    lv_string = lv_xstring.
    CONCATENATE lv_string '00' INTO lv_string.
    lv_xstring = lv_string.

    CONCATENATE lv_xstring iv_data INTO lv_xstring IN BYTE MODE.

    rv_sha1 = sha1_raw( lv_xstring ).

  ENDMETHOD.


  METHOD sha1_blob.
    rv_sha1 = sha1( iv_type = zif_abapgit_definitions=>c_type-blob
                    iv_data = iv_data ).
  ENDMETHOD.


  METHOD sha1_commit.
    rv_sha1 = sha1( iv_type = zif_abapgit_definitions=>c_type-commit
                    iv_data = iv_data ).
  ENDMETHOD.


  METHOD sha1_raw.

    DATA: lv_hash  TYPE string,
          lv_key   TYPE xstring,
          lx_error TYPE REF TO cx_abap_message_digest.
    TRY.
        cl_abap_hmac=>calculate_hmac_for_raw(
      EXPORTING
        if_key        = lv_key
        if_data       = iv_data
      IMPORTING
        ef_hmacstring = lv_hash ).
      CATCH cx_abap_message_digest INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    rv_sha1 = lv_hash.
    TRANSLATE rv_sha1 TO LOWER CASE.

  ENDMETHOD.


  METHOD sha1_string.

    DATA: lv_hash  TYPE string,
          lv_key   TYPE xstring,
          lx_error TYPE REF TO cx_abap_message_digest.
    TRY.
        cl_abap_hmac=>calculate_hmac_for_char(
      EXPORTING
        if_key        = lv_key
        if_data       = iv_data
      IMPORTING
        ef_hmacstring = lv_hash ).
      CATCH cx_abap_message_digest INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    rv_sha1 = lv_hash.
    TRANSLATE rv_sha1 TO LOWER CASE.

  ENDMETHOD.


  METHOD sha1_tag.
    rv_sha1 = sha1( iv_type = zif_abapgit_definitions=>c_type-tag
                    iv_data = iv_data ).
  ENDMETHOD.


  METHOD sha1_tree.
    rv_sha1 = sha1( iv_type = zif_abapgit_definitions=>c_type-tree
                    iv_data = iv_data ).
  ENDMETHOD.
ENDCLASS.
