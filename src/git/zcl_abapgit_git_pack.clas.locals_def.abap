*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_stream DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES: ty_hex TYPE x LENGTH 1.

    METHODS:
      constructor
        IMPORTING
          iv_data TYPE xstring,
      get
        RETURNING VALUE(rv_data) TYPE xstring,
      eat_byte
        RETURNING VALUE(rv_x) TYPE ty_hex,
      eat_bytes
        IMPORTING
          iv_length   TYPE i
        RETURNING
          VALUE(rv_x) TYPE xstring.

  PRIVATE SECTION.
    DATA: mv_data TYPE xstring.
ENDCLASS.
