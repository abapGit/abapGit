CLASS lcl_stream DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES: ty_hex TYPE x LENGTH 1.

    METHODS:
      constructor
        IMPORTING
          iv_data TYPE xstring,
      eat_byte
        RETURNING VALUE(rv_x) TYPE ty_hex,
      eat_bytes
        IMPORTING
          iv_length   TYPE i
        RETURNING
          VALUE(rv_x) TYPE xstring.

    METHODS has_data
      RETURNING
        VALUE(rv_data) TYPE abap_bool.

  PRIVATE SECTION.
    DATA mv_data TYPE xstring.
    DATA mv_position TYPE i.
    DATA mv_length TYPE i.
ENDCLASS.
