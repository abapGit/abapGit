CLASS zcl_abapgit_convert DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS string_to_xstring_utf8
      IMPORTING
        !iv_string        TYPE string
      RETURNING
        VALUE(rv_xstring) TYPE xstring
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS xstring_to_string_utf8
      IMPORTING
        !iv_data         TYPE xsequence
        !iv_length       TYPE i OPTIONAL
      RETURNING
        VALUE(rv_string) TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS xstring_to_string_utf8_raw
      IMPORTING
        !iv_data         TYPE xsequence
        !iv_length       TYPE i OPTIONAL
      RETURNING
        VALUE(rv_string) TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS string_to_xstring_utf8_bom
      IMPORTING
        !iv_string        TYPE string
      RETURNING
        VALUE(rv_xstring) TYPE xstring
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS xstring_to_string_utf8_bom
      IMPORTING
        !iv_xstring      TYPE xstring
      RETURNING
        VALUE(rv_string) TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS xstring_to_int
      IMPORTING
        !iv_xstring TYPE xstring
      RETURNING
        VALUE(rv_i) TYPE i
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS int_to_xstring4
      IMPORTING
        !iv_i             TYPE i
      RETURNING
        VALUE(rv_xstring) TYPE xstring .
    CLASS-METHODS split_string
      IMPORTING
        !iv_string      TYPE string
      RETURNING
        VALUE(rt_lines) TYPE string_table .
    CLASS-METHODS string_to_xstring
      IMPORTING
        !iv_str        TYPE string
      RETURNING
        VALUE(rv_xstr) TYPE xstring
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS string_to_tab
      IMPORTING
        !iv_str  TYPE string
      EXPORTING
        !ev_size TYPE i
        !et_tab  TYPE STANDARD TABLE .
    CLASS-METHODS base64_to_xstring
      IMPORTING
        !iv_base64     TYPE string
      RETURNING
        VALUE(rv_xstr) TYPE xstring .
    CLASS-METHODS xstring_to_bintab
      IMPORTING
        !iv_xstr   TYPE xsequence
      EXPORTING
        !ev_size   TYPE i
        !et_bintab TYPE STANDARD TABLE .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS xstring_remove_bom
      IMPORTING
        iv_xstr        TYPE xsequence
      RETURNING
        VALUE(rv_xstr) TYPE xstring.
ENDCLASS.



CLASS zcl_abapgit_convert IMPLEMENTATION.


  METHOD base64_to_xstring.

    rv_xstr = cl_http_utility=>decode_x_base64( iv_base64 ).

  ENDMETHOD.


  METHOD int_to_xstring4.
* returns xstring of length 4 containing the integer value iv_i

    DATA lv_x TYPE x LENGTH 4.

    lv_x = iv_i.
    rv_xstring = lv_x.

  ENDMETHOD.


  METHOD split_string.

    FIND FIRST OCCURRENCE OF cl_abap_char_utilities=>cr_lf IN iv_string.

    " Convert string into table depending on separator type CR_LF vs. LF
    IF sy-subrc = 0.
      SPLIT iv_string AT cl_abap_char_utilities=>cr_lf INTO TABLE rt_lines.
    ELSE.
      SPLIT iv_string AT cl_abap_char_utilities=>newline INTO TABLE rt_lines.
    ENDIF.

  ENDMETHOD.


  METHOD string_to_tab.

    DATA lv_length TYPE i.
    DATA lv_iterations TYPE i.
    DATA lv_offset TYPE i.

    FIELD-SYMBOLS <lg_line> TYPE any.


    CLEAR et_tab.
    ev_size = strlen( iv_str ).

    APPEND INITIAL LINE TO et_tab ASSIGNING <lg_line>.
    <lg_line> = iv_str.
    lv_length = cl_abap_typedescr=>describe_by_data( <lg_line> )->length / cl_abap_char_utilities=>charsize.
    lv_iterations = ev_size DIV lv_length.

    DO lv_iterations TIMES.
      lv_offset = sy-index * lv_length.
      APPEND INITIAL LINE TO et_tab ASSIGNING <lg_line>.
      <lg_line> = iv_str+lv_offset.
    ENDDO.

  ENDMETHOD.


  METHOD string_to_xstring.

    rv_xstr = string_to_xstring_utf8( iv_str ).

  ENDMETHOD.


  METHOD string_to_xstring_utf8.

    rv_xstring = lcl_out=>convert( iv_string ).

  ENDMETHOD.


  METHOD string_to_xstring_utf8_bom.

    IF iv_string IS INITIAL.
      RETURN.
    ENDIF.

    rv_xstring = string_to_xstring_utf8( iv_string ).

    " Add UTF-8 BOM
    IF xstrlen( rv_xstring ) < 3 OR rv_xstring(3) <> cl_abap_char_utilities=>byte_order_mark_utf8.
      rv_xstring = cl_abap_char_utilities=>byte_order_mark_utf8 && rv_xstring.
    ENDIF.

  ENDMETHOD.


  METHOD xstring_remove_bom.

    rv_xstr = iv_xstr.

    " cl_abap_conv_in_ce does not handle BOM in non-Unicode systems, so we remove it
    IF cl_abap_char_utilities=>charsize = 1 AND xstrlen( rv_xstr ) > 3
        AND rv_xstr(3) = cl_abap_char_utilities=>byte_order_mark_utf8.

      rv_xstr = rv_xstr+3.

    ENDIF.

  ENDMETHOD.


  METHOD xstring_to_bintab.

    DATA lv_length TYPE i.
    DATA lv_iterations TYPE i.
    DATA lv_offset TYPE i.
    DATA lv_struct TYPE abap_bool.

    FIELD-SYMBOLS <lg_line> TYPE any.

    CLEAR et_bintab.
    ev_size = xstrlen( iv_xstr ).

    IF iv_xstr IS INITIAL.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO et_bintab ASSIGNING <lg_line>.
    lv_struct = boolc(
      cl_abap_typedescr=>describe_by_data( <lg_line> )->type_kind = cl_abap_typedescr=>typekind_struct1 ).
    IF lv_struct = abap_true.
      ASSIGN COMPONENT 1 OF STRUCTURE <lg_line> TO <lg_line>.
    ENDIF.
    <lg_line> = iv_xstr.

    lv_length = cl_abap_typedescr=>describe_by_data( <lg_line> )->length.
    ASSERT lv_length > 0.

    lv_iterations = ( ev_size - 1 ) DIV lv_length.

    DO lv_iterations TIMES.
      lv_offset = sy-index * lv_length.
      APPEND INITIAL LINE TO et_bintab ASSIGNING <lg_line>.
      IF lv_struct = abap_true.
        ASSIGN COMPONENT 1 OF STRUCTURE <lg_line> TO <lg_line>.
      ENDIF.
      <lg_line> = iv_xstr+lv_offset.
    ENDDO.

  ENDMETHOD.


  METHOD xstring_to_int.

* use the built-in type conversion
    rv_i = iv_xstring.

  ENDMETHOD.


  METHOD xstring_to_string_utf8.

    DATA lv_data   TYPE xstring.
    DATA lv_length TYPE i.

    " Remove BOM for non-Unicode systems
    lv_data = xstring_remove_bom( iv_data ).

    lv_length = iv_length.
    IF lv_length <= 0.
      lv_length = xstrlen( lv_data ).
    ENDIF.

    rv_string = lcl_in=>convert(
      iv_data   = lv_data
      iv_length = lv_length ).

  ENDMETHOD.


  METHOD xstring_to_string_utf8_bom.

    DATA lv_xstring TYPE xstring.

    IF iv_xstring IS INITIAL.
      RETURN.
    ENDIF.

    lv_xstring = iv_xstring.
    " Add UTF-8 BOM
    IF xstrlen( lv_xstring ) < 3 OR lv_xstring(3) <> cl_abap_char_utilities=>byte_order_mark_utf8.
      lv_xstring = cl_abap_char_utilities=>byte_order_mark_utf8 && lv_xstring.
    ENDIF.

    rv_string = xstring_to_string_utf8( lv_xstring ).

  ENDMETHOD.


  METHOD xstring_to_string_utf8_raw.

    DATA lv_length TYPE i.

    lv_length = iv_length.
    IF lv_length <= 0.
      lv_length = xstrlen( iv_data ).
    ENDIF.

    rv_string = lcl_in=>convert(
      iv_data   = iv_data
      iv_length = lv_length ).

  ENDMETHOD.
ENDCLASS.
