CLASS zcl_abapgit_string_map DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_entry,
        k TYPE string,
        v TYPE string,
      END OF ty_entry,
      tty_entries TYPE STANDARD TABLE OF ty_entry WITH KEY k,
      tts_entries TYPE SORTED TABLE OF ty_entry WITH UNIQUE KEY k.

    CLASS-METHODS create
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_string_map.

    METHODS get
      IMPORTING
        iv_key TYPE string
      RETURNING
        VALUE(rv_val) TYPE string.

    METHODS has
      IMPORTING
        iv_key TYPE string
      RETURNING
        VALUE(rv_has) TYPE abap_bool.

    METHODS set
      IMPORTING
        iv_key TYPE string
        iv_val TYPE string OPTIONAL.

    METHODS size
      RETURNING
        VALUE(rv_size) TYPE i.

    METHODS is_empty
      RETURNING
        VALUE(rv_yes) TYPE abap_bool.

    METHODS delete
      IMPORTING
        iv_key TYPE string.

    METHODS clear.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_entries TYPE tts_entries.

ENDCLASS.



CLASS ZCL_ABAPGIT_STRING_MAP IMPLEMENTATION.


  METHOD clear.
    clear mt_entries.
  ENDMETHOD.


  METHOD create.
    CREATE OBJECT ro_instance.
  ENDMETHOD.


  METHOD delete.

    DELETE mt_entries WHERE k = iv_key.

  ENDMETHOD.


  METHOD get.

    FIELD-SYMBOLS <ls_entry> LIKE LINE OF mt_entries.
    READ TABLE mt_entries ASSIGNING <ls_entry> WITH KEY k = iv_key.
    IF sy-subrc IS INITIAL.
      rv_val = <ls_entry>-v.
    ENDIF.

  ENDMETHOD.


  METHOD has.

    READ TABLE mt_entries TRANSPORTING NO FIELDS WITH KEY k = iv_key.
    rv_has = boolc( sy-subrc IS INITIAL ).

  ENDMETHOD.


  METHOD is_empty.
    rv_yes = boolc( lines( mt_entries ) = 0 ).
  ENDMETHOD.


  METHOD set.

    DATA ls_entry LIKE LINE OF mt_entries.
    FIELD-SYMBOLS <ls_entry> LIKE LINE OF mt_entries.

    READ TABLE mt_entries ASSIGNING <ls_entry> WITH KEY k = iv_key.
    IF sy-subrc IS INITIAL.
      <ls_entry>-v = iv_val.
    ELSE.
      ls_entry-k = iv_key.
      ls_entry-v = iv_val.
      INSERT ls_entry INTO TABLE mt_entries.
    ENDIF.

  ENDMETHOD.


  METHOD size.

    rv_size = lines( mt_entries ).

  ENDMETHOD.
ENDCLASS.
