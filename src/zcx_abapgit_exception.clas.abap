CLASS zcx_abapgit_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA text TYPE string .

    METHODS constructor
      IMPORTING
        !textid   LIKE textid OPTIONAL
        !previous LIKE previous OPTIONAL
        !text     TYPE string OPTIONAL.

    CLASS-METHODS raise
      IMPORTING
        !iv_text TYPE clike
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcx_abapgit_exception IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.

    me->text = text .

  ENDMETHOD.

  METHOD raise.

    RAISE EXCEPTION TYPE zcx_abapgit_exception
      EXPORTING
        text = iv_text.

  ENDMETHOD.

ENDCLASS.
