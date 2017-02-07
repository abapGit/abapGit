*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_EXCEPTIONS
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS LCX_EXCEPTION DEFINITION
*----------------------------------------------------------------------*
CLASS lcx_exception DEFINITION INHERITING FROM cx_static_check FINAL.

  PUBLIC SECTION.
    DATA mv_text TYPE string.

    METHODS constructor
      IMPORTING iv_text     TYPE string
                ix_previous TYPE REF TO cx_root OPTIONAL.

    CLASS-METHODS: raise IMPORTING iv_text TYPE clike
                         RAISING   lcx_exception.

  PRIVATE SECTION.
    DATA mx_previous TYPE REF TO cx_root.

ENDCLASS.                    "CX_LOCAL_EXCEPTION DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCX_EXCEPTION IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcx_exception IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mv_text = iv_text.
    mx_previous = previous.
  ENDMETHOD.                    "CONSTRUCTOR

  METHOD raise.
    RAISE EXCEPTION TYPE lcx_exception
      EXPORTING
        iv_text = iv_text.
  ENDMETHOD.

ENDCLASS.                    "lcx_exception IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCX_NOT_FOUND DEFINITION
*----------------------------------------------------------------------*
CLASS lcx_not_found DEFINITION INHERITING FROM cx_static_check FINAL.

ENDCLASS.                    "CX_LOCAL_EXCEPTION DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCX_NOT_FOUND IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcx_not_found IMPLEMENTATION.

ENDCLASS.                    "lcx_not_found IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCX_CANCEL DEFINITION
*----------------------------------------------------------------------*
CLASS lcx_cancel DEFINITION INHERITING FROM cx_static_check FINAL.

ENDCLASS.                    "lcx_cancel DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCX_CANCEL IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcx_cancel IMPLEMENTATION.

ENDCLASS.                    "lcx_cancel IMPLEMENTATION
