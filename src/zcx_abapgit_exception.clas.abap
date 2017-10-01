class ZCX_ABAPGIT_EXCEPTION definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  data TEXT type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !TEXT type STRING optional .
  class-methods RAISE
    importing
      !IV_TEXT type CLIKE
    raising
      ZCX_ABAPGIT_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS ZCX_ABAPGIT_EXCEPTION IMPLEMENTATION.


  method CONSTRUCTOR ##ADT_SUPPRESS_GENERATION.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
me->TEXT = TEXT .
  endmethod.


  METHOD raise.

    RAISE EXCEPTION TYPE zcx_abapgit_exception
      EXPORTING
        text = iv_text.

  ENDMETHOD.
ENDCLASS.
