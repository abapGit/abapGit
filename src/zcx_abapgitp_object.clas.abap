class ZCX_ABAPGITP_OBJECT definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  data TEXT type STRING read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !TEXT type STRING optional .

  methods IF_MESSAGE~GET_TEXT
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCX_ABAPGITP_OBJECT IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
me->TEXT = TEXT .
  endmethod.


  METHOD IF_MESSAGE~GET_TEXT.

    IF text IS NOT INITIAL.
      result = text.
    ELSE.
      result = super->get_text( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
