class ZCX_READ_PROGDIR_EXCEPTION definition
  public
  inheriting from CX_DYNAMIC_CHECK
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_READ_PROGDIR_EXCEPTION IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
  endmethod.
ENDCLASS.
