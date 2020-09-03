class ZCX_ABAPGIT_2FA_COMM_ERROR definition
  public
  inheriting from ZCX_ABAPGIT_2FA_ERROR
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !MV_TEXT type STRING optional .
protected section.

  methods GET_DEFAULT_TEXT
    redefinition .
private section.
ENDCLASS.



CLASS ZCX_ABAPGIT_2FA_COMM_ERROR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
MV_TEXT = MV_TEXT
.
  endmethod.


  METHOD get_default_text.
    rv_text = 'Communication error.' .
  ENDMETHOD.
ENDCLASS.
