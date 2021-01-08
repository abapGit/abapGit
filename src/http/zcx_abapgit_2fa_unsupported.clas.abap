class ZCX_ABAPGIT_2FA_UNSUPPORTED definition
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



CLASS ZCX_ABAPGIT_2FA_UNSUPPORTED IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
MV_TEXT = MV_TEXT
.
  endmethod.


  METHOD get_default_text.
    rv_text = 'The service is not supported for two factor authentication.' .
  ENDMETHOD.
ENDCLASS.
