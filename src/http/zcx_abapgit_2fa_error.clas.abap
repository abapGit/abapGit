class ZCX_ABAPGIT_2FA_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  data MV_TEXT type STRING read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !MV_TEXT type STRING optional .

  methods IF_MESSAGE~GET_TEXT
    redefinition .
protected section.

  methods GET_DEFAULT_TEXT
    returning
      value(RV_TEXT) type STRING .
private section.
ENDCLASS.



CLASS ZCX_ABAPGIT_2FA_ERROR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
me->MV_TEXT = MV_TEXT .
  endmethod.


  METHOD get_default_text.

    rv_text = 'Error in two factor authentication.' .

  ENDMETHOD.


  METHOD if_message~get_text.

    IF mv_text IS NOT INITIAL.
      result = mv_text.
    ELSEIF get_default_text( ) IS NOT INITIAL.
      result = get_default_text( ).
    ELSE.
      result = super->get_text( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
