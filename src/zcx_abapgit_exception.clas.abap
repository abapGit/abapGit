"! abapGit general error
class ZCX_ABAPGIT_EXCEPTION definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    BEGIN OF dummy,
        msgid TYPE symsgid VALUE '02',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF dummy .
  data TEXT type STRING read-only .
  data SUBRC type SYSUBRC read-only .
  data MSGV1 type SYMSGV read-only .
  data MSGV2 type SYMSGV read-only .
  data MSGV3 type SYMSGV read-only .
  data MSGV4 type SYMSGV read-only .

      "! Raise exception with text
      "! @parameter iv_text | Text
      "! @raising zcx_abapgit_exception | Exception
  class-methods RAISE
    importing
      !IV_TEXT type CLIKE
    raising
      ZCX_ABAPGIT_EXCEPTION .
      "! Raise exception with T100 message
      "! <p>
      "! Will default to sy-msg* variables. These need to be set right before calling this method.
      "! </p>
      "! @parameter iv_msgid | Message ID
      "! @parameter iv_msgno | Message number
      "! @parameter iv_msgv1 | Message variable 1
      "! @parameter iv_msgv2 | Message variable 2
      "! @parameter iv_msgv3 | Message variable 3
      "! @parameter iv_msgv4 | Message variable 4
      "! @raising zcx_abapgit_exception | Exception
  class-methods RAISE_T100
    importing
      value(IV_MSGID) type SYMSGID default SY-MSGID
      value(IV_MSGNO) type SYMSGNO default SY-MSGNO
      value(IV_MSGV1) type SYMSGV default SY-MSGV1
      value(IV_MSGV2) type SYMSGV default SY-MSGV2
      value(IV_MSGV3) type SYMSGV default SY-MSGV3
      value(IV_MSGV4) type SYMSGV default SY-MSGV4
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !TEXT type STRING optional
      !SUBRC type SYSUBRC optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional
      !MSGV3 type SYMSGV optional
      !MSGV4 type SYMSGV optional .

  methods IF_MESSAGE~GET_TEXT
    redefinition .
protected section.
private section.

  constants GC_GENERIC_ERROR_MSG type STRING value `An error occured (ZCX_ABAPGIT_EXCEPTION)` ##NO_TEXT.
ENDCLASS.



CLASS ZCX_ABAPGIT_EXCEPTION IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->TEXT = TEXT .
me->SUBRC = SUBRC .
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD if_message~get_text.
    " The standard implementation of this method always uses T100 messages, if IF_T100_MESSAGE is
    " implemented. Since this is a 'hybrid' exception of IF_MESSAGE and IF_T100_MESSAGE the normal
    " get_text() is only called if a T100 message is used.
    " Otherwise try to get a meaningful error text for the user in this order:
    " mv_text variable, previous exception's text, generic error message

    IF if_t100_message~t100key IS INITIAL OR
       if_t100_message~t100key = if_t100_message=>default_textid.
      IF text IS NOT INITIAL.
        result = text.
      ELSEIF previous IS NOT INITIAL.
        result = previous->get_text( ).
      ELSE.
        IF sy-subrc IS NOT INITIAL.
          result = |{ gc_generic_error_msg } ({ subrc })|.
        ELSE.
          result = gc_generic_error_msg.
        ENDIF.
      ENDIF.

    ELSE.
      result = super->get_text( ).
    ENDIF.
  ENDMETHOD.


  METHOD raise.
    RAISE EXCEPTION TYPE zcx_abapgit_exception
      EXPORTING
        text = iv_text.
  ENDMETHOD.


  METHOD raise_t100.
    DATA: ls_t100_key TYPE scx_t100key,
          lv_subrc    TYPE sysubrc.

    " If this method is called right in the sy-subrc handling of a method or function module
    " sy-subrc should still contain the id from that.
    lv_subrc = sy-subrc.

    ls_t100_key-msgid = iv_msgid.
    ls_t100_key-msgno = iv_msgno.
    ls_t100_key-attr1 = 'MSGV1'.
    ls_t100_key-attr2 = 'MSGV2'.
    ls_t100_key-attr3 = 'MSGV3'.
    ls_t100_key-attr4 = 'MSGV4'.

    IF iv_msgid IS INITIAL OR iv_msgno IS INITIAL.
      CLEAR ls_t100_key.
    ENDIF.

    RAISE EXCEPTION TYPE zcx_abapgit_exception
      EXPORTING
        textid = ls_t100_key
        msgv1  = iv_msgv1
        msgv2  = iv_msgv2
        msgv3  = iv_msgv3
        msgv4  = iv_msgv4
        subrc  = lv_subrc.
  ENDMETHOD.
ENDCLASS.
