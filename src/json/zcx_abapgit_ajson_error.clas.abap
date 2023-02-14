class zcx_abapgit_ajson_error definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  types:
    ty_rc type c length 4 .

  constants:
    begin of ZCX_AJSON_ERROR,
      msgid type symsgid value '00',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'A1',
      attr2 type scx_attrname value 'A2',
      attr3 type scx_attrname value 'A3',
      attr4 type scx_attrname value 'A4',
    end of ZCX_AJSON_ERROR .
  data RC type TY_RC read-only .
  data MESSAGE type STRING read-only .
  data LOCATION type STRING read-only .
  data A1 type SYMSGV read-only .
  data A2 type SYMSGV read-only .
  data A3 type SYMSGV read-only .
  data A4 type SYMSGV read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !RC type TY_RC optional
      !MESSAGE type STRING optional
      !LOCATION type STRING optional
      !A1 type SYMSGV optional
      !A2 type SYMSGV optional
      !A3 type SYMSGV optional
      !A4 type SYMSGV optional .
  class-methods RAISE
    importing
      !IV_MSG type STRING
      !IV_LOCATION type STRING optional
      !IS_NODE type ANY optional
    raising
      zcx_abapgit_ajson_error .
  methods SET_LOCATION
    importing
      !IV_LOCATION type STRING optional
      !IS_NODE type ANY optional
    preferred parameter IV_LOCATION .
protected section.
private section.
  types:
    begin of ty_message_parts,
      a1 like a1,
      a2 like a1,
      a3 like a1,
      a4 like a1,
    end of ty_message_parts.
ENDCLASS.



CLASS zcx_abapgit_ajson_error IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->RC = RC .
me->MESSAGE = MESSAGE .
me->LOCATION = LOCATION .
me->A1 = A1 .
me->A2 = A2 .
me->A3 = A3 .
me->A4 = A4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_AJSON_ERROR .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
endmethod.


method raise.

  data lx type ref to zcx_abapgit_ajson_error.

  create object lx exporting message = iv_msg.
  lx->set_location(
    iv_location = iv_location
    is_node     = is_node ).
  raise exception lx.

endmethod.


method set_location.

  data ls_msg type ty_message_parts.
  data lv_location type string.
  data lv_tmp type string.
  field-symbols <path> type string.
  field-symbols <name> type string.

  if iv_location is not initial.
    lv_location = iv_location.
  elseif is_node is not initial.
    assign component 'PATH' of structure is_node to <path>.
    assign component 'NAME' of structure is_node to <name>.
    if <path> is assigned and <name> is assigned.
      lv_location = <path> && <name>.
    endif.
  endif.

  if lv_location is not initial.
    lv_tmp = message && | @{ lv_location }|.
  else.
    lv_tmp = message.
  endif.

  ls_msg = lv_tmp.

  location = lv_location.
  a1       = ls_msg-a1.
  a2       = ls_msg-a2.
  a3       = ls_msg-a3.
  a4       = ls_msg-a4.

endmethod.
ENDCLASS.
