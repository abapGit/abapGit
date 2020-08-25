class ZCX_ABAPGIT_AJSON_ERROR definition
  public
  inheriting from ZCX_ABAPGIT_EXCEPTION
  final
  create public .

public section.

  constants:
    begin of ZCX_ABAPGIT_AJSON_ERROR,
      msgid type symsgid value '00',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_ABAPGIT_AJSON_ERROR .
  data MESSAGE type STRING read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional
      !MSGV3 type SYMSGV optional
      !MSGV4 type SYMSGV optional
      !MESSAGE type STRING optional .
  class-methods RAISE_JSON
    importing
      !IV_MSG type STRING
      !IV_LOCATION type STRING optional
    raising
      ZCX_ABAPGIT_AJSON_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCX_ABAPGIT_AJSON_ERROR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MSGV1 = MSGV1
MSGV2 = MSGV2
MSGV3 = MSGV3
MSGV4 = MSGV4
.
me->MESSAGE = MESSAGE .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_ABAPGIT_AJSON_ERROR .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD raise_json.

    DATA lv_tmp TYPE string.
    DATA:
      BEGIN OF ls_msg,
        a1 LIKE msgv1,
        a2 LIKE msgv1,
        a3 LIKE msgv1,
        a4 LIKE msgv1,
      END OF ls_msg.

    IF iv_location IS INITIAL.
      ls_msg = iv_msg.
    ELSE.
      lv_tmp = iv_msg && | @{ iv_location }|.
      ls_msg = lv_tmp.
    ENDIF.

    RAISE EXCEPTION TYPE zcx_abapgit_ajson_error
      EXPORTING
        textid = zcx_abapgit_ajson_error
        message = iv_msg
        msgv1  = ls_msg-a1
        msgv2  = ls_msg-a2
        msgv3  = ls_msg-a3
        msgv4  = ls_msg-a4.

  ENDMETHOD.
ENDCLASS.
