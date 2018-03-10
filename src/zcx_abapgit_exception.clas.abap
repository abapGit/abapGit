"! abapGit general error
CLASS zcx_abapgit_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      if_t100_message.
    CONSTANTS:
      BEGIN OF dummy,
        msgid TYPE symsgid VALUE '02',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF dummy.
    CLASS-METHODS:
      "! Raise exception with text
      "! @parameter iv_text | Text
      "! @raising zcx_abapgit_exception | Exception
      raise IMPORTING iv_text TYPE clike
            RAISING   zcx_abapgit_exception,
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
      raise_t100 IMPORTING VALUE(iv_msgid) TYPE syst_msgid DEFAULT sy-msgid
                           VALUE(iv_msgno) TYPE syst_msgno DEFAULT sy-msgno
                           VALUE(iv_msgv1) TYPE syst_msgv DEFAULT sy-msgv1
                           VALUE(iv_msgv2) TYPE syst_msgv DEFAULT sy-msgv2
                           VALUE(iv_msgv3) TYPE syst_msgv DEFAULT sy-msgv3
                           VALUE(iv_msgv4) TYPE syst_msgv DEFAULT sy-msgv4
                 RAISING   zcx_abapgit_exception .
    METHODS:
      constructor  IMPORTING textid   LIKE if_t100_message=>t100key OPTIONAL
                             previous LIKE previous OPTIONAL
                             text     TYPE string OPTIONAL
                             subrc    TYPE syst_subrc OPTIONAL
                             msgv1    TYPE syst_msgv OPTIONAL
                             msgv2    TYPE syst_msgv OPTIONAL
                             msgv3    TYPE syst_msgv OPTIONAL
                             msgv4    TYPE syst_msgv OPTIONAL,
      if_message~get_text REDEFINITION.
    DATA:
      text  TYPE string READ-ONLY,
      subrc TYPE syst_subrc READ-ONLY,
      msgv1 TYPE syst_msgv READ-ONLY,
      msgv2 TYPE syst_msgv READ-ONLY,
      msgv3 TYPE syst_msgv READ-ONLY,
      msgv4 TYPE syst_msgv READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      gc_generic_error_msg TYPE string VALUE `An error occured (ZCX_ABAPGIT_EXCEPTION)` ##NO_TEXT.
ENDCLASS.



CLASS zcx_abapgit_exception IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).

    me->text = text.
    me->subrc = subrc.
    me->msgv1 = msgv1.
    me->msgv2 = msgv2.
    me->msgv3 = msgv3.
    me->msgv4 = msgv4.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

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
          lv_subrc    TYPE syst_subrc.

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
