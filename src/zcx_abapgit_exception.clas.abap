"! abapGit general error
CLASS zcx_abapgit_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      if_t100_message.
    CLASS-METHODS:
      "! Raise exception with text
      "! @parameter iv_text | Text
      "! @parameter ix_previous | Previous exception
      "! @raising zcx_abapgit_exception | Exception
      raise IMPORTING iv_text     TYPE clike
                      ix_previous TYPE REF TO cx_root OPTIONAL
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
      raise_t100 IMPORTING VALUE(iv_msgid) TYPE symsgid DEFAULT sy-msgid
                           VALUE(iv_msgno) TYPE symsgno DEFAULT sy-msgno
                           VALUE(iv_msgv1) TYPE symsgv DEFAULT sy-msgv1
                           VALUE(iv_msgv2) TYPE symsgv DEFAULT sy-msgv2
                           VALUE(iv_msgv3) TYPE symsgv DEFAULT sy-msgv3
                           VALUE(iv_msgv4) TYPE symsgv DEFAULT sy-msgv4
                 RAISING   zcx_abapgit_exception .
    METHODS:
      constructor  IMPORTING textid   LIKE if_t100_message=>t100key OPTIONAL
                             previous LIKE previous OPTIONAL
                             msgv1    TYPE symsgv OPTIONAL
                             msgv2    TYPE symsgv OPTIONAL
                             msgv3    TYPE symsgv OPTIONAL
                             msgv4    TYPE symsgv OPTIONAL.
    DATA:
      subrc TYPE sysubrc READ-ONLY,
      msgv1 TYPE symsgv READ-ONLY,
      msgv2 TYPE symsgv READ-ONLY,
      msgv3 TYPE symsgv READ-ONLY,
      msgv4 TYPE symsgv READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      gc_generic_error_msg TYPE string VALUE `An error occured (ZCX_ABAPGIT_EXCEPTION)` ##NO_TEXT.
ENDCLASS.



CLASS zcx_abapgit_exception IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).

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

  METHOD raise.
    DATA: lv_msgv1    TYPE symsgv,
          lv_msgv2    TYPE symsgv,
          lv_msgv3    TYPE symsgv,
          lv_msgv4    TYPE symsgv,
          ls_t100_key TYPE scx_t100key,
          lv_text     TYPE string.

    IF iv_text IS INITIAL.
      lv_text = gc_generic_error_msg.
    ELSE.
      lv_text = iv_text.
    ENDIF.

    cl_message_helper=>set_msg_vars_for_clike( lv_text ).

    ls_t100_key-msgid = sy-msgid.
    ls_t100_key-msgno = sy-msgno.
    ls_t100_key-attr1 = 'MSGV1'.
    ls_t100_key-attr2 = 'MSGV2'.
    ls_t100_key-attr3 = 'MSGV3'.
    ls_t100_key-attr4 = 'MSGV4'.
    lv_msgv1 = sy-msgv1.
    lv_msgv2 = sy-msgv2.
    lv_msgv3 = sy-msgv3.
    lv_msgv4 = sy-msgv4.

    RAISE EXCEPTION TYPE zcx_abapgit_exception
      EXPORTING
        textid   = ls_t100_key
        msgv1    = lv_msgv1
        msgv2    = lv_msgv2
        msgv3    = lv_msgv3
        msgv4    = lv_msgv4
        previous = ix_previous.
  ENDMETHOD.

  METHOD raise_t100.
    DATA: ls_t100_key TYPE scx_t100key.

    ls_t100_key-msgid = iv_msgid.
    ls_t100_key-msgno = iv_msgno.
    ls_t100_key-attr1 = 'MSGV1'.
    ls_t100_key-attr2 = 'MSGV2'.
    ls_t100_key-attr3 = 'MSGV3'.
    ls_t100_key-attr4 = 'MSGV4'.

    IF iv_msgid IS INITIAL.
      CLEAR ls_t100_key.
    ENDIF.

    RAISE EXCEPTION TYPE zcx_abapgit_exception
      EXPORTING
        textid = ls_t100_key
        msgv1  = iv_msgv1
        msgv2  = iv_msgv2
        msgv3  = iv_msgv3
        msgv4  = iv_msgv4.
  ENDMETHOD.
ENDCLASS.
