CLASS zcx_abapgit_auth_required DEFINITION
  PUBLIC
  INHERITING FROM zcx_abapgit_exception
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    DATA mv_url TYPE string READ-ONLY.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !log      TYPE REF TO zif_abapgit_log OPTIONAL
        !msgv1    TYPE symsgv OPTIONAL
        !msgv2    TYPE symsgv OPTIONAL
        !msgv3    TYPE symsgv OPTIONAL
        !msgv4    TYPE symsgv OPTIONAL
        !longtext TYPE csequence OPTIONAL
        !iv_url   TYPE string OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_text TYPE string VALUE `Authentication required`.
ENDCLASS.



CLASS zcx_abapgit_auth_required IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    DATA ls_textid TYPE scx_t100key.
    DATA lv_msgv1  TYPE symsgv.
    DATA lv_msgv2  TYPE symsgv.
    DATA lv_msgv3  TYPE symsgv.
    DATA lv_msgv4  TYPE symsgv.

    ls_textid = textid.
    lv_msgv1  = msgv1.
    lv_msgv2  = msgv2.
    lv_msgv3  = msgv3.
    lv_msgv4  = msgv4.

    IF ls_textid IS INITIAL.
      " MESSAGE takes the text from the T100 key, IF_MESSAGE~GET_TEXT is only used for
      " exceptions without IF_T100_MESSAGE. So build a key for the text, same approach
      " as ZCX_ABAPGIT_EXCEPTION=>RAISE
      IF iv_url IS INITIAL.
        cl_message_helper=>set_msg_vars_for_clike( c_text ).
      ELSE.
        cl_message_helper=>set_msg_vars_for_clike( |{ c_text } for { iv_url }| ).
      ENDIF.
      ls_textid-msgid = sy-msgid.
      ls_textid-msgno = sy-msgno.
      ls_textid-attr1 = 'MSGV1'.
      ls_textid-attr2 = 'MSGV2'.
      ls_textid-attr3 = 'MSGV3'.
      ls_textid-attr4 = 'MSGV4'.
      lv_msgv1 = sy-msgv1.
      lv_msgv2 = sy-msgv2.
      lv_msgv3 = sy-msgv3.
      lv_msgv4 = sy-msgv4.
    ENDIF.

    super->constructor(
      previous = previous
      log      = log
      msgv1    = lv_msgv1
      msgv2    = lv_msgv2
      msgv3    = lv_msgv3
      msgv4    = lv_msgv4
      longtext = longtext ).

    mv_url = iv_url.

    CLEAR me->textid.

    if_t100_message~t100key = ls_textid.

  ENDMETHOD.
ENDCLASS.
