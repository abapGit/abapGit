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

    METHODS if_message~get_text
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_text TYPE string VALUE `Authentication required`.
ENDCLASS.



CLASS zcx_abapgit_auth_required IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor(
      previous = previous
      log      = log
      msgv1    = msgv1
      msgv2    = msgv2
      msgv3    = msgv3
      msgv4    = msgv4
      longtext = longtext ).

    mv_url = iv_url.

    CLEAR me->textid.

    " Leave the T100 key initial when no text id is supplied, MESSAGE and friends prefer
    " it over IF_MESSAGE~GET_TEXT and the default key would only say "An exception was
    " raised"
    if_t100_message~t100key = textid.
  ENDMETHOD.


  METHOD if_message~get_text.

    " The exception is normally handled by the UI layer which shows the password popup,
    " this is the text for the flows where it ends up as a message instead
    IF mv_url IS INITIAL.
      result = c_text.
    ELSE.
      result = |{ c_text } for { mv_url }|.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
