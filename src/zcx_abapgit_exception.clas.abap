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
                 RAISING   zcx_abapgit_exception.
    METHODS:
      "! @parameter is_textid | Textid
      "! @parameter ix_previous | Previous exception
      constructor IMPORTING is_textid   LIKE if_t100_message=>t100key OPTIONAL
                            ix_previous LIKE previous OPTIONAL
                            iv_text     TYPE clike OPTIONAL
                            iv_subrc    TYPE syst_subrc OPTIONAL
                            iv_msgv1    TYPE syst_msgv OPTIONAL
                            iv_msgv2    TYPE syst_msgv OPTIONAL
                            iv_msgv3    TYPE syst_msgv OPTIONAL
                            iv_msgv4    TYPE syst_msgv OPTIONAL,
      if_message~get_text REDEFINITION.
    DATA:
      mv_text  TYPE string READ-ONLY,
      mv_subrc TYPE syst_subrc READ-ONLY,
      mv_msgv1 TYPE syst_msgv READ-ONLY,
      mv_msgv2 TYPE syst_msgv READ-ONLY,
      mv_msgv3 TYPE syst_msgv READ-ONLY,
      mv_msgv4 TYPE syst_msgv READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      gc_generic_error_msg TYPE string VALUE `An error occured (ZCX_ABAPGIT_EXCEPTION)` ##NO_TEXT.
ENDCLASS.



CLASS zcx_abapgit_exception IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = ix_previous ).

    mv_text = iv_text.
    mv_subrc = iv_subrc.
    mv_msgv1 = iv_msgv1.
    mv_msgv2 = iv_msgv2.
    mv_msgv3 = iv_msgv3.
    mv_msgv4 = iv_msgv4.

    IF is_textid-msgid IS NOT INITIAL AND is_textid-msgno IS NOT INITIAL.
      CLEAR textid.
      if_t100_message~t100key = is_textid.
    ENDIF.
  ENDMETHOD.


  METHOD if_message~get_text.
    " The standard implementation of this method always uses T100 messages, if IF_T100_MESSAGE is
    " implemented. Since this is a 'hybrid' exception of IF_MESSAGE and IF_T100_MESSAGE the normal
    " get_text() is only called if a T100 message is used.
    " Otherwise try to get a meaningful error text for the user in this order:
    " mv_text variable, previous exception's text, generic error message

    IF if_t100_message~t100key IS INITIAL.
      IF mv_text IS NOT INITIAL.
        result = mv_text.
      ELSEIF previous IS NOT INITIAL.
        result = previous->get_text( ).
      ELSE.
        IF sy-subrc IS NOT INITIAL.
          result = |{ gc_generic_error_msg } ({ mv_subrc })|.
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
        iv_text = iv_text.
  ENDMETHOD.


  METHOD raise_t100.
    DATA: ls_t100_key TYPE scx_t100key,
          lv_subrc    TYPE syst_subrc.

    " If this method is called right in the sy-subrc handling of a method or function module
    " sy-subrc should still contain the id from that.
    lv_subrc = sy-subrc.

    ls_t100_key-msgid = iv_msgid.
    ls_t100_key-msgno = iv_msgno.
    ls_t100_key-attr1 = 'MV_MSGV1'.
    ls_t100_key-attr2 = 'MV_MSGV2'.
    ls_t100_key-attr3 = 'MV_MSGV3'.
    ls_t100_key-attr4 = 'MV_MSGV4'.

    RAISE EXCEPTION TYPE zcx_abapgit_exception
      EXPORTING
        is_textid = ls_t100_key
        iv_msgv1  = iv_msgv1
        iv_msgv2  = iv_msgv2
        iv_msgv3  = iv_msgv3
        iv_msgv4  = iv_msgv4
        iv_subrc  = lv_subrc.
  ENDMETHOD.
ENDCLASS.
