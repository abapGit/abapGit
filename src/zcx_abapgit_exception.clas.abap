"! abapGit general error
CLASS zcx_abapgit_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS: gc_memory_id TYPE char30 VALUE `ZABAPGIT_CALLSTACK`.

    INTERFACES if_t100_message.

    DATA subrc TYPE sysubrc READ-ONLY.
    DATA msgv1 TYPE symsgv READ-ONLY.
    DATA msgv2 TYPE symsgv READ-ONLY.
    DATA msgv3 TYPE symsgv READ-ONLY.
    DATA msgv4 TYPE symsgv READ-ONLY.
    DATA mt_callstack TYPE abap_callstack READ-ONLY.

    "! Raise exception with text
    "! @parameter iv_text | Text
    "! @parameter ix_previous | Previous exception
    "! @raising zcx_abapgit_exception | Exception
    CLASS-METHODS raise
      IMPORTING
        !iv_text     TYPE clike
        !ix_previous TYPE REF TO cx_root OPTIONAL
      RAISING
        zcx_abapgit_exception.
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
    CLASS-METHODS raise_t100
      IMPORTING
        VALUE(iv_msgid) TYPE symsgid DEFAULT sy-msgid
        VALUE(iv_msgno) TYPE symsgno DEFAULT sy-msgno
        VALUE(iv_msgv1) TYPE symsgv DEFAULT sy-msgv1
        VALUE(iv_msgv2) TYPE symsgv DEFAULT sy-msgv2
        VALUE(iv_msgv3) TYPE symsgv DEFAULT sy-msgv3
        VALUE(iv_msgv4) TYPE symsgv DEFAULT sy-msgv4
      RAISING
        zcx_abapgit_exception.
    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msgv1    TYPE symsgv OPTIONAL
        !msgv2    TYPE symsgv OPTIONAL
        !msgv3    TYPE symsgv OPTIONAL
        !msgv4    TYPE symsgv OPTIONAL.
    METHODS get_longtext REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      gc_generic_error_msg TYPE string VALUE `An error occured (ZCX_ABAPGIT_EXCEPTION)` ##NO_TEXT.
    CLASS-DATA gv_is_merged TYPE abap_bool VALUE abap_undefined ##NO_TEXT.

    CLASS-METHODS:
      set_msg_vars_for_clike
        IMPORTING
          text TYPE string,
      is_merged
        RETURNING
          VALUE(rv_is_merged) TYPE abap_bool.

    METHODS:
      save_callstack,
      set_single_msg_var
        IMPORTING
          iv_arg    TYPE clike
        EXPORTING
          ev_target TYPE c,

      get_t100_longtext_itf
        RETURNING
          VALUE(rt_itf) TYPE tline_tab,

      set_single_msg_var_clike
        IMPORTING
          iv_arg    TYPE clike
        EXPORTING
          ev_target TYPE c,

      set_single_msg_var_numeric
        IMPORTING
          iv_arg    TYPE numeric
        EXPORTING
          ev_target TYPE c,

      set_single_msg_var_xseq
        IMPORTING
          iv_arg    TYPE xsequence
        EXPORTING
          ev_target TYPE c,

      itf_to_string
        IMPORTING
          it_itf           TYPE tline_tab
        RETURNING
          VALUE(rv_result) TYPE string.

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

    save_callstack( ).

  ENDMETHOD.


  METHOD get_t100_longtext_itf.

    "cl_message_helper=>get_t100_longtext_itf

    DATA: lv_docu_key TYPE doku_obj,
          ls_itf      LIKE LINE OF rt_itf.

    lv_docu_key = if_t100_message~t100key-msgid && if_t100_message~t100key-msgno.

    CALL FUNCTION 'DOCU_GET'
      EXPORTING
        id     = 'NA'
        langu  = sy-langu
        object = lv_docu_key
        typ    = 'E'
      TABLES
        line   = rt_itf
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc = 0.
      set_single_msg_var(
        EXPORTING
          iv_arg    = if_t100_message~t100key-attr1
        IMPORTING
          ev_target = sy-msgv1 ).

      REPLACE '&V1&' IN TABLE rt_itf
                     WITH sy-msgv1.

      set_single_msg_var(
       EXPORTING
         iv_arg    = if_t100_message~t100key-attr2
       IMPORTING
         ev_target = sy-msgv2 ).

      REPLACE '&V2&' IN TABLE rt_itf
                     WITH sy-msgv2.

      set_single_msg_var(
       EXPORTING
         iv_arg    = if_t100_message~t100key-attr3
       IMPORTING
         ev_target = sy-msgv3 ).

      REPLACE '&V3&' IN TABLE rt_itf
                     WITH sy-msgv3.

      set_single_msg_var(
       EXPORTING
         iv_arg    = if_t100_message~t100key-attr4
       IMPORTING
         ev_target = sy-msgv4 ).

      REPLACE '&V4&' IN TABLE rt_itf
                     WITH sy-msgv4.
    ENDIF.

    ls_itf-tdformat = '*'.
    ls_itf-tdline   = |T100-Message: { if_t100_message~t100key-msgid } { if_t100_message~t100key-msgno } |.
    INSERT ls_itf INTO rt_itf INDEX 1.

  ENDMETHOD.


  METHOD get_longtext.

    result = super->get_longtext( ).

    IF if_t100_message~t100key IS NOT INITIAL.
      result = itf_to_string( get_t100_longtext_itf( ) ).
    ENDIF.

  ENDMETHOD.


  METHOD is_merged.

    " Temporal duplication of zcl_abapgit_environment=>is_merged,
    " it doesn't work yet inside exception classes. #2763

    DATA lo_marker TYPE REF TO data ##NEEDED.

    IF gv_is_merged = abap_undefined.
      TRY.
          CREATE DATA lo_marker TYPE REF TO ('LIF_ABAPMERGE_MARKER')  ##no_text.
          "No exception --> marker found
          gv_is_merged = abap_true.

        CATCH cx_sy_create_data_error.
          gv_is_merged = abap_false.
      ENDTRY.
    ENDIF.

    rv_is_merged = gv_is_merged.

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

    set_msg_vars_for_clike( lv_text ).

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


  METHOD save_callstack.

    DATA: li_gui_functions TYPE REF TO zif_abapgit_gui_functions.

    FIELD-SYMBOLS: <ls_callstack> TYPE abap_callstack_line.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = mt_callstack.

    " You should remember that the first lines are from zcx_abapgit_exception
    " and are removed so that highest level in the callstack is the position where
    " the exception is raised.
    LOOP AT mt_callstack ASSIGNING <ls_callstack>.

      IF <ls_callstack>-mainprogram CP |ZCX_ABAPGIT_EXCEPTION*|.
        DELETE TABLE mt_callstack FROM <ls_callstack>.
      ELSE.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_msg_vars_for_clike.

    TYPES:
      BEGIN OF ty_msg,
        msgv1 TYPE symsgv,
        msgv2 TYPE symsgv,
        msgv3 TYPE symsgv,
        msgv4 TYPE symsgv,
      END OF ty_msg.

    DATA: ls_msg   TYPE ty_msg,
          lv_dummy TYPE string.

    ls_msg = text.

    " &1&2&3&4&5&6&7&8
    MESSAGE e001(00) WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4
                     INTO lv_dummy.

  ENDMETHOD.


  METHOD set_single_msg_var.

    FIELD-SYMBOLS <lv_arg> TYPE any.

    CLEAR ev_target.

    IF iv_arg IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        ASSIGN me->(iv_arg) TO <lv_arg>.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_sy_assign_cast_illegal_cast.
        ENDIF.
        " We cannot catch all conversion exceptions on MOVE => use CALL
        set_single_msg_var_clike(
          EXPORTING
            iv_arg    = <lv_arg>
          IMPORTING
            ev_target = ev_target ).

      CATCH cx_sy_dyn_call_illegal_type.
        TRY.
            set_single_msg_var_numeric(
              EXPORTING
                iv_arg    = <lv_arg>
              IMPORTING
                ev_target = ev_target ).

          CATCH cx_sy_dyn_call_illegal_type.
            TRY.
                set_single_msg_var_xseq(
                  EXPORTING
                    iv_arg    = <lv_arg>
                  IMPORTING
                    ev_target = ev_target ).

              CATCH cx_sy_dyn_call_illegal_type.
                CONCATENATE '&' iv_arg '&' INTO ev_target.
            ENDTRY.
        ENDTRY.

      CATCH cx_sy_assign_cast_illegal_cast.
        CONCATENATE '&' iv_arg '&' INTO ev_target.
    ENDTRY.

  ENDMETHOD.


  METHOD set_single_msg_var_clike.
    " a kind of MOVE where all conversion errors are signalled by exceptions
    WRITE iv_arg LEFT-JUSTIFIED TO ev_target.
  ENDMETHOD.


  METHOD set_single_msg_var_numeric.
    " a kind of MOVE where all conversion errors are signalled by exceptions
    WRITE iv_arg LEFT-JUSTIFIED TO ev_target.
  ENDMETHOD.


  METHOD set_single_msg_var_xseq.
    " a kind of MOVE where all conversion errors are signalled by exceptions
    WRITE iv_arg LEFT-JUSTIFIED TO ev_target.
  ENDMETHOD.


  METHOD itf_to_string.

    DATA:
      lt_stream          TYPE TABLE OF tdline,
      lt_string          TYPE TABLE OF string,
      ls_string          LIKE LINE OF lt_string,
      lt_itf             TYPE tline_tab,
      lv_has_content     TYPE abap_bool,
      lv_save_tabix_from TYPE syst-tabix,
      lv_save_tabix_to   TYPE i.

    FIELD-SYMBOLS: <ls_itf>  TYPE tline,
                   <ls_itf2> TYPE tline.

    lt_itf = it_itf.

    " You should remember that we replace the U1 format because
    " that preserves the section header of longtexts.
    LOOP AT lt_itf ASSIGNING <ls_itf>
                   WHERE tdformat = 'U1'.

      CLEAR:
        lv_has_content,
        lv_save_tabix_to.

      lv_save_tabix_from = sy-tabix.

      LOOP AT lt_itf ASSIGNING <ls_itf2>
                     FROM sy-tabix + 1.

        IF <ls_itf2>-tdformat = 'U1'.
          lv_save_tabix_to = sy-tabix.
          EXIT.
        ELSEIF <ls_itf2>-tdline IS NOT INITIAL.
          lv_has_content = abap_true.
        ENDIF.

      ENDLOOP.

      IF lv_has_content = abap_false.
        DELETE lt_itf FROM lv_save_tabix_from TO lv_save_tabix_to.
        CONTINUE.
      ENDIF.

      CASE <ls_itf>-tdline.
        WHEN '&CAUSE&'.
          <ls_itf>-tdline = 'Cause'.
        WHEN '&SYSTEM_RESPONSE&'.
          <ls_itf>-tdline = 'System response'.
        WHEN '&WHAT_TO_DO&'.
          <ls_itf>-tdline = 'Procedure'.
        WHEN '&SYS_ADMIN&'.
          <ls_itf>-tdline = 'System administration'.
      ENDCASE.

    ENDLOOP.

    CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
      EXPORTING
        lf           = 'X'
      IMPORTING
        stream_lines = lt_string
      TABLES
        itf_text     = lt_itf
        text_stream  = lt_stream.

    LOOP AT lt_string INTO ls_string.
      IF sy-tabix = 1.
        rv_result = ls_string.
      ELSE.
        CONCATENATE rv_result ls_string
                    INTO rv_result
                    SEPARATED BY cl_abap_char_utilities=>newline.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
