CLASS zcl_abapgit_message_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF gc_section_text,
        cause           TYPE string VALUE `Cause`,
        system_response TYPE string VALUE `System response`,
        what_to_do      TYPE string VALUE `Procedure`,
        sys_admin       TYPE string VALUE `System administration`,
      END OF gc_section_text .
    CONSTANTS:
      BEGIN OF gc_section_token,
        cause           TYPE string VALUE `&CAUSE&`,
        system_response TYPE string VALUE `&SYSTEM_RESPONSE&`,
        what_to_do      TYPE string VALUE `&WHAT_TO_DO&`,
        sys_admin       TYPE string VALUE `&SYS_ADMIN&`,
      END OF gc_section_token .

    CLASS-METHODS set_msg_vars_for_clike
      IMPORTING
        !iv_text TYPE string .
    METHODS constructor
      IMPORTING
        !ii_t100_message TYPE REF TO if_t100_message .
    METHODS get_t100_longtext
      RETURNING
        VALUE(rv_longtext) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_msg,
        msgv1 TYPE symsgv,
        msgv2 TYPE symsgv,
        msgv3 TYPE symsgv,
        msgv4 TYPE symsgv,
      END OF ty_msg.

    CLASS-METHODS:
      split_text
        IMPORTING
          iv_text       TYPE string
        RETURNING
          VALUE(rs_msg) TYPE ty_msg.

    DATA:
      mi_t100_message TYPE REF TO if_t100_message.

    METHODS:
      itf_to_string
        IMPORTING
          it_itf           TYPE tline_tab
        RETURNING
          VALUE(rv_result) TYPE string,

      get_t100_longtext_itf
        RETURNING
          VALUE(rt_itf) TYPE tline_tab,

      remove_empty_section
        IMPORTING
          iv_tabix_from TYPE i
          iv_tabix_to   TYPE i
        CHANGING
          ct_itf        TYPE tline_tab,

      replace_section_head_with_text
        CHANGING
          cs_itf TYPE tline,

      set_single_msg_var
        IMPORTING
          iv_arg           TYPE clike
        RETURNING
          VALUE(rv_target) TYPE char01,

      set_single_msg_var_clike
        IMPORTING
          iv_arg           TYPE clike
        RETURNING
          VALUE(rv_target) TYPE char01,

      set_single_msg_var_numeric
        IMPORTING
          iv_arg           TYPE numeric
        RETURNING
          VALUE(rv_target) TYPE char01,

      set_single_msg_var_xseq
        IMPORTING
          iv_arg           TYPE xsequence
        RETURNING
          VALUE(rv_target) TYPE char01.

ENDCLASS.



CLASS ZCL_ABAPGIT_MESSAGE_HELPER IMPLEMENTATION.


  METHOD constructor.

    mi_t100_message = ii_t100_message.

  ENDMETHOD.


  METHOD get_t100_longtext.

    rv_longtext = itf_to_string( get_t100_longtext_itf( ) ).

  ENDMETHOD.


  METHOD get_t100_longtext_itf.

    DATA: lv_docu_key TYPE doku_obj.

    lv_docu_key = mi_t100_message->t100key-msgid && mi_t100_message->t100key-msgno.

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
      sy-msgv1 = set_single_msg_var( iv_arg = mi_t100_message->t100key-attr1 ).

      REPLACE '&V1&' IN TABLE rt_itf
                     WITH sy-msgv1.

      sy-msgv2 = set_single_msg_var( iv_arg = mi_t100_message->t100key-attr2 ).

      REPLACE '&V2&' IN TABLE rt_itf
                     WITH sy-msgv2.

      sy-msgv3 = set_single_msg_var( iv_arg = mi_t100_message->t100key-attr3 ).

      REPLACE '&V3&' IN TABLE rt_itf
                     WITH sy-msgv3.

      sy-msgv4 = set_single_msg_var( iv_arg = mi_t100_message->t100key-attr4 ).

      REPLACE '&V4&' IN TABLE rt_itf
                     WITH sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD itf_to_string.

    CONSTANTS: lc_format_section TYPE string VALUE 'U1' ##NO_TEXT.

    DATA:
      lt_stream      TYPE TABLE OF tdline,
      lt_string      TYPE TABLE OF string,
      lv_string      LIKE LINE OF lt_string,
      lt_itf         TYPE tline_tab,
      lv_has_content TYPE abap_bool,
      lv_tabix_from  TYPE syst-tabix,
      lv_tabix_to    TYPE syst-tabix.

    FIELD-SYMBOLS: <ls_itf_section>      TYPE tline,
                   <ls_itf_section_item> TYPE tline.

    lt_itf = it_itf.

    " You should remember that we replace the U1 format because
    " that preserves the section header of longtexts.
    LOOP AT lt_itf ASSIGNING <ls_itf_section>
                   WHERE tdformat = lc_format_section.

      CLEAR:
        lv_has_content,
        lv_tabix_to.

      lv_tabix_from = sy-tabix.

      LOOP AT lt_itf ASSIGNING <ls_itf_section_item>
                     FROM sy-tabix + 1.

        IF <ls_itf_section_item>-tdformat = lc_format_section.
          lv_tabix_to = sy-tabix.
          EXIT.
        ELSEIF <ls_itf_section_item>-tdline IS NOT INITIAL.
          lv_has_content = abap_true.
        ENDIF.

      ENDLOOP.

      IF lv_has_content = abap_false.
        remove_empty_section(
          EXPORTING
            iv_tabix_from = lv_tabix_from
            iv_tabix_to   = lv_tabix_to
          CHANGING
            ct_itf        = lt_itf ).
        CONTINUE.
      ENDIF.

      replace_section_head_with_text( CHANGING cs_itf = <ls_itf_section> ).

    ENDLOOP.

    CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
      EXPORTING
        lf           = 'X'
      IMPORTING
        stream_lines = lt_string
      TABLES
        itf_text     = lt_itf
        text_stream  = lt_stream.

    LOOP AT lt_string INTO lv_string.
      IF sy-tabix = 1.
        rv_result = lv_string.
      ELSE.
        CONCATENATE rv_result lv_string
                    INTO rv_result
                    SEPARATED BY cl_abap_char_utilities=>newline.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD remove_empty_section.
    DELETE ct_itf FROM iv_tabix_from TO iv_tabix_to.
  ENDMETHOD.


  METHOD replace_section_head_with_text.

    CASE cs_itf-tdline.
      WHEN gc_section_token-cause.
        cs_itf-tdline = gc_section_text-cause.
      WHEN gc_section_token-system_response.
        cs_itf-tdline = gc_section_text-system_response.
      WHEN gc_section_token-what_to_do.
        cs_itf-tdline = gc_section_text-what_to_do.
      WHEN gc_section_token-sys_admin.
        cs_itf-tdline = gc_section_text-sys_admin.
    ENDCASE.

  ENDMETHOD.


  METHOD set_msg_vars_for_clike.

    DATA: ls_msg   TYPE ty_msg,
          lv_dummy TYPE string.

    ls_msg = split_text( iv_text ).

    MESSAGE e001(00) WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4
                     INTO lv_dummy.

  ENDMETHOD.


  METHOD set_single_msg_var.

    FIELD-SYMBOLS <lg_arg> TYPE any.

    IF iv_arg IS INITIAL.
      RETURN.
    ENDIF.

    ASSIGN me->(iv_arg) TO <lg_arg>.
    IF sy-subrc <> 0.
      CONCATENATE '&' iv_arg '&' INTO rv_target.
      RETURN.
    ENDIF.

    TRY.
        rv_target = set_single_msg_var_clike( iv_arg = <lg_arg> ).

        RETURN.

      CATCH cx_sy_dyn_call_illegal_type ##no_handler.
    ENDTRY.

    TRY.
        rv_target = set_single_msg_var_numeric( iv_arg = <lg_arg> ).

        RETURN.

      CATCH cx_sy_dyn_call_illegal_type ##no_handler.
    ENDTRY.

    TRY.
        rv_target = set_single_msg_var_xseq( iv_arg = <lg_arg> ).

        RETURN.

      CATCH cx_sy_dyn_call_illegal_type ##no_handler.
    ENDTRY.

    CONCATENATE '&' iv_arg '&' INTO rv_target.

  ENDMETHOD.


  METHOD set_single_msg_var_clike.
    " a kind of MOVE where all conversion errors are signalled by exceptions
    WRITE iv_arg LEFT-JUSTIFIED TO rv_target.
  ENDMETHOD.


  METHOD set_single_msg_var_numeric.
    " a kind of MOVE where all conversion errors are signalled by exceptions
    WRITE iv_arg LEFT-JUSTIFIED TO rv_target.
  ENDMETHOD.


  METHOD set_single_msg_var_xseq.
    " a kind of MOVE where all conversion errors are signalled by exceptions
    WRITE iv_arg LEFT-JUSTIFIED TO rv_target.
  ENDMETHOD.


  METHOD split_text.

    CONSTANTS:
      lc_length_of_msgv           TYPE i VALUE 50,
      lc_offset_of_last_character TYPE i VALUE 49.

    TYPES:
      ty_char200 TYPE c LENGTH 200.

    DATA:
      lv_text    TYPE ty_char200,
      lv_msg_var TYPE c LENGTH lc_length_of_msgv,
      lv_rest    TYPE ty_char200,
      lv_index   TYPE syst-index.

    lv_text = iv_text.

    DO 4 TIMES.

      lv_index = sy-index.

      CALL FUNCTION 'TEXT_SPLIT'
        EXPORTING
          length = lc_length_of_msgv
          text   = lv_text
        IMPORTING
          line   = lv_msg_var
          rest   = lv_rest.

      IF lv_msg_var+lc_offset_of_last_character(1) = space OR
         lv_text+lc_length_of_msgv(1) = space.
        " keep the space at the beginning of the rest
        " because otherwise it's lost
        lv_rest = | { lv_rest }|.
      ENDIF.

      lv_text = lv_rest.

      CASE lv_index.
        WHEN 1.
          rs_msg-msgv1 = lv_msg_var.
        WHEN 2.
          rs_msg-msgv2 = lv_msg_var.
        WHEN 3.
          rs_msg-msgv3 = lv_msg_var.
        WHEN 4.
          rs_msg-msgv4 = lv_msg_var.
      ENDCASE.

    ENDDO.

  ENDMETHOD.
ENDCLASS.
