CLASS zcl_abapgit_log DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_log .

  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_msg,
        text TYPE string,
        type TYPE symsgty,
      END OF ty_msg .
    TYPES:
      BEGIN OF ty_log, "in order of occurrence
        msg       TYPE ty_msg,
        rc        TYPE balsort,
        item      TYPE zif_abapgit_definitions=>ty_item,
        exception TYPE REF TO cx_root,
      END OF ty_log .

    DATA:
      mt_log TYPE STANDARD TABLE OF ty_log WITH DEFAULT KEY .
    DATA mv_title TYPE string .

    METHODS get_messages_status
      IMPORTING
        !it_msg          TYPE zif_abapgit_log=>tty_msg
      RETURNING
        VALUE(rv_status) TYPE symsgty .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_log IMPLEMENTATION.


  METHOD get_messages_status.

    DATA lr_msg TYPE REF TO zif_abapgit_log=>ty_msg.
    rv_status = 'S'.
    LOOP AT it_msg REFERENCE INTO lr_msg.
      CASE lr_msg->type.
        WHEN 'E' OR 'A' OR 'X'.
          rv_status = 'E'. "not okay
          EXIT.
        WHEN 'W'.
          rv_status = 'W'. "maybe
          CONTINUE.
        WHEN 'S' OR 'I'.
          IF rv_status <> 'W'.
            rv_status = 'S'. "okay
          ENDIF.
          CONTINUE.
        WHEN OTHERS. "unknown
          CONTINUE.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_log~add.

    FIELD-SYMBOLS: <ls_log> LIKE LINE OF mt_log.

    APPEND INITIAL LINE TO mt_log ASSIGNING <ls_log>.
    <ls_log>-msg-text  = iv_msg.
    <ls_log>-msg-type  = iv_type.
    <ls_log>-rc        = iv_rc.
    <ls_log>-item      = is_item.
    <ls_log>-exception = ix_exc.

  ENDMETHOD.


  METHOD zif_abapgit_log~add_error.

    zif_abapgit_log~add(
      iv_msg  = iv_msg
      iv_type = 'E'
      is_item = is_item ).

  ENDMETHOD.


  METHOD zif_abapgit_log~add_exception.

    DATA lx_exc TYPE REF TO cx_root.
    DATA lv_msg TYPE string.
    lx_exc ?= ix_exc.
    DO.
      lv_msg = lx_exc->get_text( ).
      zif_abapgit_log~add( iv_msg  = lv_msg
                           iv_type = 'E'
                           is_item = is_item
                           ix_exc  = lx_exc ).
      IF lx_exc->previous IS BOUND.
        lx_exc = lx_exc->previous.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD zif_abapgit_log~add_info.

    zif_abapgit_log~add(
      iv_msg  = iv_msg
      iv_type = 'I'
      is_item = is_item ).

  ENDMETHOD.


  METHOD zif_abapgit_log~add_success.

    zif_abapgit_log~add(
      iv_msg  = iv_msg
      iv_type = 'S'
      is_item = is_item ).

  ENDMETHOD.


  METHOD zif_abapgit_log~add_warning.

    zif_abapgit_log~add(
      iv_msg  = iv_msg
      iv_type = 'W'
      is_item = is_item ).

  ENDMETHOD.


  METHOD zif_abapgit_log~clear.
    CLEAR mt_log.
  ENDMETHOD.


  METHOD zif_abapgit_log~count.
    rv_count = lines( mt_log ).
  ENDMETHOD.


  METHOD zif_abapgit_log~get_item_status.

    DATA lr_log         TYPE REF TO ty_log.
    DATA ls_msg         TYPE zif_abapgit_log=>ty_msg.
    DATA ls_item_status TYPE zif_abapgit_log=>ty_item_status_out.
    DATA lr_item_status TYPE REF TO zif_abapgit_log=>ty_item_status_out.

    CLEAR et_item_status.

    "collect all message for all objects
    LOOP AT mt_log REFERENCE INTO lr_log.
      CLEAR ls_item_status.
      ls_item_status-item = lr_log->item.
      READ TABLE et_item_status REFERENCE INTO lr_item_status
           WITH KEY item-obj_type = ls_item_status-item-obj_type
                    item-obj_name = ls_item_status-item-obj_name.
      IF sy-subrc <> 0.
        INSERT ls_item_status INTO TABLE et_item_status.
        GET REFERENCE OF ls_item_status INTO lr_item_status.
      ENDIF.
      CLEAR ls_msg.
      ls_msg-type = lr_log->msg-type.
      ls_msg-text = lr_log->msg-text.
      INSERT ls_msg INTO TABLE lr_item_status->messages.
    ENDLOOP.

    "determine object status from object messages
    LOOP AT et_item_status REFERENCE INTO lr_item_status.
      lr_item_status->status = get_messages_status( lr_item_status->messages ).
      IF lr_item_status->messages IS INITIAL.
        CLEAR ls_msg.
        ls_msg-type = 'I'.
        ls_msg-text = 'No message'.
        INSERT ls_msg INTO TABLE lr_item_status->messages.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_log~get_messages.
    DATA ls_msg TYPE zif_abapgit_log~ty_log_out.
    FIELD-SYMBOLS <ls_log> TYPE ty_log.
    LOOP AT mt_log ASSIGNING <ls_log>.
      ls_msg-type      = <ls_log>-msg-type.
      ls_msg-text      = <ls_log>-msg-text.
      ls_msg-obj_type  = <ls_log>-item-obj_type.
      ls_msg-obj_name  = <ls_log>-item-obj_name.
      ls_msg-exception = <ls_log>-exception.
      APPEND ls_msg TO rt_msg.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_abapgit_log~get_status.

    DATA lr_log TYPE REF TO ty_log.
    rv_status = 'S'.
    LOOP AT mt_log REFERENCE INTO lr_log.
      CASE lr_log->msg-type.
        WHEN 'E' OR 'A' OR 'X'.
          rv_status = 'E'. "not okay
          EXIT.
        WHEN 'W'.
          rv_status = 'W'. "maybe
          CONTINUE.
        WHEN 'S' OR 'I'.
          IF rv_status <> 'W'.
            rv_status = 'S'. "okay
          ENDIF.
          CONTINUE.
        WHEN OTHERS. "unknown
          CONTINUE.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_log~get_title.
    rv_title = mv_title.
    IF rv_title IS INITIAL.
      rv_title = 'Log'.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_log~has_rc.
* todo, this method is only used in unit tests

    READ TABLE mt_log WITH KEY rc = iv_rc TRANSPORTING NO FIELDS.
    rv_yes = boolc( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD zif_abapgit_log~set_title.
    mv_title = iv_title.
  ENDMETHOD.
ENDCLASS.
