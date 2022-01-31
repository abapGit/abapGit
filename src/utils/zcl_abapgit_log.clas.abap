CLASS zcl_abapgit_log DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_log .

    METHODS constructor
      IMPORTING
        iv_title TYPE string OPTIONAL.

    CLASS-METHODS from_exception
      IMPORTING
        io_x TYPE REF TO cx_root
      RETURNING
        VALUE(ro_log) TYPE REF TO zcl_abapgit_log.

  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_log, "in order of occurrence
        msg       TYPE zif_abapgit_log=>ty_msg,
        item      TYPE zif_abapgit_definitions=>ty_item,
        exception TYPE REF TO cx_root,
      END OF ty_log .

    DATA:
      mt_log TYPE STANDARD TABLE OF ty_log WITH DEFAULT KEY .
    DATA mv_title TYPE string .

    METHODS get_messages_status
      IMPORTING
        !it_msg          TYPE zif_abapgit_log=>ty_msgs
      RETURNING
        VALUE(rv_status) TYPE sy-msgty .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_log IMPLEMENTATION.


  METHOD constructor.

    zif_abapgit_log~set_title( iv_title ).

  ENDMETHOD.


  METHOD from_exception.

    CREATE OBJECT ro_log.

    IF io_x IS BOUND.
      ro_log->zif_abapgit_log~add_exception( io_x ).
    ENDIF.

  ENDMETHOD.


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
    <ls_log>-item      = is_item.
    <ls_log>-exception = ix_exc.

    CASE iv_type.
      WHEN 'E' OR 'A' OR 'X'.
        <ls_log>-msg-level = zif_abapgit_log=>c_log_level-error.
      WHEN 'W'.
        <ls_log>-msg-level = zif_abapgit_log=>c_log_level-warning.
      WHEN 'S' OR 'I'.
        <ls_log>-msg-level = zif_abapgit_log=>c_log_level-info.
      WHEN OTHERS. "unknown
        ASSERT 0 = 1.
    ENDCASE.

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
    lx_exc = ix_exc.
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


  METHOD zif_abapgit_log~clone.

    DATA lo_log TYPE REF TO zcl_abapgit_log.

    CREATE OBJECT lo_log EXPORTING iv_title = mv_title.
    lo_log->mt_log = mt_log.
    ri_log = lo_log.

  ENDMETHOD.


  METHOD zif_abapgit_log~count.
    rv_count = lines( mt_log ).
  ENDMETHOD.


  METHOD zif_abapgit_log~get_item_status.

    DATA lr_log         TYPE REF TO ty_log.
    DATA ls_msg         TYPE zif_abapgit_log=>ty_msg.
    DATA ls_item_status TYPE zif_abapgit_log=>ty_item_status_out.
    DATA lr_item_status TYPE REF TO zif_abapgit_log=>ty_item_status_out.

    "collect all message for all objects
    LOOP AT mt_log REFERENCE INTO lr_log.
      CLEAR ls_item_status.
      ls_item_status-item = lr_log->item.
      READ TABLE rt_item_status REFERENCE INTO lr_item_status
           WITH KEY item-obj_type = ls_item_status-item-obj_type
                    item-obj_name = ls_item_status-item-obj_name.
      IF sy-subrc <> 0.
        INSERT ls_item_status INTO TABLE rt_item_status.
        GET REFERENCE OF ls_item_status INTO lr_item_status.
      ENDIF.
      CLEAR ls_msg.
      ls_msg-type = lr_log->msg-type.
      ls_msg-text = lr_log->msg-text.
      INSERT ls_msg INTO TABLE lr_item_status->messages.
    ENDLOOP.

    "determine object status from object messages
    LOOP AT rt_item_status REFERENCE INTO lr_item_status.
      lr_item_status->status = get_messages_status( lr_item_status->messages ).
      IF lr_item_status->messages IS INITIAL.
        CLEAR ls_msg.
        ls_msg-type = 'I'.
        ls_msg-text = 'No message'.
        INSERT ls_msg INTO TABLE lr_item_status->messages.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_log~get_log_level.

    FIELD-SYMBOLS <ls_log> LIKE LINE OF mt_log.

    rv_level = zif_abapgit_log=>c_log_level-empty.

    LOOP AT mt_log ASSIGNING <ls_log>.
      IF <ls_log>-msg-level = zif_abapgit_log=>c_log_level-error.
        rv_level = zif_abapgit_log=>c_log_level-error.
        EXIT.
      ELSEIF <ls_log>-msg-level > rv_level.
        rv_level = <ls_log>-msg-level.
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
    DELETE ADJACENT DUPLICATES FROM rt_msg.
  ENDMETHOD.


  METHOD zif_abapgit_log~get_status.

    DATA lr_log TYPE REF TO ty_log.
    rv_status = zif_abapgit_log=>c_status-ok.
    LOOP AT mt_log REFERENCE INTO lr_log.
      CASE lr_log->msg-type.
        WHEN 'E' OR 'A' OR 'X'.
          rv_status = zif_abapgit_log=>c_status-error.
          EXIT.
        WHEN 'W'.
          rv_status = zif_abapgit_log=>c_status-warning.
          CONTINUE.
        WHEN 'S' OR 'I'.
          IF rv_status <> zif_abapgit_log=>c_status-warning.
            rv_status = zif_abapgit_log=>c_status-ok.
          ENDIF.
          CONTINUE.
        WHEN OTHERS. "unknown
          ASSERT 0 = 1.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_log~get_title.
    rv_title = mv_title.
    IF rv_title IS INITIAL.
      rv_title = 'Log'.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_log~merge_with.

    DATA lo_log TYPE REF TO zcl_abapgit_log.
    DATA lt_log_temp LIKE lo_log->mt_log.

    IF ii_log IS BOUND.
      lo_log ?= ii_log.
      IF iv_min_level > 0.
        lt_log_temp = lo_log->mt_log.
        DELETE lt_log_temp WHERE msg-level < iv_min_level.
        APPEND LINES OF lt_log_temp TO mt_log.
      ELSE.
        APPEND LINES OF lo_log->mt_log TO mt_log.
      ENDIF.
    ENDIF.

    ri_log = me.

  ENDMETHOD.


  METHOD zif_abapgit_log~set_title.
    mv_title = iv_title.
    ri_log = me.
  ENDMETHOD.
ENDCLASS.
