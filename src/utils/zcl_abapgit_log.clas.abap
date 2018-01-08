CLASS zcl_abapgit_log DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      add
        IMPORTING
          iv_msg  TYPE csequence
          iv_type TYPE symsgty   DEFAULT 'E'
          iv_rc   TYPE balsort   OPTIONAL,
      count
        RETURNING VALUE(rv_count) TYPE i,
      to_html
        RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html,
      clear,
      has_rc "For unit tests mainly
        IMPORTING iv_rc         TYPE balsort
        RETURNING VALUE(rv_yes) TYPE abap_bool,
      show.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_log,
             msg  TYPE string,
             type TYPE symsgty,
             rc   TYPE balsort,
           END OF ty_log.

    DATA: mt_log TYPE STANDARD TABLE OF ty_log WITH DEFAULT KEY.

ENDCLASS.



CLASS ZCL_ABAPGIT_LOG IMPLEMENTATION.


  METHOD add.

    FIELD-SYMBOLS: <ls_log> LIKE LINE OF mt_log.

    APPEND INITIAL LINE TO mt_log ASSIGNING <ls_log>.
    <ls_log>-msg  = iv_msg.
    <ls_log>-type = iv_type.
    <ls_log>-rc   = iv_rc.

  ENDMETHOD.


  METHOD clear.
    CLEAR mt_log.
  ENDMETHOD.


  METHOD count.
    rv_count = lines( mt_log ).
  ENDMETHOD.


  METHOD has_rc.
    READ TABLE mt_log WITH KEY rc = iv_rc TRANSPORTING NO FIELDS.
    rv_yes = boolc( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD show.
* only supports showing 4 errors, but I guess this is okay
* alternatively refactor to use method TO_HTML instead

    DATA: ls_log1 LIKE LINE OF mt_log,
          ls_log2 LIKE LINE OF mt_log,
          ls_log3 LIKE LINE OF mt_log,
          ls_log4 LIKE LINE OF mt_log.


    READ TABLE mt_log INDEX 1 INTO ls_log1.
    READ TABLE mt_log INDEX 2 INTO ls_log2.
    READ TABLE mt_log INDEX 3 INTO ls_log3.
    READ TABLE mt_log INDEX 4 INTO ls_log4.

    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Log'
        txt1  = ls_log1-msg
        txt2  = ls_log2-msg
        txt3  = ls_log3-msg
        txt4  = ls_log4-msg.

  ENDMETHOD.


  METHOD to_html.

    DATA: lv_class TYPE string,
          lv_icon  TYPE string.

    FIELD-SYMBOLS: <ls_log> LIKE LINE OF mt_log.

    CREATE OBJECT ro_html.

    IF count( ) = 0.
      RETURN.
    ENDIF.

    LOOP AT mt_log ASSIGNING <ls_log>.
      CASE <ls_log>-type.
        WHEN 'W'.
          lv_icon  = 'alert'.
          lv_class = 'warning'.
        WHEN 'E'.
          lv_icon  = 'flame'.
          lv_class = 'error'.
        WHEN OTHERS. " ??? unexpected
          lv_icon  = 'flame'.
          lv_class = 'error'.
      ENDCASE.

      ro_html->add( |<span class="{ lv_class }">| ).
      ro_html->add_icon( iv_name = lv_icon ).
      ro_html->add( <ls_log>-msg ).
      ro_html->add( '</span>' ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
