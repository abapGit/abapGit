*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_UTIL
*&---------------------------------------------------------------------*

CLASS lcl_login_manager DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      load
        IMPORTING iv_uri                  TYPE string
                  ii_client               TYPE REF TO if_http_client OPTIONAL
        RETURNING VALUE(rv_authorization) TYPE string
        RAISING   zcx_abapgit_exception,
      save
        IMPORTING iv_uri    TYPE string
                  ii_client TYPE REF TO if_http_client
        RAISING   zcx_abapgit_exception,
      clear,
      set
        IMPORTING iv_uri         TYPE string
                  iv_username    TYPE string
                  iv_password    TYPE string
        RETURNING VALUE(rv_auth) TYPE string
        RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_auth,
             uri           TYPE string,
             authorization TYPE string,
           END OF ty_auth.

    CLASS-DATA: gt_auth TYPE TABLE OF ty_auth WITH DEFAULT KEY.

    CLASS-METHODS:
      append
        IMPORTING iv_uri  TYPE string
                  iv_auth TYPE string
        RAISING   zcx_abapgit_exception.

ENDCLASS.

CLASS lcl_login_manager IMPLEMENTATION.

  METHOD clear.
    CLEAR gt_auth.
  ENDMETHOD.

  METHOD set.

    DATA: lv_concat TYPE string.


    ASSERT NOT iv_uri IS INITIAL.

    IF iv_username IS INITIAL OR iv_password IS INITIAL.
      RETURN.
    ENDIF.

    CONCATENATE iv_username ':' iv_password INTO lv_concat.

    rv_auth = cl_http_utility=>if_http_utility~encode_base64( lv_concat ).

    CONCATENATE 'Basic' rv_auth INTO rv_auth
      SEPARATED BY space ##NO_TEXT.

    append( iv_uri  = iv_uri
            iv_auth = rv_auth ).

  ENDMETHOD.

  METHOD load.

    DATA: ls_auth LIKE LINE OF gt_auth.


    READ TABLE gt_auth INTO ls_auth WITH KEY uri = zcl_abapgit_url=>host( iv_uri ).
    IF sy-subrc = 0.
      rv_authorization = ls_auth-authorization.

      IF NOT ii_client IS INITIAL.
        ii_client->request->set_header_field(
          name  = 'authorization'
          value = ls_auth-authorization ).                  "#EC NOTEXT
        ii_client->propertytype_logon_popup = ii_client->co_disabled.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD save.

    DATA: lv_auth TYPE string.


    lv_auth = ii_client->request->get_header_field( 'authorization' ). "#EC NOTEXT

    IF NOT lv_auth IS INITIAL.
      append( iv_uri  = iv_uri
              iv_auth = lv_auth ).
    ENDIF.

  ENDMETHOD.

  METHOD append.

    FIELD-SYMBOLS: <ls_auth> LIKE LINE OF gt_auth.


    READ TABLE gt_auth WITH KEY uri = zcl_abapgit_url=>host( iv_uri )
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO gt_auth ASSIGNING <ls_auth>.
      <ls_auth>-uri           = zcl_abapgit_url=>host( iv_uri ).
      <ls_auth>-authorization = iv_auth.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_progress DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      show
        IMPORTING
          iv_key            TYPE string
          VALUE(iv_current) TYPE i
          iv_total          TYPE i
          iv_text           TYPE csequence.

  PRIVATE SECTION.
    CLASS-METHODS:
      calc_pct
        IMPORTING iv_current    TYPE i
                  iv_total      TYPE i
        RETURNING VALUE(rv_pct) TYPE i.

ENDCLASS.

CLASS lcl_progress IMPLEMENTATION.

  METHOD show.

    DATA: lv_pct  TYPE i,
          lv_text TYPE string.


    lv_pct = calc_pct( iv_current = iv_current
                       iv_total   = iv_total ).
    CONCATENATE iv_key '-' iv_text INTO lv_text SEPARATED BY space.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_pct
        text       = lv_text.

  ENDMETHOD.

  METHOD calc_pct.

    DATA: lv_f TYPE f.


    lv_f = ( iv_current / iv_total ) * 100.
    rv_pct = lv_f.

    IF rv_pct = 100.
      rv_pct = 99.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_log DEFINITION FINAL.

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

CLASS lcl_log IMPLEMENTATION.

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

  METHOD add.

    FIELD-SYMBOLS: <ls_log> LIKE LINE OF mt_log.

    APPEND INITIAL LINE TO mt_log ASSIGNING <ls_log>.
    <ls_log>-msg  = iv_msg.
    <ls_log>-type = iv_type.
    <ls_log>-rc   = iv_rc.

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

  METHOD count.
    rv_count = lines( mt_log ).
  ENDMETHOD.

  METHOD clear.
    CLEAR mt_log.
  ENDMETHOD.  " clear.

  METHOD has_rc.
    READ TABLE mt_log WITH KEY rc = iv_rc TRANSPORTING NO FIELDS.
    rv_yes = boolc( sy-subrc = 0 ).
  ENDMETHOD. "has_rc

ENDCLASS.
