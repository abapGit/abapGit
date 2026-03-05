CLASS zcl_abapgit_user_record DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_env_factory.

  PUBLIC SECTION.
    CLASS-METHODS reset.

    INTERFACES zif_abapgit_user_record.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_user,
        user  TYPE sy-uname,
        name  TYPE string,
        email TYPE string,
      END OF ty_user.

    CLASS-DATA gt_user TYPE HASHED TABLE OF ty_user WITH UNIQUE KEY user.

    CLASS-METHODS check_user_exists
      IMPORTING
        iv_user     TYPE sy-uname
      EXPORTING
        ev_fullname TYPE string
        ev_email    TYPE string
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS get_user_dtls_from_other_clnt
      IMPORTING
        iv_user        TYPE sy-uname
      RETURNING
        VALUE(rs_user) TYPE ty_user.

    CLASS-METHODS build_cache
      IMPORTING
        iv_user        TYPE sy-uname
      RETURNING
        VALUE(rs_user) TYPE ty_user.

    CLASS-METHODS read_cache
      IMPORTING
        iv_user        TYPE sy-uname
      RETURNING
        VALUE(rs_user) TYPE ty_user.
ENDCLASS.



CLASS ZCL_ABAPGIT_USER_RECORD IMPLEMENTATION.


  METHOD build_cache.

    " Get user details
    TRY.
        check_user_exists(
          EXPORTING
            iv_user     = iv_user
          IMPORTING
            ev_fullname = rs_user-name
            ev_email    = rs_user-email ).
      CATCH zcx_abapgit_exception.
        " Could not find user, try to get from other clients
        rs_user = get_user_dtls_from_other_clnt( iv_user ).
    ENDTRY.

    rs_user-user = iv_user.
    INSERT rs_user INTO TABLE gt_user.

  ENDMETHOD.


  METHOD check_user_exists.

    DATA lt_return  TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY.
    DATA ls_address TYPE bapiaddr3.
    DATA lt_smtp    TYPE TABLE OF bapiadsmtp.
    DATA ls_smtp    LIKE LINE OF lt_smtp.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = iv_user
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return
        addsmtp  = lt_smtp.
    LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'EA'.
      zcx_abapgit_exception=>raise( |User: { iv_user } not found| ).
    ENDLOOP.

    ev_fullname = ls_address-fullname.

    " Choose the first email from SU01
    SORT lt_smtp BY consnumber ASCENDING.

    LOOP AT lt_smtp INTO ls_smtp.
      ev_email = ls_smtp-e_mail.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_user_dtls_from_other_clnt.

    CONSTANTS lc_cc_category TYPE string VALUE 'C'.
    TYPES ty_dev_clients TYPE SORTED TABLE OF sy-mandt WITH UNIQUE KEY table_line.
    DATA lt_dev_clients TYPE ty_dev_clients.
    FIELD-SYMBOLS <lv_dev_client> LIKE LINE OF lt_dev_clients.

    " Could not find the user, try other development clients
    SELECT mandt FROM t000 INTO TABLE lt_dev_clients
        WHERE cccategory = lc_cc_category AND mandt <> sy-mandt
        ORDER BY PRIMARY KEY.

    LOOP AT lt_dev_clients ASSIGNING <lv_dev_client>.
      SELECT SINGLE u~bname p~name_text a~smtp_addr INTO (rs_user-user, rs_user-name, rs_user-email)
          FROM usr21 AS u
          INNER JOIN adrp AS p ON p~persnumber = u~persnumber
                              AND p~client     = u~mandt
          INNER JOIN adr6 AS a ON a~persnumber = u~persnumber
                              AND a~addrnumber = u~addrnumber
                              AND a~client     = u~mandt
          CLIENT SPECIFIED
          WHERE u~mandt      = <lv_dev_client>
            AND u~bname      = iv_user
            AND p~date_from <= sy-datum
            AND p~date_to   >= sy-datum
            AND a~date_from <= sy-datum.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_cache.

    READ TABLE gt_user INTO rs_user WITH TABLE KEY user = iv_user.
    IF sy-subrc <> 0.
      rs_user = build_cache( iv_user ).
    ENDIF.

  ENDMETHOD.


  METHOD reset.
    CLEAR gt_user.
  ENDMETHOD.


  METHOD zif_abapgit_user_record~get_email.

    rv_email = read_cache( iv_username )-email.

  ENDMETHOD.


  METHOD zif_abapgit_user_record~get_name.

    rv_name = read_cache( iv_username )-name.

  ENDMETHOD.


  METHOD zif_abapgit_user_record~get_title.
* the queried username might not exist, refactored for open-abap compatibility

    DATA lr_addr3             TYPE REF TO data.
    FIELD-SYMBOLS <ls_addr3>  TYPE any.
    FIELD-SYMBOLS <lv_simple> TYPE simple.

    TRY.
        CREATE DATA lr_addr3 TYPE ('ADDR3_VAL').
      CATCH cx_sy_create_data_error.
        RETURN.
    ENDTRY.
    ASSIGN lr_addr3->* TO <ls_addr3>.

    CALL FUNCTION 'SUSR_USER_ADDRESS_READ'
      EXPORTING
        user_name              = iv_username
      IMPORTING
        user_address           = <ls_addr3>
      EXCEPTIONS
        user_address_not_found = 1
        OTHERS                 = 2.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'NAME_TEXT' OF STRUCTURE <ls_addr3> TO <lv_simple>.
      rv_title = <lv_simple>.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
