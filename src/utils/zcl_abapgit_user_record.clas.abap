CLASS zcl_abapgit_user_record DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS reset.
    CLASS-METHODS get_instance
      IMPORTING
        !iv_user       TYPE sy-uname
      RETURNING
        VALUE(ro_user) TYPE REF TO zcl_abapgit_user_record.
    METHODS constructor
      IMPORTING
        !iv_user TYPE sy-uname.
    METHODS get_name
      RETURNING
        VALUE(rv_name) TYPE zif_abapgit_git_definitions=>ty_git_user-name.
    METHODS get_email
      RETURNING
        VALUE(rv_email) TYPE zif_abapgit_git_definitions=>ty_git_user-email.
    CLASS-METHODS get_title
      IMPORTING
        iv_username     TYPE sy-uname
      RETURNING
        VALUE(rv_title) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_user,
        user   TYPE sy-uname,
        o_user TYPE REF TO zcl_abapgit_user_record,
      END OF ty_user.

    TYPES:
      ty_smtp TYPE STANDARD TABLE OF bapiadsmtp WITH DEFAULT KEY.

    TYPES:
      ty_dev_clients TYPE SORTED TABLE OF sy-mandt WITH UNIQUE KEY table_line.

    CLASS-DATA:
      gt_user TYPE HASHED TABLE OF ty_user
                   WITH UNIQUE KEY user.

    DATA:
      ms_user TYPE zif_abapgit_git_definitions=>ty_git_user.

    METHODS check_user_exists
      IMPORTING
        VALUE(iv_user)    TYPE sy-uname
      EXPORTING
        VALUE(es_address) TYPE bapiaddr3
        VALUE(et_smtp)    TYPE ty_smtp
      RAISING
        zcx_abapgit_exception.

    METHODS get_user_dtls_from_other_clnt
      IMPORTING
        iv_user TYPE sy-uname.
ENDCLASS.



CLASS zcl_abapgit_user_record IMPLEMENTATION.


  METHOD get_title.
* the queried username might not exist, so this method is static

    DATA ls_user_address TYPE addr3_val.

    CALL FUNCTION 'SUSR_USER_ADDRESS_READ'
      EXPORTING
        user_name              = iv_username
      IMPORTING
        user_address           = ls_user_address
      EXCEPTIONS
        user_address_not_found = 1
        OTHERS                 = 2.
    IF sy-subrc = 0.
      rv_title = ls_user_address-name_text.
    ENDIF.

  ENDMETHOD.

  METHOD check_user_exists.

    DATA lt_return TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = iv_user
      IMPORTING
        address  = es_address
      TABLES
        return   = lt_return
        addsmtp  = et_smtp.
    LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'EA'.
      zcx_abapgit_exception=>raise( |User: { iv_user } not found| ).
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    DATA:
      ls_address   TYPE bapiaddr3,
      lt_smtp      TYPE TABLE OF bapiadsmtp,
      ls_smtp      TYPE bapiadsmtp,
      ls_user      TYPE ty_user,
      lo_exception TYPE REF TO zcx_abapgit_exception.

    "Get user details
    TRY.
        check_user_exists(
          EXPORTING
            iv_user    = iv_user
          IMPORTING
            es_address = ls_address
            et_smtp    = lt_smtp ).

        " Choose the first email from SU01
        SORT lt_smtp BY consnumber ASCENDING.

        LOOP AT lt_smtp INTO ls_smtp.
          ms_user-email = ls_smtp-e_mail.
          EXIT.
        ENDLOOP.

        " Attempt to use the full name from SU01
        ms_user-name = ls_address-fullname.
      CATCH zcx_abapgit_exception INTO lo_exception.
        "Could not find user,try to get from other clients
        get_user_dtls_from_other_clnt( iv_user ).
    ENDTRY.

    " If the user has been found add it to the list
    IF ms_user-name IS NOT INITIAL AND ms_user-email IS NOT INITIAL.
      ls_user-user = iv_user.
      ls_user-o_user = me.
      INSERT ls_user INTO TABLE gt_user.
    ENDIF.

  ENDMETHOD.


  METHOD get_email.

    rv_email = ms_user-email.

  ENDMETHOD.


  METHOD get_instance.

    FIELD-SYMBOLS: <ls_user> TYPE ty_user.

    READ TABLE gt_user ASSIGNING <ls_user> WITH TABLE KEY user = iv_user.
    IF sy-subrc = 0.
      ro_user = <ls_user>-o_user.
    ELSE.
      CREATE OBJECT ro_user
        EXPORTING
          iv_user = iv_user.
    ENDIF.

  ENDMETHOD.


  METHOD get_name.

    rv_name = ms_user-name.

  ENDMETHOD.


  METHOD get_user_dtls_from_other_clnt.

    CONSTANTS lc_cc_category TYPE string VALUE 'C'.

    DATA lt_dev_clients TYPE ty_dev_clients.

    FIELD-SYMBOLS: <lv_dev_client> LIKE LINE OF lt_dev_clients.

    " Could not find the user Try other development clients
    SELECT mandt FROM t000 INTO TABLE lt_dev_clients
        WHERE cccategory = lc_cc_category AND mandt <> sy-mandt
        ORDER BY PRIMARY KEY.

    LOOP AT lt_dev_clients ASSIGNING <lv_dev_client>.
      SELECT SINGLE p~name_text a~smtp_addr INTO (ms_user-name,ms_user-email)
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


  METHOD reset.
    CLEAR gt_user.
  ENDMETHOD.
ENDCLASS.
