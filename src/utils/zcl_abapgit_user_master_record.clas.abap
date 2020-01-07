CLASS zcl_abapgit_user_master_record DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS:
      get_instance
        IMPORTING
          !iv_user       TYPE uname
        RETURNING
          VALUE(ro_user) TYPE REF TO zcl_abapgit_user_master_record.

    METHODS:
      constructor
        IMPORTING
          !iv_user TYPE uname,

      get_name
        RETURNING
          VALUE(rv_name) TYPE zif_abapgit_definitions=>ty_git_user-name,

      get_email
        RETURNING
          VALUE(rv_email) TYPE zif_abapgit_definitions=>ty_git_user-email.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_user,
        user   TYPE uname,
        o_user TYPE REF TO zcl_abapgit_user_master_record,
      END OF ty_user.

    CLASS-DATA:
      gt_user TYPE HASHED TABLE OF ty_user
                   WITH UNIQUE KEY user.

    DATA:
      ms_user TYPE zif_abapgit_definitions=>ty_git_user.
ENDCLASS.



CLASS zcl_abapgit_user_master_record IMPLEMENTATION.


  METHOD constructor.

    DATA: lt_return      TYPE TABLE OF bapiret2,
          ls_address     TYPE bapiaddr3,
          lt_smtp        TYPE TABLE OF bapiadsmtp,
          ls_smtp        TYPE bapiadsmtp,
          lt_dev_clients TYPE SORTED TABLE OF mandt WITH UNIQUE KEY table_line,
          lv_not_found   TYPE abap_bool.
    FIELD-SYMBOLS: <lv_dev_client> LIKE LINE OF lt_dev_clients.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = iv_user
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return
        addsmtp  = lt_smtp.
    LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'EA'.
      lv_not_found = abap_true.
      EXIT.
    ENDLOOP.

    IF lv_not_found = abap_false.
      " Choose the first email from SU01
      SORT lt_smtp BY consnumber ASCENDING.

      LOOP AT lt_smtp INTO ls_smtp.
        ms_user-email = ls_smtp-e_mail.
        EXIT.
      ENDLOOP.

      " Attempt to use the full name from SU01
      ms_user-name = ls_address-fullname.

    ELSE.
      " Try other development clients
      SELECT mandt INTO TABLE lt_dev_clients
        FROM t000
        WHERE cccategory  = 'C'
          AND mandt      <> sy-mandt
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
    ENDIF.
  ENDMETHOD.


  METHOD get_email.

    rv_email = ms_user-email.

  ENDMETHOD.


  METHOD get_instance.

    DATA: ls_user TYPE ty_user.
    FIELD-SYMBOLS: <ls_user> TYPE ty_user.

    READ TABLE gt_user ASSIGNING <ls_user>
                       WITH KEY user = iv_user.
    IF sy-subrc <> 0.

      ls_user-user = iv_user.
      CREATE OBJECT ls_user-o_user
        EXPORTING
          iv_user = iv_user.

      INSERT ls_user
             INTO TABLE gt_user
             ASSIGNING <ls_user>.

    ENDIF.

    ro_user = <ls_user>-o_user.

  ENDMETHOD.


  METHOD get_name.

    rv_name = ms_user-name.

  ENDMETHOD.
ENDCLASS.
