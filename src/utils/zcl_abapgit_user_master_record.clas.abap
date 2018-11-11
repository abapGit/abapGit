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



CLASS ZCL_ABAPGIT_USER_MASTER_RECORD IMPLEMENTATION.


  METHOD constructor.

    DATA: lt_return  TYPE TABLE OF bapiret2,
          ls_address TYPE bapiaddr3,
          lt_smtp    TYPE TABLE OF bapiadsmtp,
          ls_smtp    TYPE bapiadsmtp.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = iv_user
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return
        addsmtp  = lt_smtp.

*     Choose the first email from SU01
    SORT lt_smtp BY consnumber ASCENDING.

    LOOP AT lt_smtp INTO ls_smtp.
      ms_user-email = ls_smtp-e_mail.
      EXIT.
    ENDLOOP.

*     Attempt to use the full name from SU01
    ms_user-name = ls_address-fullname.

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
