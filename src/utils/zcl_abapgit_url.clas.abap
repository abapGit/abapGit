class ZCL_ABAPGIT_URL definition
  public
  final
  create public .

public section.

  class-methods HOST
    importing
      !IV_REPO type STRING
    returning
      value(RV_HOST) type STRING
    raising
      ZCX_ABAPGIT_EXCEPTION .
  class-methods NAME
    importing
      !IV_REPO type STRING
    returning
      value(RV_NAME) type STRING
    raising
      ZCX_ABAPGIT_EXCEPTION .
  class-methods PATH_NAME
    importing
      !IV_REPO type STRING
    returning
      value(RV_PATH_NAME) type STRING
    raising
      ZCX_ABAPGIT_EXCEPTION .
private section.

  class-methods REGEX
    importing
      !IV_REPO type STRING
    exporting
      !EV_HOST type STRING
      !EV_PATH type STRING
      !EV_NAME type STRING
    raising
      ZCX_ABAPGIT_EXCEPTION .
ENDCLASS.



CLASS ZCL_ABAPGIT_URL IMPLEMENTATION.


  METHOD host.

    regex( EXPORTING iv_repo = iv_repo
           IMPORTING ev_host = rv_host ).

  ENDMETHOD.


  METHOD name.

    regex( EXPORTING iv_repo = iv_repo
           IMPORTING ev_name = rv_name ).

  ENDMETHOD.


  METHOD path_name.

    DATA: lv_host TYPE string ##NEEDED.

    FIND REGEX '(.*://[^/]*)(.*)' IN iv_repo
      SUBMATCHES lv_host rv_path_name.

  ENDMETHOD.


  METHOD regex.

    FIND REGEX '(.*://[^/]*)(.*/)([^\.]*)[\.git]?' IN iv_repo
      SUBMATCHES ev_host ev_path ev_name.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Malformed URL' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
