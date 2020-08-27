CLASS lcl_http_response DEFINITION FINAL.
  PUBLIC SECTION.

    INTERFACES zif_abapgit_http_response.

    CLASS-METHODS create
      IMPORTING
        ii_client          TYPE REF TO if_http_client
      RETURNING
        VALUE(ri_response) TYPE REF TO zif_abapgit_http_response.

  PRIVATE SECTION.
    DATA mi_client TYPE REF TO if_http_client.
    DATA mi_response TYPE REF TO if_http_response.
ENDCLASS.

CLASS lcl_http_response IMPLEMENTATION.

  METHOD create.
    DATA lo_response TYPE REF TO lcl_http_response.
    CREATE OBJECT lo_response.
    lo_response->mi_client   = ii_client.
    lo_response->mi_response = ii_client->response.
    ri_response ?= lo_response.
  ENDMETHOD.

  METHOD zif_abapgit_http_response~close.
    mi_client->close( ).
  ENDMETHOD.

  METHOD zif_abapgit_http_response~is_ok.
    DATA lv_code TYPE i.
    lv_code = zif_abapgit_http_response~code( ).
    rv_yes = boolc( lv_code >= 200 AND lv_code < 300 ).
  ENDMETHOD.

  METHOD zif_abapgit_http_response~data.
    rv_data = mi_response->get_data( ).
  ENDMETHOD.

  METHOD zif_abapgit_http_response~cdata.
    rv_data = mi_response->get_cdata( ).
  ENDMETHOD.

  METHOD zif_abapgit_http_response~code.
    DATA lv_msg TYPE string ##NEEDED.
    mi_response->get_status(
      IMPORTING
        reason = lv_msg " for debug
        code   = rv_code ).
  ENDMETHOD.

  METHOD zif_abapgit_http_response~json.

    ri_json = zcl_abapgit_ajson=>parse( zif_abapgit_http_response~cdata( ) ).

  ENDMETHOD.

  METHOD zif_abapgit_http_response~error.
    rv_message = mi_response->get_cdata( ).
  ENDMETHOD.

  METHOD zif_abapgit_http_response~headers.

    DATA lt_headers TYPE tihttpnvp.
    FIELD-SYMBOLS <ls_h> LIKE LINE OF lt_headers.

    CREATE OBJECT ro_headers.

    mi_response->get_header_fields( CHANGING fields = lt_headers ).
    LOOP AT lt_headers ASSIGNING <ls_h>.
      ro_headers->set(
        iv_key = <ls_h>-name
        iv_val = <ls_h>-value ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
