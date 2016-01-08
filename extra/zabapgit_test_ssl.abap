REPORT zabapgit_test_ssl.

* See https://github.com/larshp/abapGit/

PARAMETERS: p_url TYPE swc_value DEFAULT 'https://github.com'.

START-OF-SELECTION.
  PERFORM run.

FORM run.

  DATA: lv_code   TYPE i,
        lv_url    TYPE string,
        li_client TYPE REF TO if_http_client.


  lv_url = p_url.
  cl_http_client=>create_by_url(
    EXPORTING
      url    = lv_url
      ssl_id = 'ANONYM'
    IMPORTING
      client = li_client ).

  li_client->send( ).
  li_client->receive(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4 ).
  IF sy-subrc <> 0.
    WRITE: / 'Error', sy-subrc.
    RETURN.
  ENDIF.

  li_client->response->get_status(
      IMPORTING
        code = lv_code ).
  IF lv_code = 200.
    WRITE: / 'Success, it works'.
  ELSE.
    WRITE: / 'Error', lv_code.
  ENDIF.

ENDFORM.
