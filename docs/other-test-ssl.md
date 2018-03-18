---
title: ZABAPGIT_TEST_SSL
category: other
order: 60
---

Following report can be used to test SSL connection to github. Copy paste the report into the ABAP system use package $TMP

```abap
REPORT zabapgit_test_ssl.

* See https://github.com/larshp/abapGit/

PARAMETERS: p_url    TYPE swc_value DEFAULT 'https://github.com',
            p_proxy  TYPE string,
            p_pxport TYPE string.

START-OF-SELECTION.
  PERFORM run.

FORM run.

  DATA: lv_code          TYPE i,
        lv_url           TYPE string,
        li_client        TYPE REF TO if_http_client,
        lv_error_message TYPE string.

  lv_url = p_url.
  cl_http_client=>create_by_url(
    EXPORTING
      url           = lv_url
      ssl_id        = 'ANONYM'
      proxy_host    = p_proxy
      proxy_service = p_pxport
    IMPORTING
      client        = li_client ).

* enter username and password for proxy authentication if needed
*  li_client->authenticate(
*    proxy_authentication = abap_true
*    username             = ''
*    password             = '' ).

  li_client->send( ).
  li_client->receive(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4 ).
  IF sy-subrc <> 0.
    WRITE: / 'Error Number', sy-subrc.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    li_client->get_last_error(
      IMPORTING
        message = lv_error_message ).
    WRITE: / `Error message: ` && lv_error_message.
    WRITE: / 'Also check transaction SMICM -> Goto -> Trace File -> Display End'.
    RETURN.
  ENDIF.

* if SSL Handshake fails, make sure to also check https://launchpad.support.sap.com/#/notes/510007

  li_client->response->get_status(
      IMPORTING
        code = lv_code ).
  IF lv_code = 200.
    WRITE: / 'Success, it works'.
  ELSE.
    WRITE: / 'Error', lv_code.
  ENDIF.

ENDFORM.
```
