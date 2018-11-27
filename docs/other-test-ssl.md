---
title: ZABAPGIT_TEST_SSL
category: other
order: 60
---

Following report can be used to test SSL connection to github. Copy paste the report into the ABAP system use package $TMP

```abap
REPORT zabapgit_test_ssl.

* See https://github.com/larshp/abapGit/

PARAMETERS: p_url1 TYPE swc_value DEFAULT 'https://github.com',
            p_url2 TYPE swc_value DEFAULT 'https://api.github.com',
            p_id   TYPE ssfapplssl DEFAULT 'ANONYM'.
* api.github.com is used when pushing code back to github

SELECTION-SCREEN BEGIN OF BLOCK proxy WITH FRAME.
* proxy settings, fill if your system is behind a proxy
PARAMETERS: p_proxy  TYPE string,
            p_pxport TYPE string,
            p_puser  TYPE string,
            p_ppwd   TYPE string.
SELECTION-SCREEN END OF BLOCK proxy.

START-OF-SELECTION.
  PERFORM run USING p_url1.
  WRITE: /, '----', /.
  PERFORM run USING p_url2.

FORM run USING iv_url TYPE swc_value.

  DATA: lv_code          TYPE i,
        lv_url           TYPE string,
        li_client        TYPE REF TO if_http_client,
        lt_errors        TYPE TABLE OF string,
        lv_error_message TYPE string.

  IF iv_url IS INITIAL.
    RETURN.
  ENDIF.

  lv_url = iv_url.
  cl_http_client=>create_by_url(
    EXPORTING
      url           = lv_url
      ssl_id        = p_id
      proxy_host    = p_proxy
      proxy_service = p_pxport
    IMPORTING
      client        = li_client ).

  IF NOT p_puser IS INITIAL.
    li_client->authenticate(
      proxy_authentication = abap_true
      username             = p_puser
      password             = p_ppwd ).
  ENDIF.

  li_client->send( ).
  li_client->receive(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4 ).
  IF sy-subrc <> 0.
    WRITE: / 'Error Number', sy-subrc, /.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    li_client->get_last_error(
      IMPORTING
        message = lv_error_message ).
    SPLIT lv_error_message AT cl_abap_char_utilities=>newline INTO TABLE lt_errors.
    LOOP AT lt_errors INTO lv_error_message.
      WRITE: / lv_error_message.
    ENDLOOP.
    WRITE: / 'Also check transaction SMICM -> Goto -> Trace File -> Display End'.
    RETURN.
  ENDIF.

* if SSL Handshake fails, make sure to also check https://launchpad.support.sap.com/#/notes/510007

  li_client->response->get_status(
    IMPORTING
      code = lv_code ).
  IF lv_code = 200.
    WRITE: / lv_url, ': ok'.
  ELSE.
    WRITE: / 'Error', lv_code.
  ENDIF.

ENDFORM.
```
