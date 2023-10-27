CLASS zcl_abapgit_gitv2_porcelain DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS list_branches
      IMPORTING
        iv_url    TYPE string
        iv_prefix TYPE string OPTIONAL
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS list_no_blobs
      RAISING zcx_abapgit_exception.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_service,
        receive TYPE string VALUE 'receive',                  "#EC NOTEXT
        upload  TYPE string VALUE 'upload',                   "#EC NOTEXT
      END OF c_service .

    CONSTANTS c_flush_pkt TYPE c LENGTH 4 VALUE '0000'.
    CONSTANTS c_delim_pkt TYPE c LENGTH 4 VALUE '0001'.

    CLASS-METHODS send_command
      IMPORTING
        iv_url       TYPE string
        iv_service   TYPE string
        iv_command   TYPE string
        iv_arguments TYPE string OPTIONAL
      RETURNING
        VALUE(rv_response) TYPE xstring
      RAISING
        zcx_abapgit_exception.
ENDCLASS.

CLASS zcl_abapgit_gitv2_porcelain IMPLEMENTATION.

  METHOD list_branches.
    DATA lv_xstring   TYPE xstring.
    DATA lv_arguments TYPE string.

    IF iv_prefix IS NOT INITIAL.
      lv_arguments = |ref-prefix { iv_prefix }|.
    ENDIF.

    lv_xstring = send_command(
      iv_url       = iv_url
      iv_service   = c_service-upload
      iv_arguments = lv_arguments
      iv_command   = |command=ls-refs| ).

* todo: parse response, this is probably like in v1?
    WRITE / zcl_abapgit_convert=>xstring_to_string_utf8( lv_xstring ).
  ENDMETHOD.

  METHOD send_command.

    CONSTANTS lc_content_regex TYPE string VALUE '^[0-9a-f]{4}#'.

    DATA lo_client  TYPE REF TO zcl_abapgit_http_client.
    DATA lv_cmd_pkt TYPE string.
    DATA lt_headers TYPE zcl_abapgit_http=>ty_headers.
    DATA ls_header  LIKE LINE OF lt_headers.


    ls_header-key = 'Git-Protocol'.
    ls_header-value = 'version=2'.
    APPEND ls_header TO lt_headers.

    lo_client = zcl_abapgit_http=>create_by_url(
      iv_url     = iv_url
      iv_service = c_service-upload
      it_headers = lt_headers ).

    lo_client->check_smart_response(
      iv_expected_content_type = |application/x-git-{ iv_service }-pack-advertisement|
      iv_content_regex         = lc_content_regex ).

    lv_cmd_pkt = zcl_abapgit_git_utils=>pkt_string( |{ iv_command }\n| )
      && zcl_abapgit_git_utils=>pkt_string( |agent={ zcl_abapgit_http=>get_agent( ) }\n| ).
    IF iv_arguments IS NOT INITIAL.
      lv_cmd_pkt = lv_cmd_pkt && c_delim_pkt.
      lv_cmd_pkt = lv_cmd_pkt && zcl_abapgit_git_utils=>pkt_string( iv_arguments ).
    ENDIF.
    lv_cmd_pkt = lv_cmd_pkt && c_flush_pkt.

    lo_client->set_header(
      iv_key   = '~request_uri'
      iv_value = zcl_abapgit_url=>path_name( iv_url ) && |/git-{ iv_service }-pack| ).

    lo_client->set_header(
      iv_key   = '~request_method'
      iv_value = 'POST' ).

    lo_client->set_header(
      iv_key   = 'Content-Type'
      iv_value = |application/x-git-{ iv_service }-pack-request| ).

    lo_client->set_header(
      iv_key   = 'Accept'
      iv_value = |application/x-git-{ iv_service }-pack-result| ).

    rv_response = lo_client->send_receive_close( zcl_abapgit_convert=>string_to_xstring_utf8( lv_cmd_pkt ) ).

  ENDMETHOD.

  METHOD list_no_blobs.
* todo
  ENDMETHOD.

ENDCLASS.
