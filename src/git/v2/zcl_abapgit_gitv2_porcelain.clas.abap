CLASS zcl_abapgit_gitv2_porcelain DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gitv2_porcelain.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_service,
        receive TYPE string VALUE 'receive',                "#EC NOTEXT
        upload  TYPE string VALUE 'upload',                 "#EC NOTEXT
      END OF c_service .

    CONSTANTS c_flush_pkt TYPE c LENGTH 4 VALUE '0000'.
    CONSTANTS c_delim_pkt TYPE c LENGTH 4 VALUE '0001'.

    CLASS-METHODS send_command
      IMPORTING
        iv_url             TYPE string
        iv_service         TYPE string
        iv_command         TYPE string
        it_arguments       TYPE string_table OPTIONAL
      RETURNING
        VALUE(rv_response) TYPE xstring
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS decode_pack
      IMPORTING
        iv_xstring        TYPE xstring
      RETURNING
        VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_GITV2_PORCELAIN IMPLEMENTATION.


  METHOD decode_pack.

    DATA lv_xstring TYPE xstring.
    DATA lv_contents  TYPE xstring.
    DATA lv_pack      TYPE xstring.
    DATA lv_pktlen    TYPE i.
    DATA lv_hex4      TYPE xstring.

    lv_xstring = iv_xstring.

* The data transfer of the packfile is always multiplexed, using the same semantics of the
* side-band-64k capability from protocol version 1
    WHILE xstrlen( lv_xstring ) > 0.
      lv_hex4 = lv_xstring(4).
      lv_pktlen = zcl_abapgit_git_utils=>length_utf8_hex( lv_hex4 ).
      IF lv_pktlen = 0.
        EXIT.
      ELSEIF lv_pktlen = 1.
* its a delimiter package
        lv_xstring = lv_xstring+4.
        CONTINUE.
      ENDIF.
      lv_contents = lv_xstring(lv_pktlen).
      IF lv_contents+4(1) = '01'.
        CONCATENATE lv_pack lv_contents+5 INTO lv_pack IN BYTE MODE.
      ENDIF.
      lv_xstring = lv_xstring+lv_pktlen.
    ENDWHILE.

    rt_objects = zcl_abapgit_git_pack=>decode( lv_pack ).

  ENDMETHOD.


  METHOD send_command.

    CONSTANTS lc_content_regex TYPE string VALUE '^[0-9a-f]{4}#'.

    DATA lo_client   TYPE REF TO zcl_abapgit_http_client.
    DATA lv_cmd_pkt  TYPE string.
    DATA lt_headers  TYPE zcl_abapgit_http=>ty_headers.
    DATA ls_header   LIKE LINE OF lt_headers.
    DATA lv_argument TYPE string.


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

    lv_cmd_pkt = zcl_abapgit_git_utils=>pkt_string( |command={ iv_command }\n| )
      && zcl_abapgit_git_utils=>pkt_string( |agent={ zcl_abapgit_http=>get_agent( ) }\n| ).
    IF lines( it_arguments ) > 0.
      lv_cmd_pkt = lv_cmd_pkt && c_delim_pkt.
      LOOP AT it_arguments INTO lv_argument.
        lv_cmd_pkt = lv_cmd_pkt && zcl_abapgit_git_utils=>pkt_string( lv_argument ).
      ENDLOOP.
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


  METHOD zif_abapgit_gitv2_porcelain~commits_last_year.

    DATA lv_xstring   TYPE xstring.
    DATA lt_arguments TYPE string_table.
    DATA lv_argument  TYPE string.
    DATA lv_sha1      LIKE LINE OF it_sha1.


    ASSERT lines( it_sha1 ) > 0.

    lv_argument = |deepen-since { zcl_abapgit_git_time=>get_one_year_ago( ) }|.
    APPEND lv_argument TO lt_arguments.
    LOOP AT it_sha1 INTO lv_sha1.
      lv_argument = |want { lv_sha1 }|.
      APPEND lv_argument TO lt_arguments.
    ENDLOOP.
* 'filter object:type=commit' doesn't work on github
    APPEND 'filter blob:none' TO lt_arguments.
    APPEND 'no-progress' TO lt_arguments.
    APPEND 'done' TO lt_arguments.

    lv_xstring = send_command(
      iv_url       = iv_url
      iv_service   = c_service-upload
      iv_command   = |fetch|
      it_arguments = lt_arguments ).

    rt_objects = decode_pack( lv_xstring ).
    DELETE rt_objects WHERE type <> zif_abapgit_git_definitions=>c_type-commit.

  ENDMETHOD.


  METHOD zif_abapgit_gitv2_porcelain~list_branches.
    DATA lv_xstring   TYPE xstring.
    DATA lt_arguments TYPE string_table.
    DATA lv_argument  TYPE string.
    DATA lv_data      TYPE string.

    IF iv_prefix IS NOT INITIAL.
      lv_argument = |ref-prefix { iv_prefix }|.
      APPEND lv_argument TO lt_arguments.
    ENDIF.

    lv_xstring = send_command(
      iv_url       = iv_url
      iv_service   = c_service-upload
      iv_command   = |ls-refs|
      it_arguments = lt_arguments ).

    " add dummy packet so the v1 branch parsing can be reused
    lv_data = |0004\n{ zcl_abapgit_convert=>xstring_to_string_utf8( lv_xstring ) }|.

    CREATE OBJECT ro_list
      EXPORTING
        iv_data = lv_data.

  ENDMETHOD.


  METHOD zif_abapgit_gitv2_porcelain~list_no_blobs.

    DATA lt_sha1    TYPE zif_abapgit_git_definitions=>ty_sha1_tt.
    DATA lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

    ASSERT iv_sha1 IS NOT INITIAL.
    APPEND iv_sha1 TO lt_sha1.

    lt_objects = zif_abapgit_gitv2_porcelain~list_no_blobs_multi(
      iv_url  = iv_url
      it_sha1 = lt_sha1 ).

    rt_expanded = zcl_abapgit_git_porcelain=>full_tree(
      it_objects = lt_objects
      iv_parent  = iv_sha1 ).

  ENDMETHOD.


  METHOD zif_abapgit_gitv2_porcelain~list_no_blobs_multi.

    DATA lv_xstring   TYPE xstring.
    DATA lt_arguments TYPE string_table.
    DATA lv_argument  TYPE string.
    DATA lv_sha1      LIKE LINE OF it_sha1.


    ASSERT lines( it_sha1 ) > 0.

    APPEND 'deepen 1' TO lt_arguments.
    LOOP AT it_sha1 INTO lv_sha1.
      lv_argument = |want { lv_sha1 }|.
      APPEND lv_argument TO lt_arguments.
    ENDLOOP.
    APPEND 'filter blob:none' TO lt_arguments.
    APPEND 'no-progress' TO lt_arguments.
    APPEND 'done' TO lt_arguments.

    lv_xstring = send_command(
      iv_url       = iv_url
      iv_service   = c_service-upload
      iv_command   = |fetch|
      it_arguments = lt_arguments ).

    rt_objects = decode_pack( lv_xstring ).

  ENDMETHOD.
ENDCLASS.
