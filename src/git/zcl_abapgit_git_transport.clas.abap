CLASS zcl_abapgit_git_transport DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

* remote to local
    CLASS-METHODS upload_pack_by_branch
      IMPORTING
        !iv_url          TYPE string
        !iv_branch_name  TYPE string
        !iv_deepen_level TYPE i DEFAULT 1
        !it_branches     TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt OPTIONAL
      EXPORTING
        !et_objects      TYPE zif_abapgit_definitions=>ty_objects_tt
        !ev_branch       TYPE zif_abapgit_git_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS upload_pack_by_commit
      IMPORTING
        !iv_url          TYPE string
        !iv_hash         TYPE zif_abapgit_git_definitions=>ty_sha1 OPTIONAL
        !iv_deepen_level TYPE i DEFAULT 0
      EXPORTING
        !et_objects      TYPE zif_abapgit_definitions=>ty_objects_tt
        !ev_commit       TYPE zif_abapgit_git_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .
* local to remote
    CLASS-METHODS receive_pack
      IMPORTING
        !iv_url         TYPE string
        !iv_old         TYPE zif_abapgit_git_definitions=>ty_sha1
        !iv_new         TYPE zif_abapgit_git_definitions=>ty_sha1
        !iv_branch_name TYPE string
        !iv_pack        TYPE xstring OPTIONAL
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS branches
      IMPORTING
        !iv_url               TYPE string
      RETURNING
        VALUE(ro_branch_list) TYPE REF TO zcl_abapgit_git_branch_list
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_service,
        receive TYPE string VALUE 'receive',                  "#EC NOTEXT
        upload  TYPE string VALUE 'upload',                   "#EC NOTEXT
      END OF c_service .

    CLASS-METHODS check_report_status
      IMPORTING
        !iv_string TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS branch_list
      IMPORTING
        !iv_url         TYPE string
        !iv_service     TYPE string
      EXPORTING
        !eo_client      TYPE REF TO zcl_abapgit_http_client
        !eo_branch_list TYPE REF TO zcl_abapgit_git_branch_list
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS find_branch
      IMPORTING
        !iv_url         TYPE string
        !iv_service     TYPE string
        !iv_branch_name TYPE string
      EXPORTING
        !eo_client      TYPE REF TO zcl_abapgit_http_client
        !ev_branch      TYPE zif_abapgit_git_definitions=>ty_sha1
        !eo_branch_list TYPE REF TO zcl_abapgit_git_branch_list
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS parse
      EXPORTING
        !ev_pack TYPE xstring
      CHANGING
        !cv_data TYPE xstring
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS upload_pack
      IMPORTING
        !io_client        TYPE REF TO zcl_abapgit_http_client
        !iv_url           TYPE string
        !iv_deepen_level  TYPE i DEFAULT 0
        !it_hashes        TYPE zif_abapgit_git_definitions=>ty_sha1_tt
      RETURNING
        VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_git_transport IMPLEMENTATION.


  METHOD branches.

    DATA: lo_client TYPE REF TO zcl_abapgit_http_client.


    branch_list(
      EXPORTING
        iv_url         = iv_url
        iv_service     = c_service-upload
      IMPORTING
        eo_client      = lo_client
        eo_branch_list = ro_branch_list ).

    lo_client->close( ).

  ENDMETHOD.


  METHOD branch_list.

    CONSTANTS lc_content_regex TYPE string VALUE '^[0-9a-f]{4}#'.
    CONSTANTS lc_content_type  TYPE string VALUE 'application/x-git-<service>-pack-advertisement'.

    DATA: lv_data TYPE string.
    DATA: lv_expected_content_type TYPE string.

    eo_client = zcl_abapgit_http=>create_by_url(
      iv_url     = iv_url
      iv_service = iv_service ).

    lv_expected_content_type = lc_content_type.
    REPLACE '<service>' IN lv_expected_content_type WITH iv_service.

    eo_client->check_smart_response(
        iv_expected_content_type = lv_expected_content_type
        iv_content_regex         = lc_content_regex ).

    lv_data = eo_client->get_cdata( ).

    CREATE OBJECT eo_branch_list
      EXPORTING
        iv_data = lv_data.

  ENDMETHOD.


  METHOD check_report_status.

    DATA:
      lv_string        TYPE string,
      lv_error         TYPE string,
      lv_unpack_status TYPE string,
      lv_unpack_code   TYPE string,
      lv_unpack_text   TYPE string,
      lv_commnd_status TYPE string,
      lv_commnd_code   TYPE string,
      lv_commnd_text   TYPE string.

    " Based on https://git-scm.com/docs/pack-protocol/2.2.3#_report_status
    lv_string = iv_string.

    IF lv_string = ''.
      lv_error = 'Unexpected empty reply'.
    ELSEIF strlen( lv_string ) < 4.
      lv_error = 'Missing pkt length for unpack status'.
    ELSE.
      lv_string = lv_string+4.
      SPLIT lv_string AT cl_abap_char_utilities=>newline INTO lv_unpack_status lv_string.
      SPLIT lv_unpack_status AT space INTO lv_unpack_text lv_unpack_code.

      IF lv_unpack_text <> 'unpack'.
        lv_error = 'Unexpected unpack status'.
      ELSEIF lv_unpack_code <> 'ok'.
        lv_error = |Unpack not ok ({ lv_unpack_code })|.
      ELSEIF lv_string = ''.
        lv_error = 'Unexpected command status'.
      ELSEIF strlen( lv_string ) < 4.
        lv_error = 'Missing pkt length for command status'.
      ELSE.
        lv_string = lv_string+4.
        SPLIT lv_string AT cl_abap_char_utilities=>newline INTO lv_commnd_status lv_string.
        SPLIT lv_commnd_status AT space INTO lv_commnd_code lv_commnd_text.

        IF lv_commnd_code <> 'ok'. "=ng
          " Some pre-defined error messages
          IF lv_commnd_text CP '*pre-receive hook declined*'.
            lv_error = 'Pre-receive hook declined'.
          ELSEIF lv_commnd_text CP '*protected branch hook declined*'.
            lv_error = 'Protected branch hook declined'.
          ELSEIF lv_commnd_text CP '*push declined due to email privacy*'.
            lv_error = 'Push declined due to email privacy'.
          ELSEIF lv_commnd_text CP '*funny refname*'.
            lv_error = 'Funny refname'.
          ELSEIF lv_commnd_text CP '*failed to update ref*'.
            lv_error = 'Failed to update ref'.
          ELSEIF lv_commnd_text CP '*missing necessary objects*'.
            lv_error = 'Missing necessary objects'.
          ELSEIF lv_commnd_text CP '*refusing to delete the current branch*'.
            lv_error = 'Branch delete not allowed'.
          ELSEIF lv_commnd_text CP '*cannot lock ref*reference already exists*'.
            lv_error = 'Branch already exists'.
          ELSEIF lv_commnd_text CP '*cannot lock ref*but expected*'.
            lv_error = 'Branch cannot be locked'.
          ELSEIF lv_commnd_text CP '*invalid committer*'.
            lv_error = 'Invalid committer'.
          ELSE.
            " Otherwise return full error message
            lv_error = lv_commnd_text.
          ENDIF.
        ELSEIF strlen( lv_string ) < 4.
          lv_error = 'Missing flush-pkt'.
        ELSEIF lv_string <> '0000' AND lv_string <> '00000000'.
          " We update only one reference at a time so this should be the end
          lv_error = 'Unexpected end of status (flush-pkt)'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_error IS NOT INITIAL.
      zcx_abapgit_exception=>raise( |Git protocol error: { lv_error }| ).
    ENDIF.

  ENDMETHOD.


  METHOD find_branch.

    branch_list(
      EXPORTING
        iv_url          = iv_url
        iv_service      = iv_service
      IMPORTING
        eo_client       = eo_client
        eo_branch_list  = eo_branch_list ).

    IF ev_branch IS SUPPLIED.
      ev_branch = eo_branch_list->find_by_name( iv_branch_name )-sha1.
    ENDIF.

  ENDMETHOD.


  METHOD parse.

    CONSTANTS: lc_band1 TYPE x VALUE '01'.

    DATA: lv_len      TYPE i,
          lv_contents TYPE xstring,
          lv_pack     TYPE xstring.


    WHILE xstrlen( cv_data ) >= 4.
      lv_len = zcl_abapgit_git_utils=>length_utf8_hex( cv_data ).

      IF lv_len > xstrlen( cv_data ).
        zcx_abapgit_exception=>raise( 'parse, string length too large' ).
      ENDIF.

      lv_contents = cv_data(lv_len).
      IF lv_len = 0.
        cv_data = cv_data+4.
        CONTINUE.
      ELSE.
        cv_data = cv_data+lv_len.
      ENDIF.

      lv_contents = lv_contents+4.

      IF xstrlen( lv_contents ) > 1 AND lv_contents(1) = lc_band1.
        CONCATENATE lv_pack lv_contents+1 INTO lv_pack IN BYTE MODE.
      ENDIF.

    ENDWHILE.

    ev_pack = lv_pack.

  ENDMETHOD.


  METHOD receive_pack.

    DATA: lo_client   TYPE REF TO zcl_abapgit_http_client,
          lv_cmd_pkt  TYPE string,
          lv_line     TYPE string,
          lv_tmp      TYPE xstring,
          lv_xstring  TYPE xstring,
          lv_string   TYPE string,
          lv_cap_list TYPE string,
          lv_buffer   TYPE string.


    find_branch(
      EXPORTING
        iv_url         = iv_url
        iv_service     = c_service-receive
        iv_branch_name = iv_branch_name
      IMPORTING
        eo_client      = lo_client ).

    lo_client->set_headers(
      iv_url     = iv_url
      iv_service = c_service-receive ).

    lv_cap_list = 'report-status'.

    lv_line = iv_old &&
              ` ` &&
              iv_new &&
              ` ` &&
              iv_branch_name &&
              zcl_abapgit_git_utils=>get_null( ) &&
              ` ` &&
              lv_cap_list &&
              cl_abap_char_utilities=>newline.
    lv_cmd_pkt = zcl_abapgit_git_utils=>pkt_string( lv_line ).

    lv_buffer = lv_cmd_pkt && '0000'.
    lv_tmp = zcl_abapgit_convert=>string_to_xstring_utf8( lv_buffer ).

    CONCATENATE lv_tmp iv_pack INTO lv_xstring IN BYTE MODE.

    lv_xstring = lo_client->send_receive_close( lv_xstring ).

    lv_string = zcl_abapgit_convert=>xstring_to_string_utf8( lv_xstring ).

    check_report_status( lv_string ).

  ENDMETHOD.


  METHOD upload_pack.

    DATA: lv_capa    TYPE string,
          lv_line    TYPE string,
          lv_buffer  TYPE string,
          lv_xstring TYPE xstring,
          lv_pack    TYPE xstring.

    FIELD-SYMBOLS: <lv_hash> LIKE LINE OF it_hashes.


    io_client->set_headers( iv_url     = iv_url
                            iv_service = c_service-upload ).

    LOOP AT it_hashes FROM 1 ASSIGNING <lv_hash>.
      IF sy-tabix = 1.
        lv_capa = 'side-band-64k no-progress multi_ack'.
        lv_line = 'want' && ` ` && <lv_hash>
          && ` ` && lv_capa && cl_abap_char_utilities=>newline.
      ELSE.
        lv_line = 'want' && ` ` && <lv_hash>
          && cl_abap_char_utilities=>newline.
      ENDIF.
      lv_buffer = lv_buffer && zcl_abapgit_git_utils=>pkt_string( lv_line ).
    ENDLOOP.

    IF iv_deepen_level > 0.
      lv_buffer = lv_buffer && zcl_abapgit_git_utils=>pkt_string( |deepen { iv_deepen_level }| &&
        cl_abap_char_utilities=>newline ).
    ENDIF.

    lv_buffer = lv_buffer
             && '0000'
             && '0009done' && cl_abap_char_utilities=>newline.

    lv_xstring = io_client->send_receive_close( zcl_abapgit_convert=>string_to_xstring_utf8( lv_buffer ) ).

    parse( IMPORTING ev_pack = lv_pack
           CHANGING  cv_data = lv_xstring ).

    IF lv_pack IS INITIAL.
      zcx_abapgit_exception=>raise( 'Response could not be parsed - empty pack returned.' ).
    ENDIF.

    rt_objects = zcl_abapgit_git_pack=>decode( lv_pack ).

  ENDMETHOD.


  METHOD upload_pack_by_branch.

    DATA: lo_client TYPE REF TO zcl_abapgit_http_client,
          lt_hashes TYPE zif_abapgit_git_definitions=>ty_sha1_tt.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF it_branches.


    CLEAR: et_objects,
           ev_branch.

    find_branch(
      EXPORTING
        iv_url         = iv_url
        iv_service     = c_service-upload
        iv_branch_name = iv_branch_name
      IMPORTING
        eo_client      = lo_client
        ev_branch      = ev_branch ).

    IF it_branches IS INITIAL.
      APPEND ev_branch TO lt_hashes.
    ELSE.
      LOOP AT it_branches ASSIGNING <ls_branch>.
        APPEND <ls_branch>-sha1 TO lt_hashes.
      ENDLOOP.
    ENDIF.

    et_objects = upload_pack( io_client       = lo_client
                              iv_url          = iv_url
                              iv_deepen_level = iv_deepen_level
                              it_hashes       = lt_hashes ).

  ENDMETHOD.


  METHOD upload_pack_by_commit.

    DATA: lo_client TYPE REF TO zcl_abapgit_http_client,
          lt_hashes TYPE zif_abapgit_git_definitions=>ty_sha1_tt.


    CLEAR: et_objects,
           ev_commit.

    APPEND iv_hash TO lt_hashes.
    ev_commit = iv_hash.

    lo_client = zcl_abapgit_http=>create_by_url(
      iv_url     = iv_url
      iv_service = c_service-upload ).

    et_objects = upload_pack( io_client       = lo_client
                              iv_url          = iv_url
                              iv_deepen_level = iv_deepen_level
                              it_hashes       = lt_hashes ).

  ENDMETHOD.
ENDCLASS.
