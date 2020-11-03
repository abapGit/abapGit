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
        !it_branches     TYPE zif_abapgit_definitions=>ty_git_branch_list_tt OPTIONAL
      EXPORTING
        !et_objects      TYPE zif_abapgit_definitions=>ty_objects_tt
        !ev_branch       TYPE zif_abapgit_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS upload_pack_by_commit
      IMPORTING
        !iv_url          TYPE string
        !iv_hash         TYPE zif_abapgit_definitions=>ty_sha1 OPTIONAL
        !iv_deepen_level TYPE i DEFAULT 0
      EXPORTING
        !et_objects      TYPE zif_abapgit_definitions=>ty_objects_tt
        !ev_commit       TYPE zif_abapgit_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .
* local to remote
    CLASS-METHODS receive_pack
      IMPORTING
        !iv_url         TYPE string
        !iv_old         TYPE zif_abapgit_definitions=>ty_sha1
        !iv_new         TYPE zif_abapgit_definitions=>ty_sha1
        !iv_branch_name TYPE string
        !iv_pack        TYPE xstring
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
        receive TYPE string VALUE 'receive',                "#EC NOTEXT
        upload  TYPE string VALUE 'upload',                 "#EC NOTEXT
      END OF c_service .
    CONSTANTS:
      BEGIN OF c_smart_response_check,
        BEGIN OF get_refs,
          content_regex TYPE string VALUE '^[0-9a-f]{4}#',
          content_type  TYPE string VALUE 'application/x-git-<service>-pack-advertisement',
        END OF get_refs,
      END OF c_smart_response_check .

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
        !ev_branch      TYPE zif_abapgit_definitions=>ty_sha1
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
        !it_hashes        TYPE zif_abapgit_definitions=>ty_sha1_tt
      RETURNING
        VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS ZCL_ABAPGIT_GIT_TRANSPORT IMPLEMENTATION.


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

    DATA: lv_data TYPE string.
    DATA: lv_expected_content_type TYPE string.

    eo_client = zcl_abapgit_http=>create_by_url(
      iv_url     = iv_url
      iv_service = iv_service ).

    lv_expected_content_type = c_smart_response_check-get_refs-content_type.
    REPLACE '<service>' IN lv_expected_content_type WITH iv_service.

    eo_client->check_smart_response(
        iv_expected_content_type = lv_expected_content_type
        iv_content_regex         = c_smart_response_check-get_refs-content_regex ).

    lv_data = eo_client->get_cdata( ).

    CREATE OBJECT eo_branch_list
      EXPORTING
        iv_data = lv_data.

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
              zif_abapgit_definitions=>c_newline.
    lv_cmd_pkt = zcl_abapgit_git_utils=>pkt_string( lv_line ).

    lv_buffer = lv_cmd_pkt && '0000'.
    lv_tmp = zcl_abapgit_convert=>string_to_xstring_utf8( lv_buffer ).

    CONCATENATE lv_tmp iv_pack INTO lv_xstring IN BYTE MODE.

    lv_xstring = lo_client->send_receive_close( lv_xstring ).

* todo, this part should be changed, instead of looking at texts
* parse the reply and look for the "ng" not good indicator
    lv_string = zcl_abapgit_convert=>xstring_to_string_utf8( lv_xstring ).
    IF NOT lv_string CP '*unpack ok*'.
      zcx_abapgit_exception=>raise( 'unpack not ok' ).
    ELSEIF lv_string CP '*pre-receive hook declined*'.
      zcx_abapgit_exception=>raise( 'pre-receive hook declined' ).
    ELSEIF lv_string CP '*protected branch hook declined*'.
      zcx_abapgit_exception=>raise( 'protected branch hook declined' ).
    ELSEIF lv_string CP '*push declined due to email privacy*'.
      zcx_abapgit_exception=>raise( 'push declined due to email privacy' ).
    ELSEIF lv_string CP '*funny refname*'.
      zcx_abapgit_exception=>raise( 'funny refname' ).
    ELSEIF lv_string CP '*failed to update ref*'.
      zcx_abapgit_exception=>raise( 'failed to update ref' ).
    ELSEIF lv_string CP '*missing necessary objects*'.
      zcx_abapgit_exception=>raise( 'missing necessary objects' ).
    ELSEIF lv_string CP '*refusing to delete the current branch*'.
      zcx_abapgit_exception=>raise( 'branch delete not allowed' ).
    ELSEIF lv_string CP '*cannot lock ref*reference already exists*'.
      zcx_abapgit_exception=>raise( 'branch already exists' ).
    ELSEIF lv_string CP '*invalid committer*'.
      zcx_abapgit_exception=>raise( 'invalid committer' ).
    ENDIF.

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
          && ` ` && lv_capa && zif_abapgit_definitions=>c_newline.
      ELSE.
        lv_line = 'want' && ` ` && <lv_hash>
          && zif_abapgit_definitions=>c_newline.
      ENDIF.
      lv_buffer = lv_buffer && zcl_abapgit_git_utils=>pkt_string( lv_line ).
    ENDLOOP.

    IF iv_deepen_level > 0.
      lv_buffer = lv_buffer && zcl_abapgit_git_utils=>pkt_string( |deepen { iv_deepen_level }| &&
        zif_abapgit_definitions=>c_newline ).
    ENDIF.

    lv_buffer = lv_buffer
             && '0000'
             && '0009done' && zif_abapgit_definitions=>c_newline.

    lv_xstring = io_client->send_receive_close( zcl_abapgit_convert=>string_to_xstring_utf8( lv_buffer ) ).

    parse( IMPORTING ev_pack = lv_pack
           CHANGING  cv_data = lv_xstring ).

    IF lv_pack IS INITIAL.
      zcx_abapgit_exception=>raise( 'Response could not be parsed - empty pack returned.' ).
    ENDIF.

    rt_objects = zcl_abapgit_git_pack=>decode( lv_pack ).

  ENDMETHOD.


  METHOD upload_pack_by_branch.

    DATA: lo_client  TYPE REF TO zcl_abapgit_http_client,
          lt_hashes  TYPE zif_abapgit_definitions=>ty_sha1_tt.

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
          lt_hashes TYPE zif_abapgit_definitions=>ty_sha1_tt.


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
