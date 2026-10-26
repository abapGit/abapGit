CLASS lcl_oci_http_response DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_http_response.
    METHODS constructor
      IMPORTING
        iv_code    TYPE i
        iv_data    TYPE xstring
        io_headers TYPE REF TO zcl_abapgit_string_map.
  PRIVATE SECTION.
    DATA mv_code TYPE i.
    DATA mv_data TYPE xstring.
    DATA mo_headers TYPE REF TO zcl_abapgit_string_map.
ENDCLASS.

CLASS lcl_oci_http_response IMPLEMENTATION.
  METHOD constructor.
    mv_code = iv_code.
    mv_data = iv_data.
    mo_headers = io_headers.
  ENDMETHOD.

  METHOD zif_abapgit_http_response~data.
    rv_data = mv_data.
  ENDMETHOD.

  METHOD zif_abapgit_http_response~cdata.
    TRY.
        rv_data = zcl_abapgit_convert=>xstring_to_string_utf8_raw( mv_data ).
      CATCH zcx_abapgit_exception.
        CLEAR rv_data.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_abapgit_http_response~json.
    ri_json = zcl_abapgit_ajson=>parse( zif_abapgit_http_response~cdata( ) ).
  ENDMETHOD.

  METHOD zif_abapgit_http_response~is_ok.
    rv_yes = boolc( mv_code = 200 ).
  ENDMETHOD.

  METHOD zif_abapgit_http_response~code.
    rv_code = mv_code.
  ENDMETHOD.

  METHOD zif_abapgit_http_response~error.
    rv_message = |HTTP { mv_code }|.
  ENDMETHOD.

  METHOD zif_abapgit_http_response~headers.
    ro_headers = mo_headers.
  ENDMETHOD.

  METHOD zif_abapgit_http_response~close.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_oci_http_agent DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_http_agent.
    TYPES:
      BEGIN OF ty_call,
        url             TYPE string,
        authorization   TYPE string,
        follow_redirect TYPE abap_bool,
      END OF ty_call.
    TYPES ty_call_tt TYPE STANDARD TABLE OF ty_call WITH DEFAULT KEY.
    DATA mt_calls TYPE ty_call_tt.
    METHODS constructor
      IMPORTING
        iv_manifest             TYPE xstring
        iv_layer                TYPE xstring
        iv_manifest_digest      TYPE string
        iv_require_bearer       TYPE abap_bool DEFAULT abap_false
        iv_redirect_blob        TYPE abap_bool DEFAULT abap_false
        iv_redirect_location    TYPE string DEFAULT 'https://cdn.example.net/blobdata'
        iv_require_token_basic  TYPE abap_bool DEFAULT abap_false
        iv_reject_bearer_once   TYPE abap_bool DEFAULT abap_false
        iv_reject_bearer_always TYPE abap_bool DEFAULT abap_false
        iv_token_realm          TYPE string DEFAULT 'https://registry.example.com/token'
        iv_manifest_status      TYPE i DEFAULT 200.
  PRIVATE SECTION.
    DATA mv_manifest TYPE xstring.
    DATA mv_layer TYPE xstring.
    DATA mv_manifest_digest TYPE string.
    DATA mv_require_bearer TYPE abap_bool.
    DATA mv_redirect_blob TYPE abap_bool.
    DATA mv_redirect_location TYPE string.
    DATA mv_require_token_basic TYPE abap_bool.
    DATA mv_reject_bearer_once TYPE abap_bool.
    DATA mv_reject_bearer_always TYPE abap_bool.
    DATA mv_token_realm TYPE string.
    DATA mv_manifest_status TYPE i.
    DATA mo_global_headers TYPE REF TO zcl_abapgit_string_map.
ENDCLASS.

CLASS lcl_oci_http_agent IMPLEMENTATION.
  METHOD constructor.
    mv_manifest = iv_manifest.
    mv_layer = iv_layer.
    mv_manifest_digest = iv_manifest_digest.
    mv_require_bearer = iv_require_bearer.
    mv_redirect_blob = iv_redirect_blob.
    mv_redirect_location = iv_redirect_location.
    mv_require_token_basic = iv_require_token_basic.
    mv_reject_bearer_once = iv_reject_bearer_once.
    mv_reject_bearer_always = iv_reject_bearer_always.
    mv_token_realm = iv_token_realm.
    mv_manifest_status = iv_manifest_status.
    CREATE OBJECT mo_global_headers.
  ENDMETHOD.

  METHOD zif_abapgit_http_agent~global_headers.
    ro_global_headers = mo_global_headers.
  ENDMETHOD.

  METHOD zif_abapgit_http_agent~request.
    DATA: lo_headers TYPE REF TO zcl_abapgit_string_map,
          lo_response TYPE REF TO lcl_oci_http_response,
          ls_call TYPE ty_call,
          lv_auth TYPE string,
          lv_code TYPE i,
          lv_data TYPE xstring.

    CREATE OBJECT lo_headers EXPORTING iv_case_insensitive = abap_true.
    lv_auth = io_headers->get( 'Authorization' ).
    ls_call-url = iv_url.
    ls_call-authorization = lv_auth.
    ls_call-follow_redirect = iv_follow_redirect.
    APPEND ls_call TO mt_calls.

    IF iv_url CS '/token'.
      IF mv_require_token_basic = abap_true AND lv_auth NP 'Basic *'.
        lv_code = 401.
        lv_data = ''.
        lo_headers->set( iv_key = 'www-authenticate'
                         iv_val = 'Basic realm="OCI token service"' ).
      ELSE.
        lv_code = 200.
        lv_data = zcl_abapgit_convert=>string_to_xstring_utf8( '{"token":"token-good","expires_in":300}' ).
        lo_headers->set( iv_key = 'content-type'
                         iv_val = 'application/json' ).
      ENDIF.
    ELSEIF iv_url CS '/manifests/'.
      IF mv_manifest_status <> 200.
        lv_code = mv_manifest_status.
        lv_data = ''.
      ELSEIF ( mv_require_bearer = abap_true AND lv_auth <> 'Bearer token-good' ) OR
             ( mv_reject_bearer_always = abap_true AND lv_auth = 'Bearer token-good' ) OR
             ( mv_reject_bearer_once = abap_true AND lv_auth = 'Bearer token-good' ).
        IF mv_reject_bearer_once = abap_true AND lv_auth = 'Bearer token-good'.
          mv_reject_bearer_once = abap_false.
        ENDIF.
        lv_code = 401.
        lv_data = ''.
        lo_headers->set(
          iv_key = 'www-authenticate'
          iv_val = |Bearer realm="{ mv_token_realm }",service="registry.example.com",scope="repository:team/library:pull"| ).
      ELSE.
        lv_code = 200.
        lv_data = mv_manifest.
        lo_headers->set( iv_key = 'content-type'
                         iv_val = 'application/vnd.oci.image.manifest.v1+json' ).
        lo_headers->set( iv_key = 'docker-content-digest'
                         iv_val = mv_manifest_digest ).
      ENDIF.
    ELSEIF iv_url CS '/blobdata'.
      lv_code = 200.
      lv_data = mv_layer.
    ELSEIF iv_url CS '/blobs/'.
      IF ( mv_require_bearer = abap_true AND lv_auth <> 'Bearer token-good' ) OR
         ( mv_reject_bearer_always = abap_true AND lv_auth = 'Bearer token-good' ) OR
         ( mv_reject_bearer_once = abap_true AND lv_auth = 'Bearer token-good' ).
        IF mv_reject_bearer_once = abap_true AND lv_auth = 'Bearer token-good'.
          mv_reject_bearer_once = abap_false.
        ENDIF.
        lv_code = 401.
        lv_data = ''.
        lo_headers->set(
          iv_key = 'www-authenticate'
          iv_val = |Bearer realm="{ mv_token_realm }",service="registry.example.com",scope="repository:team/library:pull"| ).
      ELSEIF mv_redirect_blob = abap_true.
        lv_code = 302.
        lv_data = ''.
        lo_headers->set( iv_key = 'location'
                         iv_val = mv_redirect_location ).
      ELSE.
        lv_code = 200.
        lv_data = mv_layer.
        lo_headers->set( iv_key = 'content-type'
                         iv_val = 'application/vnd.oci.image.layer.v1.tar' ).
      ENDIF.
    ELSE.
      lv_code = 404.
      lv_data = ''.
    ENDIF.

    CREATE OBJECT lo_response
      EXPORTING
        iv_code    = lv_code
        iv_data    = lv_data
        io_headers = lo_headers.
    ri_response = lo_response.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_oci_client DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS setup.
    METHODS teardown.
    METHODS fetch_anonymous FOR TESTING RAISING zcx_abapgit_exception.
    METHODS fetch_pinned_digest FOR TESTING RAISING zcx_abapgit_exception.
    METHODS fetch_basic_credentials FOR TESTING RAISING zcx_abapgit_exception.
    METHODS fetch_bearer FOR TESTING RAISING zcx_abapgit_exception.
    METHODS refresh_rejected_bearer FOR TESTING RAISING zcx_abapgit_exception.
    METHODS reject_denied_bearer FOR TESTING RAISING zcx_abapgit_exception.
    METHODS external_token_auth FOR TESTING RAISING zcx_abapgit_exception.
    METHODS redirect_drops_registry_auth FOR TESTING RAISING zcx_abapgit_exception.
    METHODS redirect_drops_port_auth FOR TESTING RAISING zcx_abapgit_exception.
    METHODS reject_pinned_digest_mismatch FOR TESTING RAISING zcx_abapgit_exception.
    METHODS reject_invalid_json FOR TESTING RAISING zcx_abapgit_exception.
    METHODS reject_manifest_profile FOR TESTING RAISING zcx_abapgit_exception.
    METHODS reject_layer_integrity FOR TESTING RAISING zcx_abapgit_exception.
    METHODS reject_malformed_tar FOR TESTING RAISING zcx_abapgit_exception.
    METHODS reject_http_errors FOR TESTING RAISING zcx_abapgit_exception.
    METHODS fixture
      EXPORTING
        ev_manifest        TYPE xstring
        ev_layer           TYPE xstring
        ev_manifest_digest TYPE string
      RAISING
        zcx_abapgit_exception.
    METHODS make_tar
      RETURNING
        VALUE(rv_tar) TYPE xstring
      RAISING
        zcx_abapgit_exception.
    METHODS make_header
      RETURNING
        VALUE(rv_header) TYPE xstring
      RAISING
        zcx_abapgit_exception.
    METHODS set_field
      IMPORTING
        iv_offset TYPE i
        iv_length TYPE i
        iv_value  TYPE xstring
      CHANGING
        cv_data   TYPE xstring
      RAISING
        zcx_abapgit_exception.
    METHODS set_text
      IMPORTING
        iv_offset TYPE i
        iv_length TYPE i
        iv_value  TYPE string
      CHANGING
        cv_data   TYPE xstring
      RAISING
        zcx_abapgit_exception.
    METHODS zero_bytes
      IMPORTING
        iv_length       TYPE i
      RETURNING
        VALUE(rv_bytes) TYPE xstring.
    METHODS octal_text
      IMPORTING
        iv_number      TYPE i
        iv_width       TYPE i
      RETURNING
        VALUE(rv_text) TYPE string.
    METHODS assert_rejected
      IMPORTING
        iv_manifest        TYPE xstring
        iv_layer           TYPE xstring
        iv_manifest_digest TYPE string
        iv_expected_calls  TYPE i
        iv_manifest_status TYPE i DEFAULT 200
      RAISING
        zcx_abapgit_exception.
ENDCLASS.

CLASS ltcl_oci_client IMPLEMENTATION.
  METHOD setup.
    zcl_abapgit_login_manager=>clear( ).
  ENDMETHOD.

  METHOD teardown.
    zcl_abapgit_login_manager=>clear( ).
  ENDMETHOD.

  METHOD fetch_anonymous.
    DATA: lv_manifest TYPE xstring,
          lv_layer TYPE xstring,
          lv_digest TYPE string,
          ls_reference TYPE zcl_abapgit_oci_reference=>ty_reference,
          ls_snapshot TYPE zif_abapgit_repo_connector=>ty_snapshot,
          ls_file TYPE zif_abapgit_git_definitions=>ty_file,
          ls_call TYPE lcl_oci_http_agent=>ty_call,
          lo_agent TYPE REF TO lcl_oci_http_agent,
          lo_client TYPE REF TO zcl_abapgit_oci_client.

    fixture(
      IMPORTING
        ev_manifest = lv_manifest
        ev_layer = lv_layer
        ev_manifest_digest = lv_digest ).
    CREATE OBJECT lo_agent
      EXPORTING
        iv_manifest = lv_manifest
        iv_layer = lv_layer
        iv_manifest_digest = lv_digest.
    CREATE OBJECT lo_client EXPORTING ii_http_agent = lo_agent.
    ls_reference = zcl_abapgit_oci_reference=>parse( 'oci://registry.example.com/team/library:v1' ).

    ls_snapshot = lo_client->fetch( ls_reference-canonical ).

    cl_abap_unit_assert=>assert_equals( act = lines( ls_snapshot-files )
                                        exp = 1 ).
    READ TABLE ls_snapshot-files INTO ls_file INDEX 1.
    cl_abap_unit_assert=>assert_equals( act = ls_file-filename
                                        exp = '.abapgit.xml' ).
    cl_abap_unit_assert=>assert_equals( act = ls_snapshot-resolved_revision
                                        exp = lv_digest ).
    cl_abap_unit_assert=>assert_equals( act = lines( lo_agent->mt_calls )
                                        exp = 2 ).
    READ TABLE lo_agent->mt_calls INTO ls_call INDEX 1.
    cl_abap_unit_assert=>assert_equals( act = ls_call-follow_redirect
                                        exp = abap_false ).
  ENDMETHOD.


  METHOD fetch_pinned_digest.
    DATA: lv_manifest TYPE xstring,
          lv_layer TYPE xstring,
          lv_digest TYPE string,
          ls_reference TYPE zcl_abapgit_oci_reference=>ty_reference,
          ls_snapshot TYPE zif_abapgit_repo_connector=>ty_snapshot,
          ls_call TYPE lcl_oci_http_agent=>ty_call,
          lo_agent TYPE REF TO lcl_oci_http_agent,
          lo_client TYPE REF TO zcl_abapgit_oci_client.

    fixture(
      IMPORTING
        ev_manifest = lv_manifest
        ev_layer = lv_layer
        ev_manifest_digest = lv_digest ).
    CREATE OBJECT lo_agent
      EXPORTING
        iv_manifest = lv_manifest
        iv_layer = lv_layer
        iv_manifest_digest = lv_digest.
    CREATE OBJECT lo_client EXPORTING ii_http_agent = lo_agent.
    ls_reference = zcl_abapgit_oci_reference=>parse( |oci://registry.example.com/team/library@{ lv_digest }| ).

    ls_snapshot = lo_client->fetch( ls_reference-canonical ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_snapshot-resolved_revision
      exp = lv_digest ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_agent->mt_calls )
      exp = 2 ).
    READ TABLE lo_agent->mt_calls INTO ls_call INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      act = ls_call-url
      exp = |https://registry.example.com/v2/team/library/manifests/{ lv_digest }| ).
  ENDMETHOD.

  METHOD fetch_bearer.
    DATA: lv_manifest TYPE xstring,
          lv_layer TYPE xstring,
          lv_digest TYPE string,
          ls_reference TYPE zcl_abapgit_oci_reference=>ty_reference,
          ls_snapshot TYPE zif_abapgit_repo_connector=>ty_snapshot,
          lo_agent TYPE REF TO lcl_oci_http_agent,
          lo_client TYPE REF TO zcl_abapgit_oci_client,
          ls_call TYPE lcl_oci_http_agent=>ty_call.

    fixture(
      IMPORTING
        ev_manifest = lv_manifest
        ev_layer = lv_layer
        ev_manifest_digest = lv_digest ).
    CREATE OBJECT lo_agent
      EXPORTING
        iv_manifest = lv_manifest
        iv_layer = lv_layer
        iv_manifest_digest = lv_digest
        iv_require_bearer = abap_true.
    CREATE OBJECT lo_client EXPORTING ii_http_agent = lo_agent.
    ls_reference = zcl_abapgit_oci_reference=>parse( 'oci://registry.example.com/team/library:v1' ).

    ls_snapshot = lo_client->fetch( ls_reference-canonical ).

    cl_abap_unit_assert=>assert_equals( act = ls_snapshot-resolved_revision
                                        exp = lv_digest ).
    cl_abap_unit_assert=>assert_equals( act = lines( lo_agent->mt_calls )
                                        exp = 5 ).
    READ TABLE lo_agent->mt_calls INTO ls_call INDEX 3.
    cl_abap_unit_assert=>assert_equals( act = ls_call-authorization
                                        exp = 'Bearer token-good' ).
    READ TABLE lo_agent->mt_calls INTO ls_call INDEX 5.
    cl_abap_unit_assert=>assert_equals( act = ls_call-authorization
                                        exp = 'Bearer token-good' ).
  ENDMETHOD.


  METHOD refresh_rejected_bearer.
    DATA: lv_manifest TYPE xstring,
          lv_layer TYPE xstring,
          lv_digest TYPE string,
          ls_reference TYPE zcl_abapgit_oci_reference=>ty_reference,
          ls_snapshot TYPE zif_abapgit_repo_connector=>ty_snapshot,
          ls_call TYPE lcl_oci_http_agent=>ty_call,
          lo_agent TYPE REF TO lcl_oci_http_agent,
          lo_client TYPE REF TO zcl_abapgit_oci_client.

    fixture(
      IMPORTING
        ev_manifest = lv_manifest
        ev_layer = lv_layer
        ev_manifest_digest = lv_digest ).
    CREATE OBJECT lo_agent
      EXPORTING
        iv_manifest = lv_manifest
        iv_layer = lv_layer
        iv_manifest_digest = lv_digest
        iv_require_bearer = abap_true
        iv_reject_bearer_once = abap_true.
    CREATE OBJECT lo_client EXPORTING ii_http_agent = lo_agent.
    ls_reference = zcl_abapgit_oci_reference=>parse( 'oci://registry.example.com/team/library:v1' ).

    ls_snapshot = lo_client->fetch( ls_reference-canonical ).

    cl_abap_unit_assert=>assert_equals( act = ls_snapshot-resolved_revision
                                        exp = lv_digest ).
    cl_abap_unit_assert=>assert_equals( act = lines( lo_agent->mt_calls )
                                        exp = 7 ).
    READ TABLE lo_agent->mt_calls INTO ls_call INDEX 3.
    cl_abap_unit_assert=>assert_equals( act = ls_call-authorization
                                        exp = 'Bearer token-good' ).
    READ TABLE lo_agent->mt_calls INTO ls_call INDEX 4.
    cl_abap_unit_assert=>assert_equals( act = ls_call-url
                                        exp = 'https://registry.example.com/token' ).
    READ TABLE lo_agent->mt_calls INTO ls_call INDEX 5.
    cl_abap_unit_assert=>assert_equals( act = ls_call-authorization
                                        exp = 'Bearer token-good' ).
  ENDMETHOD.


  METHOD reject_denied_bearer.
    DATA: lv_manifest TYPE xstring,
          lv_layer TYPE xstring,
          lv_digest TYPE string,
          ls_reference TYPE zcl_abapgit_oci_reference=>ty_reference,
          ls_snapshot TYPE zif_abapgit_repo_connector=>ty_snapshot,
          lo_agent TYPE REF TO lcl_oci_http_agent,
          lo_client TYPE REF TO zcl_abapgit_oci_client.

    fixture(
      IMPORTING
        ev_manifest = lv_manifest
        ev_layer = lv_layer
        ev_manifest_digest = lv_digest ).
    CREATE OBJECT lo_agent
      EXPORTING
        iv_manifest = lv_manifest
        iv_layer = lv_layer
        iv_manifest_digest = lv_digest
        iv_require_bearer = abap_true
        iv_reject_bearer_always = abap_true.
    CREATE OBJECT lo_client EXPORTING ii_http_agent = lo_agent.
    ls_reference = zcl_abapgit_oci_reference=>parse( 'oci://registry.example.com/team/library:v1' ).

    TRY.
        ls_snapshot = lo_client->fetch( ls_reference-canonical ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>assert_equals( act = lines( lo_agent->mt_calls )
                                            exp = 5 ).
    ENDTRY.
  ENDMETHOD.

  METHOD fetch_basic_credentials.
    DATA: lv_manifest TYPE xstring,
          lv_layer TYPE xstring,
          lv_digest TYPE string,
          ls_reference TYPE zcl_abapgit_oci_reference=>ty_reference,
          ls_snapshot TYPE zif_abapgit_repo_connector=>ty_snapshot,
          ls_call TYPE lcl_oci_http_agent=>ty_call,
          lo_agent TYPE REF TO lcl_oci_http_agent,
          lo_client TYPE REF TO zcl_abapgit_oci_client.

    fixture(
      IMPORTING
        ev_manifest = lv_manifest
        ev_layer = lv_layer
        ev_manifest_digest = lv_digest ).
    zcl_abapgit_login_manager=>set_basic(
      iv_uri      = 'https://registry.example.com/v2/team/library/manifests/v1'
      iv_username = 'test-user'
      iv_password = 'test-password' ).
    CREATE OBJECT lo_agent
      EXPORTING
        iv_manifest = lv_manifest
        iv_layer = lv_layer
        iv_manifest_digest = lv_digest.
    CREATE OBJECT lo_client EXPORTING ii_http_agent = lo_agent.
    ls_reference = zcl_abapgit_oci_reference=>parse( 'oci://registry.example.com/team/library:v1' ).

    ls_snapshot = lo_client->fetch( ls_reference-canonical ).

    cl_abap_unit_assert=>assert_equals( act = ls_snapshot-resolved_revision
                                        exp = lv_digest ).
    READ TABLE lo_agent->mt_calls INTO ls_call INDEX 1.
    IF ls_call-authorization NP 'Basic *'.
      cl_abap_unit_assert=>fail( ).
    ENDIF.
    READ TABLE lo_agent->mt_calls INTO ls_call INDEX 2.
    IF ls_call-authorization NP 'Basic *'.
      cl_abap_unit_assert=>fail( ).
    ENDIF.
  ENDMETHOD.

  METHOD reject_pinned_digest_mismatch.
    DATA: lv_manifest TYPE xstring,
          lv_layer TYPE xstring,
          lv_digest TYPE string,
          lv_reference_text TYPE string,
          lv_zero_digest TYPE string,
          ls_reference TYPE zcl_abapgit_oci_reference=>ty_reference,
          ls_snapshot TYPE zif_abapgit_repo_connector=>ty_snapshot,
          lo_agent TYPE REF TO lcl_oci_http_agent,
          lo_client TYPE REF TO zcl_abapgit_oci_client.

    fixture(
      IMPORTING
        ev_manifest = lv_manifest
        ev_layer = lv_layer
        ev_manifest_digest = lv_digest ).
    CREATE OBJECT lo_agent
      EXPORTING
        iv_manifest = lv_manifest
        iv_layer = lv_layer
        iv_manifest_digest = lv_digest.
    CREATE OBJECT lo_client EXPORTING ii_http_agent = lo_agent.
    DO 64 TIMES.
      CONCATENATE lv_zero_digest '0' INTO lv_zero_digest.
    ENDDO.
    lv_reference_text = |oci://registry.example.com/team/library@sha256:{ lv_zero_digest }|.
    ls_reference = zcl_abapgit_oci_reference=>parse( lv_reference_text ).

    TRY.
        ls_snapshot = lo_client->fetch( ls_reference-canonical ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>assert_equals( act = lines( lo_agent->mt_calls )
                                            exp = 1 ).
    ENDTRY.
  ENDMETHOD.

  METHOD reject_invalid_json.
    DATA: lv_manifest TYPE xstring,
          lv_layer TYPE xstring,
          lv_digest TYPE string.

    fixture(
      IMPORTING
        ev_manifest = lv_manifest
        ev_layer = lv_layer
        ev_manifest_digest = lv_digest ).
    lv_manifest = '7B'.
    lv_digest = |sha256:{ zcl_abapgit_hash=>sha256_raw( lv_manifest ) }|.

    assert_rejected(
      iv_manifest = lv_manifest
      iv_layer = lv_layer
      iv_manifest_digest = lv_digest
      iv_expected_calls = 1 ).

  ENDMETHOD.

  METHOD reject_manifest_profile.
    DATA: lv_manifest TYPE xstring,
          lv_layer TYPE xstring,
          lv_digest TYPE string,
          lv_manifest_text TYPE string,
          lv_descriptor TYPE string,
          lv_replacement TYPE string,
          lv_size TYPE string.

    fixture(
      IMPORTING
        ev_manifest = lv_manifest
        ev_layer = lv_layer
        ev_manifest_digest = lv_digest ).
    lv_manifest_text = zcl_abapgit_convert=>xstring_to_string_utf8_raw( lv_manifest ).
    REPLACE FIRST OCCURRENCE OF 'application/vnd.abapgit.repository.v1' IN lv_manifest_text
      WITH 'application/vnd.example.other'.
    lv_manifest = zcl_abapgit_convert=>string_to_xstring_utf8( lv_manifest_text ).
    lv_digest = |sha256:{ zcl_abapgit_hash=>sha256_raw( lv_manifest ) }|.
    assert_rejected(
      iv_manifest = lv_manifest
      iv_layer = lv_layer
      iv_manifest_digest = lv_digest
      iv_expected_calls = 1 ).

    fixture(
      IMPORTING
        ev_manifest = lv_manifest
        ev_layer = lv_layer
        ev_manifest_digest = lv_digest ).
    lv_size = |{ xstrlen( lv_layer ) }|.
    lv_descriptor =
      |"mediaType":"application/vnd.oci.image.layer.v1.tar",| &&
      |"digest":"sha256:{ zcl_abapgit_hash=>sha256_raw( lv_layer ) }","size":{ lv_size }|.
    CONCATENATE '"layers":[{' lv_descriptor '},' INTO lv_replacement.
    lv_manifest_text = zcl_abapgit_convert=>xstring_to_string_utf8_raw( lv_manifest ).
    REPLACE FIRST OCCURRENCE OF '"layers":[' IN lv_manifest_text WITH lv_replacement.
    lv_manifest = zcl_abapgit_convert=>string_to_xstring_utf8( lv_manifest_text ).
    lv_digest = |sha256:{ zcl_abapgit_hash=>sha256_raw( lv_manifest ) }|.
    assert_rejected(
      iv_manifest = lv_manifest
      iv_layer = lv_layer
      iv_manifest_digest = lv_digest
      iv_expected_calls = 1 ).

    fixture(
      IMPORTING
        ev_manifest = lv_manifest
        ev_layer = lv_layer
        ev_manifest_digest = lv_digest ).
    lv_manifest_text = zcl_abapgit_convert=>xstring_to_string_utf8_raw( lv_manifest ).
    REPLACE FIRST OCCURRENCE OF 'application/vnd.oci.image.manifest.v1+json' IN lv_manifest_text
      WITH 'application/vnd.oci.image.index.v1+json'.
    lv_manifest = zcl_abapgit_convert=>string_to_xstring_utf8( lv_manifest_text ).
    lv_digest = |sha256:{ zcl_abapgit_hash=>sha256_raw( lv_manifest ) }|.
    assert_rejected(
      iv_manifest = lv_manifest
      iv_layer = lv_layer
      iv_manifest_digest = lv_digest
      iv_expected_calls = 1 ).
  ENDMETHOD.

  METHOD reject_layer_integrity.
    DATA: lv_manifest TYPE xstring,
          lv_layer TYPE xstring,
          lv_modified_layer TYPE xstring,
          lv_digest TYPE string,
          lv_zero TYPE x LENGTH 1 VALUE '00'.

    fixture(
      IMPORTING
        ev_manifest = lv_manifest
        ev_layer = lv_layer
        ev_manifest_digest = lv_digest ).
    CONCATENATE lv_layer lv_zero INTO lv_modified_layer IN BYTE MODE.
    assert_rejected(
      iv_manifest = lv_manifest
      iv_layer = lv_modified_layer
      iv_manifest_digest = lv_digest
      iv_expected_calls = 2 ).

    lv_modified_layer = lv_layer+1.
    CONCATENATE lv_zero lv_modified_layer INTO lv_modified_layer IN BYTE MODE.
    assert_rejected(
      iv_manifest = lv_manifest
      iv_layer = lv_modified_layer
      iv_manifest_digest = lv_digest
      iv_expected_calls = 2 ).
  ENDMETHOD.


  METHOD reject_malformed_tar.
    DATA: lv_manifest TYPE xstring,
          lv_layer TYPE xstring,
          lv_digest TYPE string,
          lv_old_layer_digest TYPE string,
          lv_new_layer_digest TYPE string,
          lv_manifest_text TYPE string,
          lv_zero TYPE x LENGTH 1 VALUE '00',
          lv_tail TYPE xstring.

    fixture(
      IMPORTING
        ev_manifest = lv_manifest
        ev_layer = lv_layer
        ev_manifest_digest = lv_digest ).
    lv_old_layer_digest = zcl_abapgit_hash=>sha256_raw( lv_layer ).
    lv_tail = lv_layer+1.
    lv_layer = lv_zero.
    CONCATENATE lv_layer lv_tail INTO lv_layer IN BYTE MODE.
    lv_new_layer_digest = zcl_abapgit_hash=>sha256_raw( lv_layer ).
    lv_manifest_text = zcl_abapgit_convert=>xstring_to_string_utf8_raw( lv_manifest ).
    REPLACE FIRST OCCURRENCE OF lv_old_layer_digest IN lv_manifest_text
      WITH lv_new_layer_digest.
    lv_manifest = zcl_abapgit_convert=>string_to_xstring_utf8( lv_manifest_text ).
    lv_digest = |sha256:{ zcl_abapgit_hash=>sha256_raw( lv_manifest ) }|.

    assert_rejected(
      iv_manifest = lv_manifest
      iv_layer = lv_layer
      iv_manifest_digest = lv_digest
      iv_expected_calls = 2 ).
  ENDMETHOD.

  METHOD reject_http_errors.
    DATA: lv_manifest TYPE xstring,
          lv_layer TYPE xstring,
          lv_digest TYPE string,
          lt_status TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
          lv_status TYPE i.

    APPEND 404 TO lt_status.
    APPEND 429 TO lt_status.
    LOOP AT lt_status INTO lv_status.
      fixture(
        IMPORTING
          ev_manifest = lv_manifest
          ev_layer = lv_layer
          ev_manifest_digest = lv_digest ).
      assert_rejected(
        iv_manifest = lv_manifest
        iv_layer = lv_layer
        iv_manifest_digest = lv_digest
        iv_expected_calls = 1
        iv_manifest_status = lv_status ).
    ENDLOOP.
  ENDMETHOD.

  METHOD assert_rejected.
    DATA: lo_agent TYPE REF TO lcl_oci_http_agent,
          lo_client TYPE REF TO zcl_abapgit_oci_client,
          ls_reference TYPE zcl_abapgit_oci_reference=>ty_reference,
          ls_snapshot TYPE zif_abapgit_repo_connector=>ty_snapshot.

    CREATE OBJECT lo_agent
      EXPORTING
        iv_manifest = iv_manifest
        iv_layer = iv_layer
        iv_manifest_digest = iv_manifest_digest
        iv_manifest_status = iv_manifest_status.
    CREATE OBJECT lo_client EXPORTING ii_http_agent = lo_agent.
    ls_reference = zcl_abapgit_oci_reference=>parse( 'oci://registry.example.com/team/library:v1' ).

    TRY.
        ls_snapshot = lo_client->fetch( ls_reference-canonical ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>assert_equals(
          act = lines( lo_agent->mt_calls )
          exp = iv_expected_calls ).
    ENDTRY.
  ENDMETHOD.

  METHOD external_token_auth.
    DATA: lv_manifest TYPE xstring,
          lv_layer TYPE xstring,
          lv_digest TYPE string,
          ls_reference TYPE zcl_abapgit_oci_reference=>ty_reference,
          ls_snapshot TYPE zif_abapgit_repo_connector=>ty_snapshot,
          ls_call TYPE lcl_oci_http_agent=>ty_call,
          lo_agent TYPE REF TO lcl_oci_http_agent,
          lo_client TYPE REF TO zcl_abapgit_oci_client.

    fixture(
      IMPORTING
        ev_manifest = lv_manifest
        ev_layer = lv_layer
        ev_manifest_digest = lv_digest ).
    zcl_abapgit_login_manager=>set_basic(
      iv_uri = 'https://auth.example.net/token'
      iv_username = 'test-user'
      iv_password = 'test-password' ).
    CREATE OBJECT lo_agent
      EXPORTING
        iv_manifest = lv_manifest
        iv_layer = lv_layer
        iv_manifest_digest = lv_digest
        iv_require_bearer = abap_true
        iv_require_token_basic = abap_true
        iv_token_realm = 'https://auth.example.net/token'.
    CREATE OBJECT lo_client EXPORTING ii_http_agent = lo_agent.
    ls_reference = zcl_abapgit_oci_reference=>parse( 'oci://registry.example.com/team/library:v1' ).

    ls_snapshot = lo_client->fetch( ls_reference-canonical ).

    cl_abap_unit_assert=>assert_equals( act = ls_snapshot-resolved_revision
                                        exp = lv_digest ).
    READ TABLE lo_agent->mt_calls INTO ls_call INDEX 1.
    cl_abap_unit_assert=>assert_initial( ls_call-authorization ).
    READ TABLE lo_agent->mt_calls INTO ls_call INDEX 2.
    IF ls_call-authorization NP 'Basic *'.
      cl_abap_unit_assert=>fail( ).
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = ls_call-url
                                        exp = 'https://auth.example.net/token' ).
    READ TABLE lo_agent->mt_calls INTO ls_call INDEX 3.
    cl_abap_unit_assert=>assert_equals( act = ls_call-authorization
                                        exp = 'Bearer token-good' ).
  ENDMETHOD.

  METHOD redirect_drops_registry_auth.
    DATA: lv_manifest TYPE xstring,
          lv_layer TYPE xstring,
          lv_digest TYPE string,
          ls_reference TYPE zcl_abapgit_oci_reference=>ty_reference,
          ls_snapshot TYPE zif_abapgit_repo_connector=>ty_snapshot,
          ls_call TYPE lcl_oci_http_agent=>ty_call,
          lo_agent TYPE REF TO lcl_oci_http_agent,
          lo_client TYPE REF TO zcl_abapgit_oci_client.

    fixture(
      IMPORTING
        ev_manifest = lv_manifest
        ev_layer = lv_layer
        ev_manifest_digest = lv_digest ).
    CREATE OBJECT lo_agent
      EXPORTING
        iv_manifest = lv_manifest
        iv_layer = lv_layer
        iv_manifest_digest = lv_digest
        iv_require_bearer = abap_true
        iv_redirect_blob = abap_true.
    CREATE OBJECT lo_client EXPORTING ii_http_agent = lo_agent.
    ls_reference = zcl_abapgit_oci_reference=>parse( 'oci://registry.example.com/team/library:v1' ).

    ls_snapshot = lo_client->fetch( ls_reference-canonical ).

    cl_abap_unit_assert=>assert_equals( act = ls_snapshot-resolved_revision
                                        exp = lv_digest ).
    cl_abap_unit_assert=>assert_equals( act = lines( lo_agent->mt_calls )
                                        exp = 6 ).
    READ TABLE lo_agent->mt_calls INTO ls_call INDEX 6.
    cl_abap_unit_assert=>assert_equals( act = ls_call-url
                                        exp = 'https://cdn.example.net/blobdata' ).
    cl_abap_unit_assert=>assert_initial( ls_call-authorization ).
  ENDMETHOD.

  METHOD redirect_drops_port_auth.
    DATA: lv_manifest TYPE xstring,
          lv_layer TYPE xstring,
          lv_digest TYPE string,
          ls_reference TYPE zcl_abapgit_oci_reference=>ty_reference,
          ls_snapshot TYPE zif_abapgit_repo_connector=>ty_snapshot,
          ls_call TYPE lcl_oci_http_agent=>ty_call,
          lo_agent TYPE REF TO lcl_oci_http_agent,
          lo_client TYPE REF TO zcl_abapgit_oci_client.

    fixture(
      IMPORTING
        ev_manifest = lv_manifest
        ev_layer = lv_layer
        ev_manifest_digest = lv_digest ).
    zcl_abapgit_login_manager=>set_basic(
      iv_uri      = 'https://registry.example.com:5000/v2/team/library/manifests/v1'
      iv_username = 'test-user'
      iv_password = 'test-password' ).
    CREATE OBJECT lo_agent
      EXPORTING
        iv_manifest = lv_manifest
        iv_layer = lv_layer
        iv_manifest_digest = lv_digest
        iv_redirect_blob = abap_true
        iv_redirect_location = 'https://registry.example.com:5001/blobdata'.
    CREATE OBJECT lo_client EXPORTING ii_http_agent = lo_agent.
    ls_reference = zcl_abapgit_oci_reference=>parse( 'oci://registry.example.com:5000/team/library:v1' ).

    ls_snapshot = lo_client->fetch( ls_reference-canonical ).

    cl_abap_unit_assert=>assert_equals( act = ls_snapshot-resolved_revision
                                        exp = lv_digest ).
    cl_abap_unit_assert=>assert_equals( act = lines( lo_agent->mt_calls )
                                        exp = 3 ).
    READ TABLE lo_agent->mt_calls INTO ls_call INDEX 1.
    IF ls_call-authorization NP 'Basic *'.
      cl_abap_unit_assert=>fail( ).
    ENDIF.
    READ TABLE lo_agent->mt_calls INTO ls_call INDEX 2.
    IF ls_call-authorization NP 'Basic *'.
      cl_abap_unit_assert=>fail( ).
    ENDIF.
    READ TABLE lo_agent->mt_calls INTO ls_call INDEX 3.
    cl_abap_unit_assert=>assert_equals(
      act = ls_call-url
      exp = 'https://registry.example.com:5001/blobdata' ).
    cl_abap_unit_assert=>assert_initial( ls_call-authorization ).
  ENDMETHOD.

  METHOD fixture.
    DATA: lv_manifest_text TYPE string,
          lv_config_data TYPE xstring,
          lv_config_digest TYPE string.
    ev_layer = make_tar( ).
    lv_config_data = '7B7D'.
    lv_config_digest = zcl_abapgit_hash=>sha256_raw( lv_config_data ).
    lv_manifest_text = '{"schemaVersion":2,"mediaType":"application/vnd.oci.image.manifest.v1+json",' &&
      '"artifactType":"application/vnd.abapgit.repository.v1",' &&
      '"config":{"mediaType":"application/vnd.oci.empty.v1+json","digest":"sha256:' &&
      lv_config_digest && '","size":2},' &&
      '"layers":[{"mediaType":"application/vnd.oci.image.layer.v1.tar","digest":"sha256:' &&
      zcl_abapgit_hash=>sha256_raw( ev_layer ) && '","size":' &&
      |{ xstrlen( ev_layer ) }| && '}]}'.
    ev_manifest = zcl_abapgit_convert=>string_to_xstring_utf8( lv_manifest_text ).
    ev_manifest_digest = |sha256:{ zcl_abapgit_hash=>sha256_raw( ev_manifest ) }|.
  ENDMETHOD.

  METHOD make_tar.
    DATA: lv_header TYPE xstring,
          lv_payload TYPE xstring,
          lv_padding TYPE xstring,
          lv_end_marker TYPE xstring,
          lv_checksum TYPE i,
          lv_index TYPE i,
          lv_byte TYPE xstring,
          lv_space TYPE x LENGTH 1 VALUE '20',
          lv_checksum_text TYPE string.

    lv_header = make_header( ).
    DO 512 TIMES.
      lv_index = sy-index - 1.
      IF lv_index >= 148 AND lv_index < 156.
        lv_checksum = lv_checksum + 32.
      ELSE.
        lv_byte = lv_header+lv_index(1).
        lv_checksum = lv_checksum + zcl_abapgit_convert=>xstring_to_int( lv_byte ).
      ENDIF.
    ENDDO.
    lv_checksum_text = octal_text( iv_number = lv_checksum
                                   iv_width = 6 ).
    lv_byte = zcl_abapgit_convert=>string_to_xstring_utf8( lv_checksum_text ).
    CONCATENATE lv_byte lv_space lv_space INTO lv_byte IN BYTE MODE.
    set_field( EXPORTING iv_offset = 148
                         iv_length = 8
                         iv_value = lv_byte
               CHANGING  cv_data = lv_header ).

    lv_payload = zcl_abapgit_convert=>string_to_xstring_utf8( '<x/>' ).
    lv_padding = zero_bytes( 508 ).
    lv_end_marker = zero_bytes( 1024 ).
    rv_tar = lv_header.
    CONCATENATE rv_tar lv_payload lv_padding lv_end_marker INTO rv_tar IN BYTE MODE.
  ENDMETHOD.

  METHOD make_header.
    rv_header = zero_bytes( 512 ).
    set_text( EXPORTING iv_offset = 0
                        iv_length = 100
                        iv_value = '.abapgit.xml' CHANGING cv_data = rv_header ).
    set_text( EXPORTING iv_offset = 100
                        iv_length = 8
                        iv_value = '0000644' CHANGING cv_data = rv_header ).
    set_text( EXPORTING iv_offset = 108
                        iv_length = 8
                        iv_value = '0000000' CHANGING cv_data = rv_header ).
    set_text( EXPORTING iv_offset = 116
                        iv_length = 8
                        iv_value = '0000000' CHANGING cv_data = rv_header ).
    set_text( EXPORTING iv_offset = 124
                        iv_length = 12
                        iv_value = octal_text( iv_number = 4 iv_width = 11 ) CHANGING cv_data = rv_header ).
    set_text( EXPORTING iv_offset = 136
                        iv_length = 12
                        iv_value = '00000000000' CHANGING cv_data = rv_header ).
    set_text( EXPORTING iv_offset = 148
                        iv_length = 8
                        iv_value = '        ' CHANGING cv_data = rv_header ).
    set_text( EXPORTING iv_offset = 156
                        iv_length = 1
                        iv_value = '0' CHANGING cv_data = rv_header ).
    set_text( EXPORTING iv_offset = 257
                        iv_length = 6
                        iv_value = 'ustar' CHANGING cv_data = rv_header ).
    set_text( EXPORTING iv_offset = 263
                        iv_length = 2
                        iv_value = '00' CHANGING cv_data = rv_header ).
  ENDMETHOD.

  METHOD set_text.
    set_field(
      EXPORTING
        iv_offset = iv_offset
        iv_length = iv_length
        iv_value = zcl_abapgit_convert=>string_to_xstring_utf8( iv_value )
      CHANGING
        cv_data = cv_data ).
  ENDMETHOD.

  METHOD set_field.
    DATA: lv_original_size TYPE i,
          lv_after_offset TYPE i,
          lv_before TYPE xstring,
          lv_after TYPE xstring,
          lv_field TYPE xstring,
          lv_padding TYPE xstring,
          lv_result TYPE xstring.
    lv_original_size = xstrlen( cv_data ).
    IF iv_offset < 0 OR iv_length < 0 OR iv_offset > lv_original_size OR
       iv_length > lv_original_size - iv_offset OR xstrlen( iv_value ) > iv_length.
      zcx_abapgit_exception=>raise( 'Invalid field placement in OCI test TAR fixture' ).
    ENDIF.
    lv_field = iv_value.
    lv_padding = zero_bytes( iv_length - xstrlen( iv_value ) ).
    CONCATENATE lv_field lv_padding INTO lv_field IN BYTE MODE.
    IF iv_offset > 0.
      lv_before = cv_data+0(iv_offset).
    ENDIF.
    lv_after_offset = iv_offset + iv_length.
    IF lv_after_offset < lv_original_size.
      lv_after = cv_data+lv_after_offset.
    ENDIF.
    CONCATENATE lv_before lv_field lv_after INTO lv_result IN BYTE MODE.
    cv_data = lv_result.
  ENDMETHOD.

  METHOD zero_bytes.
    DATA lv_zero TYPE x LENGTH 1 VALUE '00'.
    DO iv_length TIMES.
      CONCATENATE rv_bytes lv_zero INTO rv_bytes IN BYTE MODE.
    ENDDO.
  ENDMETHOD.

  METHOD octal_text.
    DATA: lv_number TYPE i,
          lv_digit TYPE i,
          lv_char TYPE c LENGTH 1.
    lv_number = iv_number.
    WHILE lv_number > 0.
      lv_digit = lv_number MOD 8.
      lv_char = lv_digit.
      rv_text = lv_char && rv_text.
      lv_number = lv_number DIV 8.
    ENDWHILE.
    IF rv_text IS INITIAL.
      rv_text = '0'.
    ENDIF.
    WHILE strlen( rv_text ) < iv_width.
      rv_text = '0' && rv_text.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.
