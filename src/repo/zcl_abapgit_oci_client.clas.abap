CLASS zcl_abapgit_oci_client DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        ii_http_agent TYPE REF TO zif_abapgit_http_agent OPTIONAL.
    METHODS fetch
      IMPORTING
        iv_reference       TYPE string
      RETURNING
        VALUE(rs_snapshot) TYPE zif_abapgit_repo_connector=>ty_snapshot
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
    CONSTANTS:
      c_manifest_media_type TYPE string VALUE 'application/vnd.oci.image.manifest.v1+json',
      c_artifact_type       TYPE string VALUE 'application/vnd.abapgit.repository.v1',
      c_layer_media_type    TYPE string VALUE 'application/vnd.oci.image.layer.v1.tar',
      c_max_manifest_size   TYPE i VALUE 2097152,
      c_max_layer_size      TYPE i VALUE 52428800.

    DATA mi_http_agent TYPE REF TO zif_abapgit_http_agent.
    DATA mv_cached_token TYPE string.
    DATA mv_cached_service TYPE string.
    DATA mv_cached_scope TYPE string.
    DATA mv_cached_registry TYPE string.
    DATA mv_cached_realm TYPE string.
    DATA mv_token_expires_at TYPE timestampl.

    METHODS get_data
      IMPORTING
        iv_url      TYPE string
        iv_registry TYPE string
        iv_accept   TYPE string
        iv_scope    TYPE string
      EXPORTING
        eo_headers  TYPE REF TO zcl_abapgit_string_map
        ev_data     TYPE xstring
      RAISING
        zcx_abapgit_exception.
    METHODS request_get
      IMPORTING
        iv_url             TYPE string
        iv_accept          TYPE string OPTIONAL
        iv_authorization   TYPE string OPTIONAL
        iv_auth_origin     TYPE string OPTIONAL
        ii_query           TYPE REF TO zcl_abapgit_string_map OPTIONAL
        iv_follow_redirect TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_response) TYPE REF TO zif_abapgit_http_response
      RAISING
        zcx_abapgit_exception.
    METHODS get_token
      IMPORTING
        iv_registry     TYPE string
        iv_realm        TYPE string
        iv_service      TYPE string
        iv_scope        TYPE string
      RETURNING
        VALUE(rv_token) TYPE string
      RAISING
        zcx_abapgit_exception.
    METHODS parse_json
      IMPORTING
        iv_data        TYPE xstring
      RETURNING
        VALUE(ri_json) TYPE REF TO zif_abapgit_ajson
      RAISING
        zcx_abapgit_exception.
    METHODS challenge_parameter
      IMPORTING
        iv_challenge    TYPE string
        iv_name         TYPE string
      RETURNING
        VALUE(rv_value) TYPE string.
    METHODS header_value
      IMPORTING
        io_headers      TYPE REF TO zcl_abapgit_string_map
        iv_name         TYPE string
      RETURNING
        VALUE(rv_value) TYPE string.
    METHODS origin
      IMPORTING
        iv_url           TYPE string
      RETURNING
        VALUE(rv_origin) TYPE string
      RAISING
        zcx_abapgit_exception.
    METHODS resolve_location
      IMPORTING
        iv_url        TYPE string
        iv_location   TYPE string
      RETURNING
        VALUE(rv_url) TYPE string
      RAISING
        zcx_abapgit_exception.
    METHODS validate_digest
      IMPORTING
        iv_digest  TYPE string
        iv_context TYPE string
      RAISING
        zcx_abapgit_exception.
ENDCLASS.


CLASS zcl_abapgit_oci_client IMPLEMENTATION.

  METHOD constructor.
    mi_http_agent = ii_http_agent.
    IF mi_http_agent IS NOT BOUND.
      mi_http_agent = zcl_abapgit_http_agent=>create( ).
    ENDIF.
  ENDMETHOD.


  METHOD fetch.

    DATA: lv_registry_url TYPE string,
          lv_manifest_url TYPE string,
          lv_layer_url TYPE string,
          lv_manifest_data TYPE xstring,
          lv_manifest_digest TYPE string,
          lv_layer_digest TYPE string,
          lv_layer_data TYPE xstring,
          lv_manifest_media_type TYPE string,
          lv_artifact_type TYPE string,
          lv_layer_media_type TYPE string,
          lv_layer_size TYPE i,
          lv_layer_length TYPE i,
          lo_headers TYPE REF TO zcl_abapgit_string_map,
          lo_manifest TYPE REF TO zif_abapgit_ajson,
          ls_reference TYPE zcl_abapgit_oci_reference=>ty_reference.

    ls_reference = zcl_abapgit_oci_reference=>parse( iv_reference ).

    lv_registry_url = |https://{ ls_reference-registry }|.
    lv_manifest_url = |{ lv_registry_url }/v2/{ ls_reference-repository }/manifests/{ ls_reference-reference }|.
    get_data(
      EXPORTING
        iv_url      = lv_manifest_url
        iv_registry = ls_reference-registry
        iv_accept   = c_manifest_media_type
        iv_scope    = |repository:{ ls_reference-repository }:pull|
      IMPORTING
        eo_headers = lo_headers
        ev_data    = lv_manifest_data ).

    IF xstrlen( lv_manifest_data ) > c_max_manifest_size.
      zcx_abapgit_exception=>raise( |OCI manifest exceeds the { c_max_manifest_size } byte limit| ).
    ENDIF.

    lv_manifest_media_type = header_value(
      io_headers = lo_headers
      iv_name    = 'content-type' ).
    SPLIT lv_manifest_media_type AT ';' INTO lv_manifest_media_type lv_layer_url.
    CONDENSE lv_manifest_media_type.
    TRANSLATE lv_manifest_media_type TO LOWER CASE.
    IF lv_manifest_media_type <> c_manifest_media_type.
      zcx_abapgit_exception=>raise(
        |OCI registry returned unsupported manifest media type "{ lv_manifest_media_type }"| ).
    ENDIF.

    lv_manifest_digest = |sha256:{ zcl_abapgit_hash=>sha256_raw( lv_manifest_data ) }|.
    IF ls_reference-is_digest = abap_true AND lv_manifest_digest <> ls_reference-reference.
      zcx_abapgit_exception=>raise( 'OCI manifest digest does not match the requested digest' ).
    ENDIF.
    IF header_value( io_headers = lo_headers
                     iv_name = 'docker-content-digest' ) IS NOT INITIAL AND
       header_value( io_headers = lo_headers
                     iv_name = 'docker-content-digest' ) <> lv_manifest_digest.
      zcx_abapgit_exception=>raise( 'OCI manifest digest does not match Docker-Content-Digest' ).
    ENDIF.

    lo_manifest = parse_json( lv_manifest_data ).

    IF lo_manifest->get_integer( '/schemaVersion' ) <> 2 OR
       lo_manifest->get_string( '/mediaType' ) <> c_manifest_media_type.
      zcx_abapgit_exception=>raise( 'OCI response is not a supported image manifest' ).
    ENDIF.

    lv_artifact_type = lo_manifest->get_string( '/artifactType' ).
    IF lv_artifact_type IS INITIAL.
      lv_artifact_type = lo_manifest->get_string( '/config/mediaType' ).
    ENDIF.
    IF lv_artifact_type <> c_artifact_type.
      zcx_abapgit_exception=>raise( |OCI artifact identity is unsupported; expected { c_artifact_type }| ).
    ENDIF.

    IF lines( lo_manifest->members( '/layers' ) ) <> 1.
      zcx_abapgit_exception=>raise( 'OCI artifact must contain exactly one filesystem layer' ).
    ENDIF.

    lv_layer_media_type = lo_manifest->get_string( '/layers/1/mediaType' ).
    IF lv_layer_media_type <> c_layer_media_type.
      zcx_abapgit_exception=>raise(
        |OCI layer media type "{ lv_layer_media_type }" is unsupported; expected an uncompressed USTAR layer| ).
    ENDIF.
    lv_layer_digest = lo_manifest->get_string( '/layers/1/digest' ).
    validate_digest( iv_digest = lv_layer_digest
                     iv_context = 'OCI layer descriptor' ).
    lv_layer_size = lo_manifest->get_integer( '/layers/1/size' ).
    IF lv_layer_size < 1024 OR lv_layer_size > c_max_layer_size.
      zcx_abapgit_exception=>raise( |OCI layer size must be between 1024 and { c_max_layer_size } bytes| ).
    ENDIF.

    lv_layer_url = |{ lv_registry_url }/v2/{ ls_reference-repository }/blobs/{ lv_layer_digest }|.
    get_data(
      EXPORTING
        iv_url      = lv_layer_url
        iv_registry = ls_reference-registry
        iv_accept   = c_layer_media_type
        iv_scope    = |repository:{ ls_reference-repository }:pull|
      IMPORTING
        eo_headers = lo_headers
        ev_data    = lv_layer_data ).

    lv_layer_length = xstrlen( lv_layer_data ).
    IF lv_layer_length <> lv_layer_size.
      zcx_abapgit_exception=>raise(
        |OCI layer size mismatch: expected { lv_layer_size }, received { lv_layer_length }| ).
    ENDIF.
    IF |sha256:{ zcl_abapgit_hash=>sha256_raw( lv_layer_data ) }| <> lv_layer_digest.
      zcx_abapgit_exception=>raise( 'OCI layer digest does not match its descriptor' ).
    ENDIF.

    rs_snapshot-files = zcl_abapgit_tar=>decode(
      iv_tar                 = lv_layer_data
      iv_require_repo_marker = abap_true ).
    rs_snapshot-resolved_revision = lv_manifest_digest.

  ENDMETHOD.


  METHOD get_data.

    DATA: lv_registry_url TYPE string,
          lv_authorization TYPE string,
          lv_challenge TYPE string,
          lv_realm TYPE string,
          lv_service TYPE string,
          lv_scope TYPE string,
          lv_token TYPE string,
          lv_status TYPE i,
          lo_response TYPE REF TO zif_abapgit_http_response,
          lo_response_headers TYPE REF TO zcl_abapgit_string_map,
          lx_response TYPE REF TO zcx_abapgit_exception.

    lv_registry_url = |https://{ iv_registry }|.
    lv_authorization = zcl_abapgit_login_manager=>load( iv_url ).
    lo_response = request_get(
      iv_url           = iv_url
      iv_accept        = iv_accept
      iv_authorization = lv_authorization
      iv_auth_origin   = origin( lv_registry_url ) ).

    lv_status = lo_response->code( ).
    IF lv_status = 401.
      TRY.
          lo_response_headers = lo_response->headers( ).
        CATCH zcx_abapgit_exception INTO lx_response.
          lo_response->close( ).
          CLEAR lo_response.
          RAISE EXCEPTION lx_response.
      ENDTRY.
      lv_challenge = header_value(
        io_headers = lo_response_headers
        iv_name    = 'www-authenticate' ).
      lo_response->close( ).
      CLEAR lo_response.

      IF lv_challenge CP 'Bearer *'.
        lv_realm = challenge_parameter( iv_challenge = lv_challenge
                                        iv_name = 'realm' ).
        lv_service = challenge_parameter( iv_challenge = lv_challenge
                                          iv_name = 'service' ).
        lv_scope = challenge_parameter( iv_challenge = lv_challenge
                                        iv_name = 'scope' ).
        IF lv_scope IS NOT INITIAL AND lv_scope <> iv_scope.
          zcx_abapgit_exception=>raise( 'OCI registry requested a scope other than repository pull' ).
        ENDIF.
        IF lv_scope IS INITIAL.
          lv_scope = iv_scope.
        ENDIF.
        IF lv_realm IS INITIAL OR origin( lv_realm ) IS INITIAL.
          zcx_abapgit_exception=>raise( 'OCI Bearer challenge has no valid HTTPS token realm' ).
        ENDIF.

        lv_token = get_token(
          iv_registry = iv_registry
          iv_realm    = lv_realm
          iv_service  = lv_service
          iv_scope    = lv_scope ).
        lo_response = request_get(
          iv_url           = iv_url
          iv_accept        = iv_accept
          iv_authorization = |Bearer { lv_token }|
          iv_auth_origin   = origin( lv_registry_url ) ).
        IF lo_response->code( ) = 401.
          lo_response->close( ).
          CLEAR lo_response.
          CLEAR: mv_cached_token, mv_cached_service, mv_cached_scope,
                 mv_cached_registry, mv_cached_realm, mv_token_expires_at.
          lv_token = get_token(
            iv_registry = iv_registry
            iv_realm    = lv_realm
            iv_service  = lv_service
            iv_scope    = lv_scope ).
          lo_response = request_get(
            iv_url           = iv_url
            iv_accept        = iv_accept
            iv_authorization = |Bearer { lv_token }|
            iv_auth_origin   = origin( lv_registry_url ) ).
        ENDIF.
      ELSE.
        IF lv_authorization IS NOT INITIAL.
          zcl_abapgit_login_manager=>remove( iv_url ).
        ENDIF.
        RAISE EXCEPTION TYPE zcx_abapgit_auth_required
          EXPORTING
            iv_url = iv_url.
      ENDIF.
    ENDIF.

    IF lo_response->code( ) <> 200.
      lv_status = lo_response->code( ).
      lo_response->close( ).
      CLEAR lo_response.
      IF lv_status = 401 AND lv_authorization IS NOT INITIAL.
        zcl_abapgit_login_manager=>remove( iv_url ).
        RAISE EXCEPTION TYPE zcx_abapgit_auth_required
          EXPORTING
            iv_url = iv_url.
      ENDIF.
      zcx_abapgit_exception=>raise( |OCI GET failed with HTTP { lv_status } for { iv_url }| ).
    ENDIF.

    TRY.
        eo_headers = lo_response->headers( ).
        ev_data = lo_response->data( ).
      CATCH zcx_abapgit_exception INTO lx_response.
        lo_response->close( ).
        CLEAR lo_response.
        RAISE EXCEPTION lx_response.
    ENDTRY.
    lo_response->close( ).
    CLEAR lo_response.

  ENDMETHOD.


  METHOD request_get.

    DATA: lv_url TYPE string,
          lv_location TYPE string,
          lv_origin TYPE string,
          lv_auth TYPE string,
          lv_status TYPE i,
          lo_headers TYPE REF TO zcl_abapgit_string_map,
          lx_response TYPE REF TO zcx_abapgit_exception.
    DATA lv_hop TYPE i.

    lv_url = iv_url.
    DO 4 TIMES.
      CREATE OBJECT lo_headers EXPORTING iv_case_insensitive = abap_true.
      IF iv_accept IS NOT INITIAL.
        lo_headers->set( iv_key = 'Accept'
                         iv_val = iv_accept ).
      ENDIF.

      lv_origin = origin( lv_url ).
      IF iv_authorization IS NOT INITIAL AND lv_origin = iv_auth_origin.
        lv_auth = iv_authorization.
        lo_headers->set( iv_key = 'Authorization'
                         iv_val = lv_auth ).
      ENDIF.

      ri_response = mi_http_agent->request(
        iv_url              = lv_url
        iv_method           = zif_abapgit_http_agent=>c_methods-get
        io_query            = ii_query
        io_headers          = lo_headers
        iv_follow_redirect  = iv_follow_redirect ).
      lv_status = ri_response->code( ).
      IF lv_status <> 301 AND lv_status <> 302 AND lv_status <> 303 AND
         lv_status <> 307 AND lv_status <> 308.
        RETURN.
      ENDIF.

      IF lv_hop >= 3.
        ri_response->close( ).
        zcx_abapgit_exception=>raise( 'OCI registry redirect limit exceeded' ).
      ENDIF.
      lv_hop = lv_hop + 1.
      TRY.
          lo_headers = ri_response->headers( ).
        CATCH zcx_abapgit_exception INTO lx_response.
          ri_response->close( ).
          CLEAR ri_response.
          RAISE EXCEPTION lx_response.
      ENDTRY.
      lv_location = header_value( io_headers = lo_headers
                                  iv_name = 'location' ).
      ri_response->close( ).
      CLEAR ri_response.
      IF lv_location IS INITIAL.
        zcx_abapgit_exception=>raise( 'OCI registry redirect omitted its Location header' ).
      ENDIF.
      lv_url = resolve_location( iv_url = lv_url
                                 iv_location = lv_location ).
    ENDDO.

  ENDMETHOD.


  METHOD get_token.

    DATA: lo_query TYPE REF TO zcl_abapgit_string_map,
          lo_response TYPE REF TO zif_abapgit_http_response,
          lo_json TYPE REF TO zif_abapgit_ajson,
          lv_registry_url TYPE string,
          lv_auth_url TYPE string,
          lv_authorization TYPE string,
          lv_token TYPE string,
          lv_data TYPE xstring,
          lv_expiration TYPE i,
          lv_now TYPE timestampl,
          lx_json TYPE REF TO zcx_abapgit_ajson_error,
          lx_token_parse TYPE REF TO zcx_abapgit_exception,
          lx_tstmp TYPE REF TO cx_root.

    GET TIME STAMP FIELD lv_now.
    IF mv_cached_token IS NOT INITIAL AND mv_cached_service = iv_service AND
       mv_cached_scope = iv_scope AND mv_cached_registry = origin( |https://{ iv_registry }| ) AND
       mv_cached_realm = origin( iv_realm ) AND mv_token_expires_at > lv_now.
      rv_token = mv_cached_token.
      RETURN.
    ENDIF.

    lv_registry_url = |https://{ iv_registry }|.
    CREATE OBJECT lo_query.
    IF iv_service IS NOT INITIAL.
      lo_query->set( iv_key = 'service'
                     iv_val = iv_service ).
    ENDIF.
    lo_query->set( iv_key = 'scope'
                   iv_val = iv_scope ).

    " An external token realm uses credentials only after the user has separately entered them.
    lv_authorization = zcl_abapgit_login_manager=>load( iv_realm ).
    lv_auth_url = iv_realm.

    lo_response = request_get(
      iv_url           = iv_realm
      iv_authorization = lv_authorization
      iv_auth_origin   = origin( iv_realm )
      ii_query         = lo_query ).
    IF lo_response->code( ) <> 200.
      lv_expiration = lo_response->code( ).
      lo_response->close( ).
      CLEAR lo_response.
      IF lv_expiration = 401.
        IF lv_authorization IS NOT INITIAL.
          zcl_abapgit_login_manager=>remove( lv_auth_url ).
        ENDIF.
        RAISE EXCEPTION TYPE zcx_abapgit_auth_required
          EXPORTING
            iv_url = lv_auth_url.
      ENDIF.
      zcx_abapgit_exception=>raise( |OCI token request failed with HTTP { lv_expiration }| ).
    ENDIF.

    lv_data = lo_response->data( ).
    IF xstrlen( lv_data ) > 1048576.
      lo_response->close( ).
      CLEAR lo_response.
      zcx_abapgit_exception=>raise( 'OCI token response exceeds the 1 MiB limit' ).
    ENDIF.

    TRY.
        lo_json = zcl_abapgit_ajson=>parse( zcl_abapgit_convert=>xstring_to_string_utf8( lv_data ) ).
      CATCH zcx_abapgit_ajson_error INTO lx_json.
        lo_response->close( ).
        CLEAR lo_response.
        zcx_abapgit_exception=>raise( |OCI token response is invalid JSON: { lx_json->get_text( ) }| ).
      CATCH zcx_abapgit_exception INTO lx_token_parse.
        lo_response->close( ).
        CLEAR lo_response.
        zcx_abapgit_exception=>raise( |OCI token response is not valid UTF-8: { lx_token_parse->get_text( ) }| ).
    ENDTRY.
    lo_response->close( ).
    CLEAR lo_response.

    lv_token = lo_json->get_string( '/token' ).
    IF lv_token IS INITIAL.
      lv_token = lo_json->get_string( '/access_token' ).
    ENDIF.
    IF lv_token IS INITIAL.
      zcx_abapgit_exception=>raise( 'OCI token response contains neither token nor access_token' ).
    ENDIF.
    FIND REGEX '^[A-Za-z0-9._~+/-]+=*$' IN lv_token.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'OCI token contains unsupported characters' ).
    ENDIF.

    lv_expiration = lo_json->get_integer( '/expires_in' ).
    IF lv_expiration <= 0.
      lv_expiration = 300.
    ENDIF.
    IF lv_expiration > 3600.
      lv_expiration = 3600.
    ENDIF.
    IF lv_expiration > 30.
      lv_expiration = lv_expiration - 30.
    ENDIF.
    TRY.
        mv_token_expires_at = cl_abap_tstmp=>add(
          tstmp = lv_now
          secs  = lv_expiration ).
      CATCH cx_parameter_invalid_range cx_parameter_invalid_type INTO lx_tstmp.
        zcx_abapgit_exception=>raise( |Cannot calculate OCI token expiration: { lx_tstmp->get_text( ) }| ).
    ENDTRY.

    mv_cached_token = lv_token.
    mv_cached_service = iv_service.
    mv_cached_scope = iv_scope.
    mv_cached_registry = origin( lv_registry_url ).
    mv_cached_realm = origin( iv_realm ).
    rv_token = lv_token.

  ENDMETHOD.


  METHOD parse_json.
    DATA: lx_json TYPE REF TO zcx_abapgit_ajson_error,
          lx_convert TYPE REF TO zcx_abapgit_exception.

    TRY.
        ri_json = zcl_abapgit_ajson=>parse( zcl_abapgit_convert=>xstring_to_string_utf8( iv_data ) ).
      CATCH zcx_abapgit_ajson_error INTO lx_json.
        zcx_abapgit_exception=>raise( |OCI manifest JSON is invalid: { lx_json->get_text( ) }| ).
      CATCH zcx_abapgit_exception INTO lx_convert.
        zcx_abapgit_exception=>raise( |OCI manifest is not valid UTF-8: { lx_convert->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.


  METHOD challenge_parameter.
    DATA: lv_prefix TYPE string,
          lv_rest TYPE string,
          lv_offset TYPE i,
          lv_end TYPE i,
          lv_start TYPE i.

    lv_prefix = |{ iv_name }="|.
    FIND FIRST OCCURRENCE OF lv_prefix IN iv_challenge MATCH OFFSET lv_offset.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    lv_start = lv_offset + strlen( lv_prefix ).
    lv_rest = iv_challenge+lv_start.
    FIND FIRST OCCURRENCE OF '"' IN lv_rest MATCH OFFSET lv_end.
    IF sy-subrc = 0.
      rv_value = lv_rest(lv_end).
    ENDIF.
  ENDMETHOD.


  METHOD header_value.
    FIELD-SYMBOLS <ls_entry> LIKE LINE OF io_headers->mt_entries.

    IF io_headers IS NOT BOUND.
      RETURN.
    ENDIF.
    LOOP AT io_headers->mt_entries ASSIGNING <ls_entry>.
      IF to_lower( <ls_entry>-k ) = to_lower( iv_name ).
        rv_value = <ls_entry>-v.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD origin.
    DATA: lv_authority TYPE string,
          lv_offset TYPE i,
          lv_port_offset TYPE i,
          lv_port TYPE string,
          lv_port_value TYPE i.

    IF NOT iv_url CP 'https://*' OR iv_url CS '@' OR iv_url CS '#'.
      zcx_abapgit_exception=>raise( 'OCI HTTP endpoints must use HTTPS without user information or fragments' ).
    ENDIF.
    lv_authority = iv_url+8.
    FIND FIRST OCCURRENCE OF '/' IN lv_authority MATCH OFFSET lv_offset.
    IF sy-subrc = 0.
      lv_authority = lv_authority(lv_offset).
    ELSE.
      FIND FIRST OCCURRENCE OF '?' IN lv_authority MATCH OFFSET lv_offset.
      IF sy-subrc = 0.
        lv_authority = lv_authority(lv_offset).
      ENDIF.
    ENDIF.
    IF lv_authority IS INITIAL.
      zcx_abapgit_exception=>raise( 'OCI HTTP endpoint has an empty host' ).
    ENDIF.

    FIND REGEX '^[A-Za-z0-9.-]+(:[0-9]{1,5})?$' IN lv_authority.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'OCI HTTP endpoint has a malformed host or port' ).
    ENDIF.
    FIND FIRST OCCURRENCE OF ':' IN lv_authority MATCH OFFSET lv_port_offset.
    IF sy-subrc = 0.
      lv_offset = lv_port_offset.
      lv_port_offset = lv_port_offset + 1.
      lv_port = lv_authority+lv_port_offset.
      lv_port_value = lv_port.
      IF lv_port_value < 1 OR lv_port_value > 65535.
        zcx_abapgit_exception=>raise( 'OCI HTTP endpoint port is outside the TCP port range' ).
      ENDIF.
      lv_authority = lv_authority(lv_offset).
    ENDIF.
    TRANSLATE lv_authority TO LOWER CASE.
    IF lv_port_value IS INITIAL OR lv_port_value = 443.
      rv_origin = |https://{ lv_authority }|.
    ELSE.
      rv_origin = |https://{ lv_authority }:{ lv_port }|.
    ENDIF.
  ENDMETHOD.


  METHOD resolve_location.
    DATA lv_origin TYPE string.

    IF iv_location CP 'https://*'.
      rv_url = iv_location.
    ELSEIF iv_location CP '/*'.
      lv_origin = origin( iv_url ).
      rv_url = lv_origin && iv_location.
    ELSE.
      zcx_abapgit_exception=>raise( 'OCI registry redirect must use an absolute HTTPS URL or absolute path' ).
    ENDIF.
    origin( rv_url ).
  ENDMETHOD.


  METHOD validate_digest.
    FIND REGEX '^sha256:[0-9a-f]{64}$' IN iv_digest.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |{ iv_context } must contain a lowercase SHA-256 digest| ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
