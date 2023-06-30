CLASS zcl_abapgit_exit DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_exit .
  types:
    BEGIN OF ts_rfc_definition,
        destination             TYPE  rfcdes-rfcdest,
        servicenr               TYPE rfcdisplay-rfcsysid,
        server                  TYPE rfcdisplay-rfchost,
        trace                   TYPE rfcdisplay-rfctrace,
        language                TYPE rfcdisplay-rfclang,
        client                  TYPE rfcdisplay-rfcclient,
        user                    TYPE rfcdisplay-rfcalias,
        password                TYPE rfcdisplay-rfcexec,
        description             TYPE rfcdoc-rfcdoc1,
        proxy_host              TYPE rfcdisplay-rfcgwhost,
        proxy_service           TYPE rfcdisplay-rfcgwserv,
        proxy_user              TYPE rfcdisplay-proxyuser,
        proxy_password          TYPE string,
        sslapplic               TYPE rfcdisplay-sslapplic,
        path_prefix             TYPE string,
        destlock                TYPE rfcdisplay-rfclock,
        authority               TYPE destauth,
        slogin                  TYPE rfcdisplay-rfcslogin,
        sameusr                 TYPE rfcdisplay-rfcsameusr,
        ssl                     TYPE rfcdisplay-rfcsnc,
        http_timeout            TYPE i,
        http_version            TYPE rfcdisplay-rfctype,
        http_compress           TYPE rfcdisplay-rfctype,
        compressreply           TYPE rfcdisplay-rfctype,
        cookie_accept           TYPE rfcdisplay-rfctype,
        sso_ticket              TYPE rfcdisplay-rfctsysopt,
        category                TYPE rfcdisplay-rfccategory,
        assertion_ticket        TYPE assertion_ticket,
        assertion_ticket_sysid  TYPE assertion_ticket_sysid,
        assertion_ticket_client TYPE assertion_ticket_client,
        user_long254            TYPE rfcdisplaz-http_ext_user,
        scc_enabled             TYPE rfcdisplay-scc_enabled,
        mqtt_user               TYPE rfcdisplay-mqttuser,
        mqtt_pw                 TYPE rfcdisplay-mqttpw,
        scc_location_id         TYPE rfcdisplay-scc_location_id,
        no_client_cert          TYPE rfcdisplay-noccert,
        oauth_used              TYPE char1,
        oauth_profile           TYPE oa2c_profile,
        oauth_config            TYPE oa2c_configuration,
        created_by              TYPE rfcattrib-cuname,
        modified_by             TYPE rfcattrib-muname,
      END OF ts_rfc_definition .
  types:
    tt_rfc_definition TYPE SORTED TABLE OF ts_rfc_definition WITH UNIQUE KEY destination .

  class-data GT_RFC_DESTinations type tt_rfc_definition read-only .
  class-methods CLASS_CONSTRUCTOR .
  class-methods FIND_RFC_DEST_FROM_URI
    importing
      !IV_URL type STRING
    returning
      value(R_DESTINATION) type RFCDEST .
  class-methods FIND_RFC_DEST_FROM_HOST_PATH
    importing
      !IV_HOST type STRING
      !IV_PATH_NAME type STRING
      !IV_USER type SYST-UNAME default '*'
    returning
      value(R_DESTINATION) type RFCDEST .
	  
    CLASS-METHODS get_instance
      RETURNING
        VALUE(ri_exit) TYPE REF TO zif_abapgit_exit .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gi_exit TYPE REF TO zif_abapgit_exit .
ENDCLASS.



CLASS zcl_abapgit_exit IMPLEMENTATION.

METHOD class_constructor.
  SELECT d~rfcdest AS destination, u~cuname AS created_by, u~muname AS modified_by
    INTO CORRESPONDING FIELDS OF TABLE @gt_rfc_destinations
    FROM rfcdes AS d LEFT JOIN rfcattrib AS u ON u~rfcdest = d~rfcdest
    WHERE d~rfctype = 'G'.

  LOOP AT gt_rfc_destinations ASSIGNING FIELD-SYMBOL(<d>).
    CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
      EXPORTING
        destination             = <d>-destination
*       AUTHORITY_CHECK         = 'X'
*       BYPASS_BUF              =
      IMPORTING
*        rfctype                 = <d>-rfctype
        servicenr               = <d>-servicenr
        server                  = <d>-server
        trace                   = <d>-trace
        language                = <d>-language
        client                  = <d>-client
        user                    = <d>-user
        password                = <d>-password
        description             = <d>-description
        proxy_host              = <d>-proxy_host
        proxy_service           = <d>-proxy_service
        proxy_user              = <d>-proxy_user
        proxy_password          = <d>-proxy_password
        sslapplic               = <d>-sslapplic
        path_prefix             = <d>-path_prefix
        destlock                = <d>-destlock
        authority               = <d>-authority
        slogin                  = <d>-slogin
        sameusr                 = <d>-sameusr
        ssl                     = <d>-ssl
        http_timeout            = <d>-http_timeout
        http_version            = <d>-http_version
        http_compress           = <d>-http_compress
        compressreply           = <d>-compressreply
        cookie_accept           = <d>-cookie_accept
        sso_ticket              = <d>-sso_ticket
        category                = <d>-category
        assertion_ticket        = <d>-assertion_ticket
        assertion_ticket_sysid  = <d>-assertion_ticket_sysid
        assertion_ticket_client = <d>-assertion_ticket_client
        user_long254            = <d>-user_long254
        scc_enabled             = <d>-scc_enabled
        mqtt_user               = <d>-mqtt_user
        mqtt_pw                 = <d>-mqtt_pw
        scc_location_id         = <d>-scc_location_id
        no_client_cert          = <d>-no_client_cert
        oauth_used              = <d>-oauth_used
        oauth_profile           = <d>-oauth_profile
        oauth_config            = <d>-oauth_config
      EXCEPTIONS
        authority_not_available = 1
        destination_not_exist   = 2
        information_failure     = 3
        internal_failure        = 4
        no_http_destination     = 5
        OTHERS                  = 6.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  DELETE gt_rfc_destinations WHERE server IS INITIAL.

ENDMETHOD.

  METHOD find_rfc_dest_from_uri.
* Goal : Split URI Into Server and Path
* not working get http or https prefix not real host name.
*    DATA(lv_host) = zcl_abapgit_url=>host( iv_url ).
*    DATA(lv_path_name) = zcl_abapgit_url=>path_name( iv_url ).
*    DATA(lv_name) = zcl_abapgit_url=>name( iv_url ).
* Replaced by REGEX derived from zcl_abapgit_url=>path_name
    DATA: lv_prot TYPE string ##NEEDED.
    DATA: lv_host TYPE string.
    DATA: lv_path_name TYPE string.
    FIND REGEX '(http[s]?)://([^/]*)(.*)' IN iv_url
      SUBMATCHES lv_prot lv_host lv_path_name.
    r_destination = find_rfc_dest_from_host_path( iv_host = lv_host
                                             iv_path_name = lv_path_name
                                                  iv_user = sy-uname ).
    IF r_destination IS INITIAL.
      r_destination = find_rfc_dest_from_host_path( iv_host = lv_host
                                               iv_path_name = lv_path_name ).
    ENDIF.
  ENDMETHOD.

  METHOD find_rfc_dest_from_host_path.
* Look into preloaded RFC destination for external RFC
* BOTH server and PATH MUST Match!
* Need adjustment in zcl_abapgit_http=>create_by_url( )
    LOOP AT gt_rfc_destinations ASSIGNING FIELD-SYMBOL(<d>)
      WHERE server = iv_host
        AND path_prefix = iv_path_name
        AND ( created_by CS iv_user OR modified_by CS iv_user ).
      r_destination = <d>-destination.
      EXIT.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_instance.

    DATA lv_class_name TYPE string.

    lv_class_name = 'ZCL_ABAPGIT_USER_EXIT'.

    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      " Prevent accidental usage of exit handlers in the developer version
      lv_class_name = |\\PROGRAM={ sy-repid }\\CLASS={ lv_class_name }|.
    ENDIF.

    IF gi_exit IS INITIAL.
      TRY.
          CREATE OBJECT gi_exit TYPE (lv_class_name).
        CATCH cx_sy_create_object_error ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    CREATE OBJECT ri_exit TYPE zcl_abapgit_exit.

  ENDMETHOD.


  METHOD zif_abapgit_exit~adjust_display_commit_url.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->adjust_display_commit_url(
            EXPORTING
              iv_repo_url    = iv_repo_url
              iv_repo_name   = iv_repo_name
              iv_repo_key    = iv_repo_key
              iv_commit_hash = iv_commit_hash
            CHANGING
              cv_display_url = cv_display_url ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~adjust_display_filename.

    IF gi_exit IS NOT INITIAL.
      TRY.
          rv_filename = gi_exit->adjust_display_filename( iv_filename ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    IF rv_filename IS INITIAL.
      rv_filename = iv_filename.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~allow_sap_objects.

    IF gi_exit IS NOT INITIAL.
      TRY.
          rv_allowed = gi_exit->allow_sap_objects( ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_local_host.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_local_host( CHANGING ct_hosts = ct_hosts ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_authentication.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_proxy_authentication(
            EXPORTING
              iv_repo_url             = iv_repo_url
            CHANGING
              cv_proxy_authentication = cv_proxy_authentication ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_port.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_proxy_port(
            EXPORTING
              iv_repo_url   = iv_repo_url
            CHANGING
              cv_proxy_port = cv_proxy_port ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_url.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_proxy_url(
            EXPORTING
              iv_repo_url  = iv_repo_url
            CHANGING
              cv_proxy_url = cv_proxy_url ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_supported_data_objects.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_supported_data_objects( CHANGING ct_objects = ct_objects ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_supported_object_types.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_supported_object_types( CHANGING ct_types = ct_types ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_tadir.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_tadir(
            EXPORTING
              iv_package = iv_package
              ii_log     = ii_log
            CHANGING
              ct_tadir   = ct_tadir ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~create_http_client.

    IF gi_exit IS NOT INITIAL.
      TRY.
          ri_client = gi_exit->create_http_client( iv_url ).
		  RETUNR.
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.
	
*DEFAULT Implementation: look to the proper RFC destination (for the user). See #1841
	DATA(lv_destination) = find_rfc_dest_from_uri( iv_url ).

  IF lv_destination IS INITIAL.
    RETURN.
  ENDIF.

  cl_http_client=>create_by_destination(
    EXPORTING
      destination              = lv_destination
    IMPORTING
      client                   = ri_client
    EXCEPTIONS
      argument_not_found       = 1
      destination_not_found    = 2
      destination_no_authority = 3
      plugin_not_active        = 4
      internal_error           = 5
      OTHERS                   = 6 ).

  IF sy-subrc <> 0.
    zcx_abapgit_exception=>raise_t100(  ).
  ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~custom_serialize_abap_clif.

    " This exit might be called twice per object
    " 1st call: it_source = initial
    "    Can be used for serializing complete source
    "    If source is returned, there will be no second call
    " 2nd call: it_source = code as serialized by abapGit
    "    Can be used for post-processing of source
    IF gi_exit IS NOT INITIAL.
      TRY.
          rt_source = gi_exit->custom_serialize_abap_clif(
            is_class_key = is_class_key
            it_source    = it_source ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    IF rt_source IS INITIAL.
      rt_source = it_source.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~deserialize_postprocess.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->deserialize_postprocess( is_step = is_step
                                            ii_log  = ii_log ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~determine_transport_request.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->determine_transport_request(
            EXPORTING
              io_repo              = io_repo
              iv_transport_type    = iv_transport_type
            CHANGING
              cv_transport_request = cv_transport_request ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~get_ci_tests.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->get_ci_tests(
            EXPORTING
              iv_object   = iv_object
            CHANGING
              ct_ci_repos = ct_ci_repos ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~get_ssl_id.

    IF gi_exit IS NOT INITIAL.
      TRY.
          rv_ssl_id = gi_exit->get_ssl_id( ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    IF rv_ssl_id IS INITIAL.
      rv_ssl_id = 'ANONYM'.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~http_client.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->http_client(
            iv_url    = iv_url
            ii_client = ii_client ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~on_event.

    IF gi_exit IS NOT INITIAL.
      TRY.
          rs_handled = gi_exit->on_event( ii_event ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~pre_calculate_repo_status.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->pre_calculate_repo_status(
            EXPORTING
              is_repo_meta = is_repo_meta
            CHANGING
              ct_local     = ct_local
              ct_remote    = ct_remote ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~serialize_postprocess.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->serialize_postprocess(
            EXPORTING
              iv_package = iv_package
              ii_log     = ii_log
            CHANGING
              ct_files   = ct_files ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~validate_before_push.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->validate_before_push(
            is_comment = is_comment
            io_stage   = io_stage
            io_repo    = io_repo ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~wall_message_list.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->wall_message_list( ii_html ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_exit~wall_message_repo.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->wall_message_repo(
            is_repo_meta = is_repo_meta
            ii_html      = ii_html ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD zif_abapgit_exit~enhance_repo_toolbar.
    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->enhance_repo_toolbar(
            io_menu = io_menu
            iv_key  = iv_key
            iv_act  = iv_act ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
