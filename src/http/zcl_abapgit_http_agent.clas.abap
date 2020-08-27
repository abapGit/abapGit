CLASS zcl_abapgit_http_agent DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_http_agent .

    CLASS-METHODS create
      RETURNING
        VALUE(ri_instance) TYPE REF TO zif_abapgit_http_agent .

    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_global_headers TYPE REF TO zcl_abapgit_string_map.

    CLASS-METHODS attach_payload
      IMPORTING
        ii_request TYPE REF TO if_http_request
        iv_payload TYPE any
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_HTTP_AGENT IMPLEMENTATION.


  METHOD attach_payload.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
    lo_type = cl_abap_typedescr=>describe_by_data( iv_payload ).

    IF lo_type->type_kind = cl_abap_typedescr=>typekind_xstring.
      ii_request->set_data( iv_payload ).

    ELSEIF lo_type->type_kind = cl_abap_typedescr=>typekind_string.
      ii_request->set_cdata( iv_payload ).

    ELSE.
      zcx_abapgit_exception=>raise( |Unexpected payload type { lo_type->absolute_name }| ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    CREATE OBJECT mo_global_headers.

  ENDMETHOD.


  METHOD create.

    CREATE OBJECT ri_instance TYPE zcl_abapgit_http_agent.

  ENDMETHOD.


  METHOD zif_abapgit_http_agent~global_headers.

    ro_global_headers = mo_global_headers.

  ENDMETHOD.


  METHOD zif_abapgit_http_agent~request.

    DATA li_client TYPE REF TO if_http_client.
    DATA lo_proxy_configuration TYPE REF TO zcl_abapgit_proxy_config.
    DATA lv_code TYPE i.
    DATA lv_message TYPE string.
    FIELD-SYMBOLS <ls_entry> LIKE LINE OF io_query->mt_entries.

    CREATE OBJECT lo_proxy_configuration.

    cl_http_client=>create_by_url(
      EXPORTING
        url           = iv_url
        ssl_id        = zcl_abapgit_exit=>get_instance( )->get_ssl_id( )
        proxy_host    = lo_proxy_configuration->get_proxy_url( iv_url )
        proxy_service = lo_proxy_configuration->get_proxy_port( iv_url )
      IMPORTING
        client = li_client ).

    li_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
    li_client->request->set_method( iv_method ).

    IF io_query IS BOUND.
      LOOP AT io_query->mt_entries ASSIGNING <ls_entry>.
        li_client->request->set_form_field(
          name  = <ls_entry>-k
          value = <ls_entry>-v ).
      ENDLOOP.
    ENDIF.

    LOOP AT mo_global_headers->mt_entries ASSIGNING <ls_entry>.
      li_client->request->set_header_field(
        name  = to_lower( <ls_entry>-k )
        value = <ls_entry>-v ).
    ENDLOOP.

    IF io_headers IS BOUND.
      LOOP AT io_headers->mt_entries ASSIGNING <ls_entry>.
        li_client->request->set_header_field(
          name  = to_lower( <ls_entry>-k )
          value = <ls_entry>-v ).
      ENDLOOP.
    ENDIF.

    IF iv_method = zif_abapgit_http_agent=>c_methods-post
      OR iv_method = zif_abapgit_http_agent=>c_methods-put
      OR iv_method = zif_abapgit_http_agent=>c_methods-patch.
      attach_payload(
        ii_request = li_client->request
        iv_payload = iv_payload ).
    ENDIF.

    li_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).
    IF sy-subrc = 0.
      li_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).
    ENDIF.

    IF sy-subrc <> 0.
      li_client->get_last_error(
        IMPORTING
          code    = lv_code
          message = lv_message ).
      zcx_abapgit_exception=>raise( |HTTP error: [{ lv_code }] { lv_message }| ).
    ENDIF.

    ri_response = lcl_http_response=>create( li_client ).

  ENDMETHOD.
ENDCLASS.
