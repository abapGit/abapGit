CLASS zcl_abapgit_exit_super DEFINITION
  PUBLIC
  CREATE PROTECTED.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_exit.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_exit_super IMPLEMENTATION.


  METHOD zif_abapgit_exit~adjust_display_commit_url.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~adjust_display_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~allow_sap_objects.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~change_local_host.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_authentication.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_port.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_url.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~change_supported_data_objects.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~change_supported_object_types.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~change_tadir.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~create_http_client.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~custom_serialize_abap_clif.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~deserialize_postprocess.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~determine_transport_request.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~enhance_repo_toolbar.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~get_ci_tests.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~get_ssl_id.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~http_client.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~on_event.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~pre_calculate_repo_status.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~serialize_postprocess.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~validate_before_push.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~wall_message_list.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_exit~wall_message_repo.
    RETURN.
  ENDMETHOD.
ENDCLASS.
