INTERFACE zif_abapgit_object PUBLIC.

  METHODS:
    serialize
      IMPORTING io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING   zcx_abapgit_exception,
    deserialize
      IMPORTING iv_package TYPE devclass
                io_xml     TYPE REF TO zcl_abapgit_xml_input
      RAISING   zcx_abapgit_exception,
    delete
      RAISING zcx_abapgit_exception,
    exists
      RETURNING VALUE(rv_bool) TYPE abap_bool
      RAISING   zcx_abapgit_exception,
    is_locked
      RETURNING VALUE(rv_is_locked) TYPE abap_bool
      RAISING   zcx_abapgit_exception,
    is_active
      RETURNING VALUE(rv_active) TYPE abap_bool
      RAISING   zcx_abapgit_exception,
    changed_by
      RETURNING VALUE(rv_user) TYPE xubname
      RAISING   zcx_abapgit_exception,
    jump
      RAISING zcx_abapgit_exception,
    get_metadata
      RETURNING VALUE(rs_metadata) TYPE zif_abapgit_definitions=>ty_metadata,
    has_changed_since
      IMPORTING iv_timestamp      TYPE timestamp
      RETURNING VALUE(rv_changed) TYPE abap_bool
      RAISING   zcx_abapgit_exception.
  METHODS:
    compare_to_remote_version
      IMPORTING io_remote_version_xml       TYPE REF TO zcl_abapgit_xml_input
      RETURNING VALUE(ro_comparison_result) TYPE REF TO zif_abapgit_comparison_result
      RAISING   zcx_abapgit_exception.

  DATA: mo_files TYPE REF TO zcl_abapgit_objects_files.

ENDINTERFACE.
