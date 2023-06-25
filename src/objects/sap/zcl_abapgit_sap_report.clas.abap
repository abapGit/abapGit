CLASS zcl_abapgit_sap_report DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_sap_report.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_language_version
      IMPORTING
        iv_package        TYPE devclass
        iv_version        TYPE zif_abapgit_sap_report=>ty_abap_language_version
      RETURNING
        VALUE(rv_version) TYPE zif_abapgit_sap_report=>ty_abap_language_version.

    METHODS authorization_check
      IMPORTING
        iv_mode    TYPE csequence
        is_item    TYPE zif_abapgit_definitions=>ty_item
        iv_version TYPE zif_abapgit_sap_report=>ty_abap_language_version OPTIONAL
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_sap_report IMPLEMENTATION.


  METHOD authorization_check.

    IF is_item IS NOT INITIAL.
      " TODO: Check for ABAP Language Version (ABAP_LANGU_VERSION_UPON_INSERT = iv_version)
      CALL FUNCTION 'RS_ACCESS_PERMISSION'
        EXPORTING
          mode                     = iv_mode
          object                   = is_item-obj_name
          object_class             = is_item-obj_type
          suppress_corr_check      = abap_true
          suppress_language_check  = abap_true
          suppress_extend_dialog   = abap_true
        EXCEPTIONS
          canceled_in_corr         = 1
          enqueued_by_user         = 2
          enqueue_system_failure   = 3
          illegal_parameter_values = 4
          locked_by_author         = 5
          no_modify_permission     = 6
          no_show_permission       = 7
          permission_failure       = 8
          request_language_denied  = 9
          OTHERS                   = 10.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_language_version.

    ASSERT iv_version CA ' X25'.

    " TODO: Determine ABAP Language Version
    " https://github.com/abapGit/abapGit/issues/6154#issuecomment-1503566920)

    " For now, use default for ABAP source code
    IF zcl_abapgit_factory=>get_environment( )->is_sap_cloud_platform( ) = abap_true.
      rv_version = '5'. " abap_for_cloud_development
    ELSE.
      rv_version = 'X'. " standard_abap
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_sap_report~delete_report.

    authorization_check(
      iv_mode = 'DELETE'
      is_item = is_item ).

    DELETE REPORT iv_name.

    IF sy-subrc <> 0 AND iv_raise_error = abap_true.
      zcx_abapgit_exception=>raise( |Error deleting report { iv_name }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_sap_report~insert_report.

    DATA lv_version TYPE zif_abapgit_sap_report=>ty_abap_language_version.
    DATA lv_obj_name TYPE e071-obj_name.

    ASSERT iv_state CA ' AI'.
    ASSERT iv_program_type CA ' 1FIJKMST'.

    lv_version = get_language_version(
      iv_package = iv_package
      iv_version = iv_version ).

    authorization_check(
      iv_mode    = 'MODIFY'
      is_item    = is_item
      iv_version = lv_version ).

    IF iv_state IS INITIAL.
      INSERT REPORT iv_name FROM it_source
        VERSION lv_version.
    ELSEIF iv_program_type IS INITIAL AND iv_extension_type IS INITIAL.
      INSERT REPORT iv_name FROM it_source
        STATE   iv_state
        VERSION lv_version.
    ELSEIF iv_extension_type IS INITIAL.
      INSERT REPORT iv_name FROM it_source
        STATE        iv_state
        PROGRAM TYPE iv_program_type
        VERSION      lv_version.
    ELSE.
      INSERT REPORT iv_name FROM it_source
        STATE          iv_state
        EXTENSION TYPE iv_extension_type
        PROGRAM TYPE   iv_program_type
        VERSION        lv_version.
    ENDIF.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error inserting report { iv_name }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_sap_report~read_report.

    ASSERT iv_state CA ' AI'.

    authorization_check(
      iv_mode = 'SHOW'
      is_item = is_item ).

    IF iv_state IS INITIAL.
      READ REPORT iv_name INTO rt_source.
    ELSE.
      READ REPORT iv_name INTO rt_source STATE iv_state.
    ENDIF.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error reading report { iv_name }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_sap_report~update_report.

    DATA lt_new TYPE string_table.
    DATA lt_old TYPE string_table.

    lt_new = it_source.
    lt_old = zif_abapgit_sap_report~read_report( iv_name ).

    IF lt_old <> lt_new.
      zif_abapgit_sap_report~insert_report(
        iv_name            = iv_name
        it_source          = it_source
        iv_state           = iv_state
        iv_program_type    = iv_program_type
        iv_extension_type  = iv_extension_type
        iv_package         = iv_package
        iv_version         = iv_version
        is_item            = is_item ).

      rv_updated = abap_true.
    ELSE.
      rv_updated = abap_false.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
