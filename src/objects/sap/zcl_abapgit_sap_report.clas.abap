CLASS zcl_abapgit_sap_report DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_sap_report.

    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_settings TYPE REF TO zcl_abapgit_settings.

    METHODS authorization_check
      IMPORTING
        iv_mode    TYPE csequence
        is_item    TYPE zif_abapgit_definitions=>ty_item
        iv_version TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version OPTIONAL
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_sap_report IMPLEMENTATION.


  METHOD authorization_check.

    IF is_item IS NOT INITIAL.
      TRY.
          CALL FUNCTION 'RS_ACCESS_PERMISSION'
            EXPORTING
              mode                           = iv_mode
              object                         = is_item-obj_name
              object_class                   = is_item-obj_type
              suppress_corr_check            = abap_true
              suppress_language_check        = abap_true
              suppress_extend_dialog         = abap_true
              abap_langu_version_upon_insert = iv_version " does not exist on lower releases
            EXCEPTIONS
              canceled_in_corr               = 1
              enqueued_by_user               = 2
              enqueue_system_failure         = 3
              illegal_parameter_values       = 4
              locked_by_author               = 5
              no_modify_permission           = 6
              no_show_permission             = 7
              permission_failure             = 8
              request_language_denied        = 9
              OTHERS                         = 10.
        CATCH cx_sy_dyn_call_param_not_found.
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
      ENDTRY.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.
    mo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).
  ENDMETHOD.


  METHOD zif_abapgit_sap_report~clear_abap_language_version.
    IF mo_settings->is_feature_enabled( zcl_abapgit_abap_language_vers=>c_feature_flag ) = abap_true.
      CLEAR cv_version.
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


  METHOD zif_abapgit_sap_report~get_abap_language_version.

    DATA lo_abap_language_vers TYPE REF TO zcl_abapgit_abap_language_vers.

    IF mo_settings->is_feature_enabled( zcl_abapgit_abap_language_vers=>c_feature_flag ) = abap_true.
      " Determine ABAP Language Version for source code
      " https://github.com/abapGit/abapGit/issues/6154#issuecomment-1503566920)
      CREATE OBJECT lo_abap_language_vers.

      rv_version = lo_abap_language_vers->get_abap_language_vers_by_objt(
        iv_object_type = iv_object_type
        iv_package     = iv_package ).
    ELSE.
      rv_version = iv_version.
    ENDIF.

    " Fallback for ABAP source code based on environment
    IF rv_version IS INITIAL.
      IF zcl_abapgit_factory=>get_environment( )->is_sap_cloud_platform( ) = abap_true.
        rv_version = zif_abapgit_aff_types_v1=>co_abap_language_version_src-cloud_development.
      ELSE.
        rv_version = zif_abapgit_aff_types_v1=>co_abap_language_version_src-standard.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_sap_report~insert_report.

    DATA lv_version TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version.
    DATA lv_obj_name TYPE e071-obj_name.

    ASSERT iv_state CA ' AI'.
    ASSERT iv_program_type CA ' 1FIJKMST'.

    lv_version = zif_abapgit_sap_report~get_abap_language_version(
      iv_object_type = 'PROG'
      iv_package     = iv_package ).

    authorization_check(
      iv_mode    = 'MODIFY'
      is_item    = is_item
      iv_version = lv_version ).

    IF iv_state IS INITIAL.
      INSERT REPORT iv_name FROM it_source.
    ELSEIF iv_program_type IS INITIAL AND iv_extension_type IS INITIAL.
      INSERT REPORT iv_name FROM it_source
        STATE   iv_state.
    ELSEIF iv_extension_type IS INITIAL.
      INSERT REPORT iv_name FROM it_source
        STATE        iv_state
        PROGRAM TYPE iv_program_type.
    ELSE.
      INSERT REPORT iv_name FROM it_source
        STATE          iv_state
        EXTENSION TYPE iv_extension_type
        PROGRAM TYPE   iv_program_type.
    ENDIF.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error inserting report { iv_name }| ).
    ENDIF.

    " In lower releases, INSERT REPORT does not support setting ABAP Language version (VERSION)
    " Therefore, update the flag directly
    UPDATE progdir SET uccheck = lv_version WHERE name = iv_name AND state = iv_state.

  ENDMETHOD.


  METHOD zif_abapgit_sap_report~read_progdir.

    DATA ls_sapdir TYPE progdir.

    CALL FUNCTION 'READ_PROGDIR'
      EXPORTING
        i_progname = iv_name
        i_state    = iv_state
      IMPORTING
        e_progdir  = ls_sapdir.

    MOVE-CORRESPONDING ls_sapdir TO rs_progdir.

    CLEAR: rs_progdir-edtx,
           rs_progdir-cnam,
           rs_progdir-cdat,
           rs_progdir-unam,
           rs_progdir-udat,
           rs_progdir-levl,
           rs_progdir-vern,
           rs_progdir-rmand,
           rs_progdir-sdate,
           rs_progdir-stime,
           rs_progdir-idate,
           rs_progdir-itime,
           rs_progdir-varcl,
           rs_progdir-state.

    zif_abapgit_sap_report~clear_abap_language_version( CHANGING cv_version = rs_progdir-uccheck ).

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


  METHOD zif_abapgit_sap_report~update_progdir.

    DATA ls_progdir_new TYPE progdir.

    CALL FUNCTION 'READ_PROGDIR'
      EXPORTING
        i_progname = is_progdir-name
        i_state    = iv_state
      IMPORTING
        e_progdir  = ls_progdir_new
      EXCEPTIONS
        not_exists = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error reading program directory' ).
    ENDIF.

    ls_progdir_new-ldbname = is_progdir-ldbname.
    ls_progdir_new-dbna    = is_progdir-dbna.
    ls_progdir_new-dbapl   = is_progdir-dbapl.
    ls_progdir_new-rload   = is_progdir-rload.
    ls_progdir_new-fixpt   = is_progdir-fixpt.
    ls_progdir_new-appl    = is_progdir-appl.
    ls_progdir_new-rstat   = is_progdir-rstat.
    ls_progdir_new-sqlx    = is_progdir-sqlx.
    ls_progdir_new-clas    = is_progdir-clas.
    ls_progdir_new-secu    = is_progdir-secu.

    ls_progdir_new-uccheck = zif_abapgit_sap_report~get_abap_language_version(
      iv_object_type = 'PROG'
      iv_package     = iv_package
      iv_version     = is_progdir-uccheck ).

    CALL FUNCTION 'UPDATE_PROGDIR'
      EXPORTING
        i_progdir    = ls_progdir_new
        i_progname   = ls_progdir_new-name
        i_state      = ls_progdir_new-state
      EXCEPTIONS
        not_executed = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error updating program directory' ).
    ENDIF.

    " Function UPDATE_PROGDIR does not update VARCL, so we do it here
    SELECT SINGLE * FROM progdir INTO ls_progdir_new
      WHERE name  = ls_progdir_new-name
        AND state = ls_progdir_new-state.
    IF sy-subrc = 0 AND is_progdir-varcl <> ls_progdir_new-varcl.
      UPDATE progdir SET varcl = is_progdir-varcl
        WHERE name  = ls_progdir_new-name
          AND state = ls_progdir_new-state.               "#EC CI_SUBRC
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_sap_report~update_report.

    DATA lt_new TYPE string_table.
    DATA lt_old TYPE string_table.

    lt_new = it_source.
    lt_old = zif_abapgit_sap_report~read_report( iv_name ).

    IF lt_old <> lt_new.
      zif_abapgit_sap_report~insert_report(
        iv_name           = iv_name
        it_source         = it_source
        iv_state          = iv_state
        iv_program_type   = iv_program_type
        iv_extension_type = iv_extension_type
        iv_package        = iv_package
        is_item           = is_item ).

      rv_updated = abap_true.
    ELSE.
      rv_updated = abap_false.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
