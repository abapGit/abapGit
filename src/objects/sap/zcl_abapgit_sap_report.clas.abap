CLASS zcl_abapgit_sap_report DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_sap_report.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS authorization_check
      IMPORTING
        iv_mode TYPE csequence
        is_item TYPE zif_abapgit_definitions=>ty_item
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
              abap_langu_version_upon_insert = is_item-abap_language_version " does not exist on lower releases
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
              OTHERS                         = 10 ##FM_SUBRC_OK.
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
              OTHERS                   = 10 ##FM_SUBRC_OK.
      ENDTRY.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
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

    ASSERT iv_state CA ' AI'.
    ASSERT iv_program_type CA ' 1FIJKMST'.

    authorization_check(
      iv_mode = 'MODIFY'
      is_item = is_item ).

    IF iv_state IS INITIAL.
      INSERT REPORT iv_name FROM it_source.
    ELSEIF iv_program_type IS INITIAL AND iv_extension_type IS INITIAL.
      INSERT REPORT iv_name FROM it_source
        STATE iv_state.
    ELSEIF iv_extension_type IS INITIAL.
      INSERT REPORT iv_name FROM it_source
        STATE iv_state
        PROGRAM TYPE iv_program_type.
    ELSE.
      INSERT REPORT iv_name FROM it_source
        STATE iv_state
        EXTENSION TYPE iv_extension_type
        PROGRAM TYPE iv_program_type.
    ENDIF.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error inserting report { iv_name }| ).
    ENDIF.

    " In lower releases, INSERT REPORT does not support setting ABAP Language version (VERSION)
    " Therefore, update the flag directly
    UPDATE progdir SET uccheck = iv_version WHERE name = iv_name AND state = iv_state.

  ENDMETHOD.


  METHOD zif_abapgit_sap_report~read_progdir.

    DATA ls_sapdir TYPE progdir.

    CALL FUNCTION 'READ_PROGDIR'
      EXPORTING
        i_progname = iv_name
        i_state    = iv_state
      IMPORTING
        e_progdir  = ls_sapdir
      EXCEPTIONS
        not_exists = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

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
    ls_progdir_new-uccheck = is_progdir-uccheck.
    ls_progdir_new-sqlx    = is_progdir-sqlx.
    ls_progdir_new-clas    = is_progdir-clas.
    ls_progdir_new-secu    = is_progdir-secu.

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
        iv_version        = iv_version
        is_item           = is_item ).

      rv_updated = abap_true.
    ELSE.
      rv_updated = abap_false.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
