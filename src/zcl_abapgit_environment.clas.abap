CLASS zcl_abapgit_environment DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_factory .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_environment .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_cloud TYPE abap_bool VALUE abap_undefined ##NO_TEXT.
    DATA mv_is_merged TYPE abap_bool VALUE abap_undefined ##NO_TEXT.
    DATA mv_modifiable TYPE abap_bool VALUE abap_undefined ##NO_TEXT.

    METHODS is_system_changes_allowed
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
ENDCLASS.



CLASS zcl_abapgit_environment IMPLEMENTATION.


  METHOD is_system_changes_allowed.

    DATA:
      lv_systemedit         TYPE tadir-edtflag,
      lv_sys_cliinddep_edit TYPE t000-ccnocliind,
      lv_is_shadow          TYPE abap_bool,
      ls_upginfo            TYPE uvers,
      lv_is_upgrade         TYPE abap_bool.

    CALL FUNCTION 'TR_SYS_PARAMS'
      IMPORTING
        systemedit         = lv_systemedit
        sys_cliinddep_edit = lv_sys_cliinddep_edit
      EXCEPTIONS
        no_systemname      = 1
        no_systemtype      = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      " Assume system can't be changed
      RETURN.
    ENDIF.

    CALL FUNCTION 'UPG_IS_SHADOW_SYSTEM'
      IMPORTING
        ev_shadow = lv_is_shadow.

    CALL FUNCTION 'UPG_GET_ACTIVE_COMP_UPGRADE'
      EXPORTING
        iv_component = 'SAP_BASIS'
        iv_upgtype   = 'A'
        iv_buffered  = abap_false
      IMPORTING
        ev_upginfo   = ls_upginfo
      EXCEPTIONS
        OTHERS       = 4.
    IF sy-subrc = 0 AND ls_upginfo-putstatus NA 'ITU'.
      lv_is_upgrade = abap_true.
    ENDIF.

    " SAP system has status 'not modifiable' (TK 102)
    " Changes to repository objects are not permitted in this client (TK 729)
    " Shadow system
    " Running upgrade
    rv_result = boolc(
      lv_systemedit <> 'N' AND
      lv_sys_cliinddep_edit NA '23' AND
      lv_is_shadow <> abap_true AND
      lv_is_upgrade <> abap_true ).

  ENDMETHOD.


  METHOD zif_abapgit_environment~compare_with_inactive.
    rv_result = zif_abapgit_environment~is_sap_cloud_platform( ).
  ENDMETHOD.


  METHOD zif_abapgit_environment~get_basis_release.

    SELECT SINGLE release extrelease FROM cvers INTO (rs_result-release, rs_result-sp)
      WHERE component = 'SAP_BASIS' ##SUBRC_OK.

  ENDMETHOD.


  METHOD zif_abapgit_environment~is_merged.
    DATA lr_marker TYPE REF TO data ##NEEDED.

    IF mv_is_merged = abap_undefined.
      TRY.
          CREATE DATA lr_marker TYPE REF TO ('LIF_ABAPMERGE_MARKER').
          "No exception --> marker found
          mv_is_merged = abap_true.

        CATCH cx_sy_create_data_error.
          mv_is_merged = abap_false.
      ENDTRY.
    ENDIF.
    rv_result = mv_is_merged.
  ENDMETHOD.


  METHOD zif_abapgit_environment~is_repo_object_changes_allowed.
    IF mv_modifiable = abap_undefined.
      mv_modifiable = is_system_changes_allowed( ).
    ENDIF.
    rv_result = mv_modifiable.
  ENDMETHOD.


  METHOD zif_abapgit_environment~is_restart_required.
    " This method will be used in the context of SAP Cloud Platform:
    " Pull/Push operations are executed in background jobs.
    " In case of the respective application server needs to be restarted,
    " it is required to terminate the background job and reschedule again.
    rv_result = abap_false.
    TRY.
        CALL METHOD ('CL_APJ_SCP_TOOLS')=>('IS_RESTART_REQUIRED')
          RECEIVING
            restart_required = rv_result.
      CATCH cx_sy_dyn_call_illegal_method cx_sy_dyn_call_illegal_class.
        rv_result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_environment~is_sap_cloud_platform.
    IF mv_cloud = abap_undefined.
      TRY.
          CALL METHOD ('CL_COS_UTILITIES')=>('IS_SAP_CLOUD_PLATFORM')
            RECEIVING
              rv_is_sap_cloud_platform = mv_cloud.
        CATCH cx_sy_dyn_call_error.
          mv_cloud = abap_false.
      ENDTRY.
    ENDIF.
    rv_result = mv_cloud.
  ENDMETHOD.


  METHOD zif_abapgit_environment~is_sap_object_allowed.

    rv_allowed = cl_enh_badi_def_utility=>is_sap_system( ).
    IF rv_allowed = abap_true.
      RETURN.
    ENDIF.

    rv_allowed = zcl_abapgit_exit=>get_instance( )->allow_sap_objects( ).

  ENDMETHOD.

  METHOD zif_abapgit_environment~get_system_language_filter.
    DATA lv_translation_detective_lang TYPE spras.
    DATA lv_pseudo_translation_language TYPE spras.
    FIELD-SYMBOLS <ls_system_language_filter> LIKE LINE OF rt_system_language_filter.

    " Translation Object Detective
    " https://help.sap.com/docs/ABAP_PLATFORM_NEW/ceb25152cb0d4adba664cebea2bf4670/88a3d3cbccf64601975acabaccdfde45.html
    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
      EXPORTING
        input            = '1Q'
      IMPORTING
        output           = lv_translation_detective_lang
      EXCEPTIONS
        unknown_language = 1
        OTHERS           = 2.
    IF sy-subrc = 1.
      " The language for Translation Object Detective was not setup
    ENDIF.
    IF NOT lv_translation_detective_lang IS INITIAL.
      APPEND INITIAL LINE TO rt_system_language_filter ASSIGNING <ls_system_language_filter>.
      <ls_system_language_filter>-sign = 'E'.
      <ls_system_language_filter>-option = 'EQ'.
      <ls_system_language_filter>-low = lv_translation_detective_lang.
    ENDIF.
    " 1943470 - Using technical language key 2Q to create pseudo-translations of ABAP developments
    " https://launchpad.support.sap.com/#/notes/1943470
    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
      EXPORTING
        input            = '2Q'
      IMPORTING
        output           = lv_pseudo_translation_language
      EXCEPTIONS
        unknown_language = 1
        OTHERS           = 2.
    IF sy-subrc = 1.
      " The language for Pseudo Translation was not setup
    ENDIF.
    IF NOT lv_pseudo_translation_language IS INITIAL.
      APPEND INITIAL LINE TO rt_system_language_filter ASSIGNING <ls_system_language_filter>.
      <ls_system_language_filter>-sign = 'E'.
      <ls_system_language_filter>-option = 'EQ'.
      <ls_system_language_filter>-low = lv_pseudo_translation_language.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_environment~is_variant_maintenance.

    DATA:
      lt_variscreens TYPE STANDARD TABLE OF rsdynnr
                          WITH NON-UNIQUE DEFAULT KEY.

    " Memory is set in LSVARF08 / EXPORT_SCREEN_TABLES.
    IMPORT variscreens = lt_variscreens FROM MEMORY ID '%_SCRNR_%'.

    rv_is_variant_maintenance = boolc( lines( lt_variscreens ) > 0 ).

  ENDMETHOD.

ENDCLASS.
