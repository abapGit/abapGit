CLASS zcl_abapgit_environment DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS is_sap_cloud_platform
      RETURNING
        VALUE(rv_cloud) TYPE abap_bool.

    CLASS-METHODS is_merged
      RETURNING
        VALUE(rv_is_merged) TYPE abap_bool.

    CLASS-METHODS is_repo_object_changes_allowed
      RETURNING VALUE(rv_allowed) TYPE abap_bool.
  PROTECTED SECTION.

    CLASS-DATA gv_cloud TYPE abap_bool VALUE abap_undefined ##NO_TEXT.
    CLASS-DATA gv_is_merged TYPE abap_bool VALUE abap_undefined ##NO_TEXT.
    CLASS-DATA gv_client_modifiable TYPE abap_bool VALUE abap_undefined.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_ENVIRONMENT IMPLEMENTATION.


  METHOD is_merged.

    DATA lo_marker TYPE REF TO data ##NEEDED.

    IF gv_is_merged = abap_undefined.
      TRY.
          CREATE DATA lo_marker TYPE REF TO ('LIF_ABAPMERGE_MARKER')  ##no_text.
          "No exception --> marker found
          gv_is_merged = abap_true.

        CATCH cx_sy_create_data_error.
          gv_is_merged = abap_false.
      ENDTRY.
    ENDIF.

    rv_is_merged = gv_is_merged.

  ENDMETHOD.


  METHOD is_repo_object_changes_allowed.
    DATA lv_ind TYPE t000-ccnocliind.

    IF gv_client_modifiable = abap_undefined.
      SELECT SINGLE ccnocliind FROM t000 INTO lv_ind
             WHERE mandt = sy-mandt.
      IF sy-subrc = 0
          AND ( lv_ind = ' ' OR lv_ind = '1' ). "check changes allowed
        gv_client_modifiable = abap_true.
      ELSE.
        gv_client_modifiable = abap_false.
      ENDIF.
    ENDIF.
    rv_allowed = gv_client_modifiable.
  ENDMETHOD.


  METHOD is_sap_cloud_platform.

    IF gv_cloud = abap_undefined.
      TRY.
          CALL METHOD ('CL_COS_UTILITIES')=>('IS_SAP_CLOUD_PLATFORM')
            RECEIVING
              rv_is_sap_cloud_platform = gv_cloud.
        CATCH cx_sy_dyn_call_illegal_method.
          gv_cloud = abap_false.
      ENDTRY.
    ENDIF.

    rv_cloud = gv_cloud.

  ENDMETHOD.
ENDCLASS.
