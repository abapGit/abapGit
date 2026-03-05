CLASS zcl_abapgit_object_dsfi DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_abapgit_object~changed_by            REDEFINITION.
    METHODS zif_abapgit_object~get_deserialize_steps REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_object_dsfi IMPLEMENTATION.
  METHOD zif_abapgit_object~changed_by.

    DATA: lo_dsfi_handler          TYPE REF TO object,
          lo_dsfi_source_container TYPE REF TO object,
          lv_object_key            TYPE seu_objkey,
          lv_exists                TYPE abap_bool,
          lx_error                 TYPE REF TO cx_root.
    FIELD-SYMBOLS: <lv_active> TYPE any.

    TRY.
        lv_object_key = ms_item-obj_name.
        CALL METHOD ('CL_DSFI_AFF_OBJECT_HANDLER')=>('GET_DDIC_HANDLER')
          EXPORTING
            object_key = lv_object_key
          RECEIVING
            handler    = lo_dsfi_handler.

        ASSIGN ('CE_DD_DSFI_AS4LOCAL=>EN_STATE-ACTIVE')
          TO <lv_active>.
        IF sy-subrc = 0.
          CALL METHOD lo_dsfi_handler->('IF_DD_DSFI_WB_HANDLER~CHECK_EXISTENCE')
            EXPORTING
              iv_as4local = <lv_active>
            RECEIVING
              rv_exists   = lv_exists.

          IF lv_exists = abap_true.
            CALL METHOD lo_dsfi_handler->('IF_DD_DSFI_WB_HANDLER~GET_SOURCE_CONTAINER')
              EXPORTING
                iv_as4local = <lv_active>
              RECEIVING
                ro_result   = lo_dsfi_source_container.

            CALL METHOD lo_dsfi_source_container->('IF_DD_DSFI_SRC_CONTAINER~GET_AS4USER')
              RECEIVING
                rv_result = rv_user.
          ENDIF.
        ENDIF.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-ddic TO rt_steps.
  ENDMETHOD.
ENDCLASS.
