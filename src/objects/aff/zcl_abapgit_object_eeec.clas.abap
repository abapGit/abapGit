CLASS zcl_abapgit_object_eeec DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      zif_abapgit_object~changed_by REDEFINITION .

  PROTECTED SECTION.
    METHODS: get_object_handler REDEFINITION.
ENDCLASS.



CLASS zcl_abapgit_object_eeec IMPLEMENTATION.

  METHOD zif_abapgit_object~changed_by.

    DATA: lr_data             TYPE REF TO data,
          lo_registry_adapter TYPE REF TO object,
          lv_object_key       TYPE seu_objkey,
          lx_error            TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_consumer>   TYPE any,
                   <lv_changed_by> TYPE any.

    TRY.
        CREATE OBJECT lo_registry_adapter TYPE ('/IWXBE/CL_EEEC_REG_ADAPTER').
        CREATE DATA lr_data TYPE ('/IWXBE/IF_REGISTRY_TYPES=>TY_S_CONSUMER').
        ASSIGN lr_data->* TO <ls_consumer>.

        lv_object_key = ms_item-obj_name.

        TRY.
            CALL METHOD lo_registry_adapter->('/IWXBE/IF_EEEC_REG_ADAPTER_WB~GET_METADATA')
              EXPORTING
                iv_object_key = lv_object_key
                iv_state      = 'I'
              RECEIVING
                rs_consumer   = <ls_consumer>.

          CATCH cx_root.
            CALL METHOD lo_registry_adapter->('/IWXBE/IF_EEEC_REG_ADAPTER_WB~GET_METADATA')
              EXPORTING
                iv_object_key = lv_object_key
                iv_state      = 'A'
              RECEIVING
                rs_consumer   = <ls_consumer>.
        ENDTRY.

        ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <ls_consumer> TO <lv_changed_by>.
        rv_user = <lv_changed_by>.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error ).
    ENDTRY.

  ENDMETHOD.

  METHOD get_object_handler.

    DATA lx_error TYPE REF TO cx_root.

    CALL METHOD super->get_object_handler
      RECEIVING
        result = result.

    IF result IS NOT BOUND.
      TRY.
          CREATE OBJECT result TYPE ('/IWXBE/CL_EEEC_AFF_OBJECTHANDL').
        CATCH cx_root INTO lx_error.
          zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                        ix_previous = lx_error ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
