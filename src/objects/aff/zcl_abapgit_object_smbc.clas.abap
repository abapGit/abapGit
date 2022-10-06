CLASS zcl_abapgit_object_smbc DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_abapgit_object~changed_by REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_SMBC IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    DATA: lo_handler      TYPE REF TO object,
          lo_db_api       TYPE REF TO object,
          lr_data         TYPE REF TO data,
          lv_technical_id TYPE c LENGTH 30.

    FIELD-SYMBOLS: <ls_smbc_config>     TYPE any,
                   <lv_smbc_changed_by> TYPE any.
    TRY.
        CREATE OBJECT lo_handler TYPE ('CL_SMBC_AFF_OBJECT_HANDLER').
        CREATE OBJECT lo_db_api TYPE ('CL_MBC_BUSINESS_CONFIG_DB').
        CREATE DATA lr_data TYPE ('SMBC_CONFIG').
        ASSIGN lr_data->* TO <ls_smbc_config>.
      CATCH cx_sy_create_object_error
            cx_sy_create_data_error.
        zcx_abapgit_exception=>raise( 'SMBC not supported' ).
    ENDTRY.
    lv_technical_id = ms_item-obj_name.
    CALL METHOD lo_db_api->('IF_MBC_BUSINESS_CONFIG_DB~READ')
      EXPORTING
        iv_technical_id = lv_technical_id
        version         = 'I'
      RECEIVING
        rs_config       = <ls_smbc_config>.
    IF <ls_smbc_config> IS INITIAL.
      CALL METHOD lo_db_api->('IF_MBC_BUSINESS_CONFIG_DB~READ')
        EXPORTING
          iv_technical_id = lv_technical_id
          version         = 'A'
        RECEIVING
          rs_config       = <ls_smbc_config>.
    ENDIF.
    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <ls_smbc_config> TO <lv_smbc_changed_by>.
    rv_user = <lv_smbc_changed_by>.

  ENDMETHOD.
ENDCLASS.
