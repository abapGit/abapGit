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

    DATA: handler      TYPE REF TO object,
          db_api       TYPE REF TO object,
          lr_data      TYPE REF TO data,
          technical_id TYPE c LENGTH 30.

    FIELD-SYMBOLS: <smbc_config>     TYPE any,
                   <smbc_changed_by> TYPE any.
    TRY.
        CREATE OBJECT handler TYPE ('CL_SMBC_AFF_OBJECT_HANDLER').
        CREATE OBJECT db_api TYPE ('CL_MBC_BUSINESS_CONFIG_DB').
        CREATE DATA lr_data TYPE ('SMBC_CONFIG').
        ASSIGN lr_data->* TO <smbc_config>.
      CATCH cx_sy_create_object_error
            cx_sy_create_data_error.
        zcx_abapgit_exception=>raise( 'SMBC not supported' ).
    ENDTRY.
    technical_id = ms_item-obj_name.
    CALL METHOD db_api->('IF_MBC_BUSINESS_CONFIG_DB~READ')
      EXPORTING
        iv_technical_id = technical_id
        version         = 'I'
      RECEIVING
        rs_config       = <smbc_config>.
    IF <smbc_config> IS INITIAL.
      CALL METHOD db_api->('IF_MBC_BUSINESS_CONFIG_DB~READ')
        EXPORTING
          iv_technical_id = technical_id
          version         = 'A'
        RECEIVING
          rs_config       = <smbc_config>.
    ENDIF.
    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <smbc_config> TO <smbc_changed_by>.
    rv_user = <smbc_changed_by>.

  ENDMETHOD.
ENDCLASS.
