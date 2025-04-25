CLASS zcl_abapgit_object_smbc DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_abapgit_object~changed_by REDEFINITION.
    METHODS constructor
      IMPORTING
        is_item        TYPE zif_abapgit_definitions=>ty_item
        iv_language    TYPE spras
        io_files       TYPE REF TO zcl_abapgit_objects_files OPTIONAL
        io_i18n_params TYPE REF TO zcl_abapgit_i18n_params OPTIONAL
      RAISING
        zcx_abapgit_type_not_supported.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_object_smbc IMPLEMENTATION.

  METHOD constructor.

    DATA: lo_handler TYPE REF TO object,
          lo_db_api  TYPE REF TO object,
          lr_data    TYPE REF TO data.

    super->constructor(
        is_item        = is_item
        iv_language    = iv_language
        io_files       = io_files
        io_i18n_params = io_i18n_params ).

    TRY.
        CREATE OBJECT lo_handler TYPE ('CL_SMBC_AFF_OBJECT_HANDLER').
        CREATE OBJECT lo_db_api TYPE ('CL_MBC_BUSINESS_CONFIG_DB').
        CREATE DATA lr_data TYPE ('SMBC_CONFIG').
      CATCH cx_sy_create_object_error
            cx_sy_create_data_error.
        RAISE EXCEPTION TYPE zcx_abapgit_type_not_supported EXPORTING obj_type = is_item-obj_type.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: lo_handler      TYPE REF TO object,
          lo_db_api       TYPE REF TO object,
          lr_data         TYPE REF TO data,
          lv_technical_id TYPE c LENGTH 30.

    FIELD-SYMBOLS: <ls_smbc_config>     TYPE any,
                   <lv_smbc_changed_by> TYPE any.

    CREATE OBJECT lo_handler TYPE ('CL_SMBC_AFF_OBJECT_HANDLER').
    CREATE OBJECT lo_db_api TYPE ('CL_MBC_BUSINESS_CONFIG_DB').
    CREATE DATA lr_data TYPE ('SMBC_CONFIG').
    ASSIGN lr_data->* TO <ls_smbc_config>.

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
