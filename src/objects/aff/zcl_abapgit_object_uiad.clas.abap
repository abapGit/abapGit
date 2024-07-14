CLASS zcl_abapgit_object_uiad DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS zif_abapgit_object~changed_by
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_UIAD IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    DATA: lo_db_api     TYPE REF TO object,
          lr_data       TYPE REF TO data,
          lv_object_key TYPE c LENGTH 32,
          lx_root       TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_metadata>   TYPE any,
                   <lv_changed_by> TYPE any.

    TRY.
        CALL METHOD ('CL_SUI_UIAD_DB_ACCESS')=>('GET_INSTANCE')
          RECEIVING
            ro_instance = lo_db_api.
        CREATE DATA lr_data TYPE ('CL_BLUE_AFF_WB_ACCESS=>TY_METADATA').
        ASSIGN lr_data->* TO <ls_metadata>.
      CATCH cx_sy_create_object_error
              cx_sy_create_data_error.
        zcx_abapgit_exception=>raise( 'Object UIAD not supported' ).
    ENDTRY.

    TRY.
        lv_object_key = ms_item-obj_name.
        CALL METHOD lo_db_api->('IF_SUI_UIAD_DB_ACCESS~READ_WB_METADATA')
          EXPORTING
            iv_id       = lv_object_key
            iv_version  = 'A'
            iv_language = mv_language
          RECEIVING
            rs_metadata      = <ls_metadata>.

        ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <ls_metadata> TO <lv_changed_by>.
        rv_user = <lv_changed_by>.

      CATCH cx_root INTO lx_root.
        zcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
