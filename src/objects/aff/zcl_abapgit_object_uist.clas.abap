CLASS zcl_abapgit_object_uist DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS zif_abapgit_object~changed_by
        REDEFINITION .
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



CLASS zcl_abapgit_object_uist IMPLEMENTATION.

  METHOD constructor.

    DATA: lo_db_api TYPE REF TO object,
          lr_data   TYPE REF TO data.

    super->constructor(
        is_item        = is_item
        iv_language    = iv_language
        io_files       = io_files
        io_i18n_params = io_i18n_params ).

    TRY.
        CREATE OBJECT lo_db_api TYPE ('/UI2/CL_UIST_SVAL_SQL').
        CREATE DATA lr_data TYPE ('CL_BLUE_AFF_WB_ACCESS=>TY_METADATA').
      CATCH cx_sy_create_object_error
            cx_sy_create_data_error.
        RAISE EXCEPTION TYPE zcx_abapgit_type_not_supported EXPORTING obj_type = is_item-obj_type.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: lo_db_api     TYPE REF TO object,
          lr_data       TYPE REF TO data,
          lv_object_key TYPE seu_objkey,
          lx_root       TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_metadata>   TYPE any,
                   <lv_changed_by> TYPE any.


    TRY.
        CREATE OBJECT lo_db_api TYPE ('/UI2/CL_UIST_SVAL_SQL').
        CREATE DATA lr_data TYPE ('CL_BLUE_AFF_WB_ACCESS=>TY_METADATA').
        ASSIGN lr_data->* TO <ls_metadata>.
      CATCH cx_sy_create_object_error
              cx_sy_create_data_error.
        zcx_abapgit_exception=>raise( 'Object UIST not supported' ).
    ENDTRY.

    TRY.
        lv_object_key = ms_item-obj_name.
        CALL METHOD lo_db_api->('/UI2/IF_UIST_SVAL~GET_METADATA')
          EXPORTING
            object_name = lv_object_key
            version     = 'A'
            language    = mv_language
          RECEIVING
            result      = <ls_metadata>.

        ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <ls_metadata> TO <lv_changed_by>.
        rv_user = <lv_changed_by>.

      CATCH cx_root INTO lx_root.
        zcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
