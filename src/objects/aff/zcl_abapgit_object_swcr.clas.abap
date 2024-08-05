CLASS zcl_abapgit_object_swcr DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: zif_abapgit_object~changed_by REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_object_swcr IMPLEMENTATION.
  METHOD zif_abapgit_object~changed_by.

    DATA: lr_data        TYPE REF TO data,
          lo_swcr_db_api TYPE REF TO object,
          lv_name        TYPE c LENGTH 30,
          lx_error       TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_swcr_header> TYPE any,
                   <ls_swcr_user>   TYPE any.

    TRY.
        CALL METHOD ('CL_SCR_DB_ACCESS')=>('CREATE_INSTANCE')
          RECEIVING
            result = lo_swcr_db_api.
        CREATE DATA lr_data TYPE ('IF_SCR_TYPES=>TY_S_HEADER').
        ASSIGN lr_data->* TO <ls_swcr_header>.

        lv_name = ms_item-obj_name.

        TRY.
            CALL METHOD lo_swcr_db_api->('IF_SCR_DB_ACCESS~READ_HEADER')
              EXPORTING
                software_component = lv_name
                version            = 'A'
              RECEIVING
                result             = <ls_swcr_header>.
          CATCH cx_root. " no active version found, try to find an inactive version
            CALL METHOD lo_swcr_db_api->('IF_SCR_DB_ACCESS~READ_HEADER')
              EXPORTING
                software_component = lv_name
                version            = 'I'
              RECEIVING
                result             = <ls_swcr_header>.
        ENDTRY.

        ASSIGN COMPONENT 'LAST_CHANGED_BY' OF STRUCTURE <ls_swcr_header> TO <ls_swcr_user>.
        rv_user = <ls_swcr_user>.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                     ix_previous = lx_error ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
