CLASS zcl_abapgit_object_dteb DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_abapgit_object~changed_by            REDEFINITION .
    METHODS zif_abapgit_object~get_deserialize_steps REDEFINITION.
  PROTECTED SECTION.

    METHODS get_additional_extensions
        REDEFINITION .
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abapgit_object_dteb IMPLEMENTATION.


  METHOD get_additional_extensions.

    DATA ls_additional_extension LIKE LINE OF rv_additional_extensions.
    ls_additional_extension-extension = 'acds'.
    CALL METHOD ('CL_CDS_AFF_FILE_NAME_MAPPER')=>for_cds
      RECEIVING
        result = ls_additional_extension-file_name_mapper.
    APPEND ls_additional_extension TO rv_additional_extensions.
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: lo_dteb_handler TYPE REF TO object,
          lv_object_key   TYPE seu_objkey,
          lx_error        TYPE REF TO cx_root.

    TRY.
        lv_object_key = ms_item-obj_name.
        CALL METHOD ('CL_DDIC_ADT_DT_UTILITY')=>('CREATE_DD_DTEB_HANDLER')
          EXPORTING
            object_key = lv_object_key
          RECEIVING
            handler = lo_dteb_handler.

        CALL METHOD lo_dteb_handler->('GET_CHANGED_BY')
          RECEIVING
            rv_changed_by = rv_user.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-ddic TO rt_steps.
  ENDMETHOD.
ENDCLASS.
