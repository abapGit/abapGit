CLASS zcl_abapgit_object_gsmp DEFINITION
  PUBLIC
    INHERITING FROM zcl_abapgit_object_common_aff
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_abapgit_object~changed_by REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA: sv_emsgty TYPE sychar01 VALUE 'S'.          "#EC NOTEXT
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_GSMP IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    DATA lv_name TYPE c LENGTH 180.

    DATA lx_root TYPE REF TO cx_root.


    TRY.
        lv_name = ms_item-obj_name.

        SELECT SINGLE changed_by FROM ('GSM_MD_PRV_W')
          WHERE provider_id = @lv_name AND
          version = 'I'
          INTO @rv_user.                                    

        IF sy-subrc = 0.
          RETURN.
        ENDIF.

        SELECT SINGLE changed_by FROM ('GSM_MD_PRV_W')
          WHERE provider_id = @lv_name AND
          version = 'A'
          INTO @rv_user.                                    "#EC NOTEXT

      CATCH cx_root INTO lx_root.
        zcx_abapgit_exception=>raise( iv_text     = lx_root->get_text( )
                                     ix_previous = lx_root ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
