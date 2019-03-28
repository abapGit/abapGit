class ZCL_ABAPGIT_ENVIRONMENT definition
  public
  create public .

public section.

  class-methods IS_SAP_CLOUD_PLATFORM
    returning
      value(RV_CLOUD) type ABAP_BOOL .
  PROTECTED SECTION.

    CLASS-DATA gv_cloud TYPE abap_bool VALUE abap_undefined ##NO_TEXT.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_ENVIRONMENT IMPLEMENTATION.


  METHOD IS_SAP_CLOUD_PLATFORM.

    IF gv_cloud = abap_undefined.
      TRY.
          CALL METHOD ('CL_COS_UTILITIES')=>('IS_SAP_CLOUD_PLATFORM')
            RECEIVING
              rv_is_sap_cloud_platform = gv_cloud.
        CATCH cx_sy_dyn_call_illegal_method.
          gv_cloud = abap_false.
      ENDTRY.
    ENDIF.

    rv_cloud = gv_cloud.

  ENDMETHOD.
ENDCLASS.
