interface ZIF_ABAPGIT_HANDLE_CUSTOMIZING
  public .


  methods STAGE_CUSTOMIZING_CONTENT
    returning
      value(RO_STAGED_CONTENT) type ref to ZCL_ABAPGIT_STAGE
    raising
      ZCX_ABAPGIT_EXCEPTION .
endinterface.
