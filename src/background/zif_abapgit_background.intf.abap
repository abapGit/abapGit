interface ZIF_ABAPGIT_BACKGROUND
  public .


  class-methods GET_DESCRIPTION
    returning
      value(RV_DESCRIPTION) type STRING .
  class-methods GET_SETTINGS
    changing
      value(CT_SETTINGS) type ZCL_ABAPGIT_PERSIST_BACKGROUND=>TY_SETTINGS_TT .
  methods RUN
    importing
      !IO_REPO type ref to ZCL_ABAPGIT_REPO_ONLINE
      !IT_SETTINGS type ZCL_ABAPGIT_PERSIST_BACKGROUND=>TY_SETTINGS_TT optional
    raising
      ZCX_ABAPGIT_EXCEPTION .
endinterface.
