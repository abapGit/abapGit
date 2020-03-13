interface ZIF_ABAPGIT_CUSTOMIZING_COMP
  public .


  types:
    BEGIN OF ty_bcset_metadata,
      scprattr TYPE scprattr,
      scprtext TYPE STANDARD TABLE OF scprtext WITH DEFAULT KEY,
      scprvals TYPE STANDARD TABLE OF scprvals WITH DEFAULT KEY,
      scprvall TYPE STANDARD TABLE OF scprvall WITH DEFAULT KEY,
      scprreca TYPE STANDARD TABLE OF scprreca WITH DEFAULT KEY,
      scprfldv TYPE STANDARD TABLE OF scprfldv WITH DEFAULT KEY,
      subprofs TYPE STANDARD TABLE OF scprpprl WITH DEFAULT KEY,
    END OF ty_bcset_metadata .

  methods COMPARE_CUSTOMIZING_WITH_TABLE
    importing
      !IS_FILE_DETAILS type ZIF_ABAPGIT_DEFINITIONS=>TY_FILE
      !IS_ITEM type ZIF_ABAPGIT_DEFINITIONS=>TY_ITEM
    changing
      !CS_RESULT type ZIF_ABAPGIT_DEFINITIONS=>TY_RESULT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods CREATE_LOCAL_FILE
    changing
      !RS_FILE type ZIF_ABAPGIT_DEFINITIONS=>TY_STAGE_FILES
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods APPLY_CUSTOMIZING_CONTENT
    importing
      !IS_BCSET_METADATA type TY_BCSET_METADATA
      !IO_LOG type ref to ZIF_ABAPGIT_LOG .
  methods DISPLAY_DIFFERENCES
    importing
      !IV_KEY type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-KEY
      !IS_FILE type ZIF_ABAPGIT_DEFINITIONS=>TY_FILE
    returning
      value(RV_IS_CUSTOMIZING_CONTENT) type ABAP_BOOL
    raising
      ZCX_ABAPGIT_EXCEPTION .
endinterface.
