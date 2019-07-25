class ZCL_ABAPGIT_INJECTOR definition
  public
  create private
  for testing .

public section.

  class-methods SET_TADIR
    importing
      !II_TADIR type ref to ZIF_ABAPGIT_TADIR .
  class-methods SET_SAP_PACKAGE
    importing
      !IV_PACKAGE type DEVCLASS
      !II_SAP_PACKAGE type ref to ZIF_ABAPGIT_SAP_PACKAGE .
  class-methods SET_CODE_INSPECTOR
    importing
      !IV_PACKAGE type DEVCLASS
      !II_CODE_INSPECTOR type ref to ZIF_ABAPGIT_CODE_INSPECTOR .
  class-methods SET_STAGE_LOGIC
    importing
      !II_LOGIC type ref to ZIF_ABAPGIT_STAGE_LOGIC .
  class-methods SET_CTS_API
    importing
      !II_CTS_API type ref to ZIF_ABAPGIT_CTS_API .
  class-methods SET_ENVIRONMENT
    importing
      !IO_ENVIRONMENT type ref to ZIF_ABAPGIT_ENVIRONMENT .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_ABAPGIT_INJECTOR IMPLEMENTATION.


  METHOD set_code_inspector.

    DATA: ls_code_inspector LIKE LINE OF zcl_abapgit_factory=>gt_code_inspector.
    FIELD-SYMBOLS: <ls_code_inspector> LIKE LINE OF zcl_abapgit_factory=>gt_code_inspector.

    READ TABLE zcl_abapgit_factory=>gt_code_inspector
         ASSIGNING <ls_code_inspector>
         WITH TABLE KEY package = iv_package.
    IF sy-subrc <> 0.

      ls_code_inspector-package = iv_package.

      INSERT ls_code_inspector
             INTO TABLE zcl_abapgit_factory=>gt_code_inspector
             ASSIGNING <ls_code_inspector>.

    ENDIF.

    <ls_code_inspector>-instance = ii_code_inspector.

  ENDMETHOD.


  METHOD set_cts_api.
    zcl_abapgit_factory=>gi_cts_api = ii_cts_api.
  ENDMETHOD.


  METHOD set_environment.
    zcl_abapgit_factory=>go_environment = io_environment.
  ENDMETHOD.


  METHOD set_sap_package.

    DATA: ls_sap_package TYPE zcl_abapgit_factory=>ty_sap_package.
    FIELD-SYMBOLS: <ls_sap_package> TYPE zcl_abapgit_factory=>ty_sap_package.

    READ TABLE zcl_abapgit_factory=>gt_sap_package
         ASSIGNING <ls_sap_package>
         WITH TABLE KEY package = iv_package.

    IF sy-subrc <> 0.

      ls_sap_package-package = iv_package.
      INSERT ls_sap_package
             INTO TABLE zcl_abapgit_factory=>gt_sap_package
             ASSIGNING <ls_sap_package>.

    ENDIF.

    <ls_sap_package>-instance = ii_sap_package.

  ENDMETHOD.


  METHOD set_stage_logic.
    zcl_abapgit_factory=>gi_stage_logic = ii_logic.
  ENDMETHOD.


  METHOD set_tadir.
    zcl_abapgit_factory=>gi_tadir = ii_tadir.
  ENDMETHOD.
ENDCLASS.
