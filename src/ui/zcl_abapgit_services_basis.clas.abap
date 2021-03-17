CLASS zcl_abapgit_services_basis DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS create_package
      IMPORTING
        !iv_prefill_package TYPE devclass OPTIONAL
      RETURNING
        VALUE(rv_package)   TYPE devclass
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS open_ie_devtools
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS raise_error_if_package_exists
      IMPORTING
        iv_devclass TYPE scompkdtln-devclass
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS ZCL_ABAPGIT_SERVICES_BASIS IMPLEMENTATION.


  METHOD create_package.

    DATA ls_package_data TYPE scompkdtln.
    DATA lv_create       TYPE abap_bool.
    DATA li_popup        TYPE REF TO zif_abapgit_popups.

    ls_package_data-devclass = to_upper( iv_prefill_package ).

    raise_error_if_package_exists( ls_package_data-devclass ).

    li_popup = zcl_abapgit_ui_factory=>get_popups( ).

    li_popup->popup_to_create_package(
      IMPORTING
        es_package_data = ls_package_data
        ev_create       = lv_create ).

    IF lv_create = abap_true.
      zcl_abapgit_factory=>get_sap_package( ls_package_data-devclass )->create( ls_package_data ).
      rv_package = ls_package_data-devclass.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD open_ie_devtools.
    DATA: lv_system_directory TYPE string,
          lv_exe_full_path    TYPE string.

    IF zcl_abapgit_ui_factory=>get_gui_functions( )->is_sapgui_for_windows( ) = abap_false.
      zcx_abapgit_exception=>raise( |IE DevTools not supported on frontend OS| ).
    ENDIF.

    cl_gui_frontend_services=>get_system_directory(
      CHANGING
        system_directory     = lv_system_directory
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from GET_SYSTEM_DIRECTORY sy-subrc: { sy-subrc }| ).
    ENDIF.

    cl_gui_cfw=>flush( ).

    lv_exe_full_path = lv_system_directory && `\F12\IEChooser.exe`.

    cl_gui_frontend_services=>execute(
      EXPORTING
        application            = lv_exe_full_path
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10 ).
    IF sy-subrc <> 0.
      " IEChooser is only available on Windows 10
      zcx_abapgit_exception=>raise( |Error from EXECUTE sy-subrc: { sy-subrc }| ).
    ENDIF.
  ENDMETHOD.


  METHOD raise_error_if_package_exists.

    IF iv_devclass IS INITIAL.
      RETURN.
    ENDIF.

    IF zcl_abapgit_factory=>get_sap_package( iv_devclass )->exists( ) = abap_true.
      " Package &1 already exists
      MESSAGE e042(pak) INTO sy-msgli WITH iv_devclass.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
