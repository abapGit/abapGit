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



CLASS zcl_abapgit_services_basis IMPLEMENTATION.


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
          lv_exe_full_path    TYPE string,
          lo_frontend_serv    TYPE REF TO zif_abapgit_frontend_services.

    lo_frontend_serv = zcl_abapgit_ui_factory=>get_frontend_services( ).

    IF lo_frontend_serv->is_sapgui_for_windows( ) = abap_false.
      zcx_abapgit_exception=>raise( |IE DevTools not supported on frontend OS| ).
    ENDIF.

    lo_frontend_serv->get_system_directory( CHANGING cv_system_directory = lv_system_directory ).

    cl_gui_cfw=>flush( ).

    lv_exe_full_path = lv_system_directory && `\F12\IEChooser.exe`.
    lo_frontend_serv->execute( iv_application = lv_exe_full_path ).
  ENDMETHOD.


  METHOD raise_error_if_package_exists.

    IF iv_devclass IS INITIAL.
      RETURN.
    ENDIF.

    IF zcl_abapgit_factory=>get_sap_package( iv_devclass )->exists( ) = abap_true.
      zcx_abapgit_exception=>raise( |Package { iv_devclass } already exists| ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
