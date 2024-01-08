CLASS zcl_abapgit_frontend_no_gui DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_ui_factory.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_frontend_services.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_frontend_no_gui IMPLEMENTATION.


  METHOD zif_abapgit_frontend_services~clipboard_export.
  ENDMETHOD.


  METHOD zif_abapgit_frontend_services~directory_browse.
  ENDMETHOD.


  METHOD zif_abapgit_frontend_services~directory_create.
  ENDMETHOD.


  METHOD zif_abapgit_frontend_services~directory_exist.
  ENDMETHOD.


  METHOD zif_abapgit_frontend_services~execute.
  ENDMETHOD.


  METHOD zif_abapgit_frontend_services~file_download.
  ENDMETHOD.


  METHOD zif_abapgit_frontend_services~file_upload.
  ENDMETHOD.


  METHOD zif_abapgit_frontend_services~get_file_separator.
  ENDMETHOD.


  METHOD zif_abapgit_frontend_services~get_gui_version.
  ENDMETHOD.


  METHOD zif_abapgit_frontend_services~get_system_directory.
  ENDMETHOD.


  METHOD zif_abapgit_frontend_services~gui_is_available.
    rv_gui_is_available = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_frontend_services~is_sapgui_for_java.
    rv_result = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_frontend_services~is_sapgui_for_windows.
    rv_result = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_frontend_services~is_webgui.
    rv_is_webgui = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_frontend_services~open_ie_devtools.
  ENDMETHOD.


  METHOD zif_abapgit_frontend_services~show_file_open_dialog.
  ENDMETHOD.


  METHOD zif_abapgit_frontend_services~show_file_save_dialog.
  ENDMETHOD.
ENDCLASS.
