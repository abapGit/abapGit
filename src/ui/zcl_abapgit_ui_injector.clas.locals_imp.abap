CLASS lcl_gui_services_dummy DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_gui_services.
    CLASS-METHODS create
      RETURNING
        VALUE(ro_instance) TYPE REF TO lcl_gui_services_dummy.
ENDCLASS.

CLASS lcl_gui_services_dummy IMPLEMENTATION.
  METHOD create.
    CREATE OBJECT ro_instance.
  ENDMETHOD.
  METHOD zif_abapgit_gui_services~cache_asset.
  ENDMETHOD.
  METHOD zif_abapgit_gui_services~register_event_handler.
  ENDMETHOD.
  METHOD zif_abapgit_gui_services~get_current_page_name.
  ENDMETHOD.
  METHOD zif_abapgit_gui_services~get_hotkeys_ctl.
  ENDMETHOD.
  METHOD zif_abapgit_gui_services~get_html_parts.
  ENDMETHOD.
  METHOD zif_abapgit_gui_services~get_log.
  ENDMETHOD.
  METHOD zif_abapgit_gui_services~register_page_asset.
  ENDMETHOD.

ENDCLASS.
