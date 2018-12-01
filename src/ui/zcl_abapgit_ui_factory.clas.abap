CLASS zcl_abapgit_ui_factory DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_ui_injector.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_popups
        RETURNING
          VALUE(ri_popups) TYPE REF TO zif_abapgit_popups,

      get_tag_popups
        RETURNING
          VALUE(ri_tag_popups) TYPE REF TO zif_abapgit_tag_popups,

      get_gui_functions
        RETURNING
          VALUE(ri_gui_functions) TYPE REF TO zif_abapgit_gui_functions.

    CLASS-METHODS: get_gui
      RETURNING
        VALUE(ro_gui) TYPE REF TO zcl_abapgit_gui
      RAISING
        zcx_abapgit_exception.


  PRIVATE SECTION.
    CLASS-DATA:
      gi_popups        TYPE REF TO zif_abapgit_popups,
      gi_tag_popups    TYPE REF TO zif_abapgit_tag_popups,
      gi_gui_functions TYPE REF TO zif_abapgit_gui_functions,
      go_gui           TYPE REF TO zcl_abapgit_gui.

ENDCLASS.


CLASS zcl_abapgit_ui_factory IMPLEMENTATION.

  METHOD get_popups.

    IF gi_popups IS INITIAL.
      CREATE OBJECT gi_popups TYPE zcl_abapgit_popups.
    ENDIF.

    ri_popups = gi_popups.

  ENDMETHOD.


  METHOD get_tag_popups.

    IF gi_tag_popups IS INITIAL.
      CREATE OBJECT gi_tag_popups TYPE zcl_abapgit_tag_popups.
    ENDIF.

    ri_tag_popups = gi_tag_popups.

  ENDMETHOD.


  METHOD get_gui_functions.

    IF gi_gui_functions IS INITIAL.
      CREATE OBJECT gi_gui_functions TYPE zcl_abapgit_gui_functions.
    ENDIF.

    ri_gui_functions = gi_gui_functions.

  ENDMETHOD.

  METHOD get_gui.

    DATA:
          li_router    TYPE REF TO zif_abapgit_gui_router,
          li_asset_man TYPE REF TO zif_abapgit_gui_asset_manager.

    IF go_gui IS INITIAL.
      CREATE OBJECT li_router TYPE zcl_abapgit_gui_router.
      CREATE OBJECT li_asset_man TYPE zcl_abapgit_gui_asset_manager.
      CREATE OBJECT go_gui
        EXPORTING
          ii_router    = li_router
          ii_asset_man = li_asset_man.
    ENDIF.
    ro_gui = go_gui.

  ENDMETHOD.

ENDCLASS.
