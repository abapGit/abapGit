CLASS zcl_abapgit_gui_page_flow DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.
    INTERFACES zif_abapgit_gui_menu_provider.

    CLASS-METHODS create
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abapgit_gui_page_flow IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_flow.

    CREATE OBJECT lo_component.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Flow'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.
    RETURN. " todo, implement method
  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar EXPORTING iv_id = 'toolbar-main'.

    ro_toolbar->add(
      iv_txt = zcl_abapgit_gui_buttons=>repo_list( )
      iv_act = zif_abapgit_definitions=>c_action-abapgit_home ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.
    DATA lt_favorites TYPE zif_abapgit_repo_srv=>ty_repo_list.
    DATA li_favorite  LIKE LINE OF lt_favorites.
    DATA lo_online    TYPE REF TO zcl_abapgit_repo_online.


    register_handlers( ).
    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( '<div class="repo-overview">' ).

* list branches on favorite transported repos
    lt_favorites = zcl_abapgit_repo_srv=>get_instance( )->list_favorites( abap_false ).
    LOOP AT lt_favorites INTO li_favorite.
      " todo, IF zcl_abapgit_factory=>get_sap_package( li_favorite->get_package( )
      " todo,     )->are_changes_recorded_in_tr_req( ) = abap_false.
      " todo,   CONTINUE.
      " todo, ENDIF.

      lo_online ?= li_favorite.
      ri_html->add( '<u>' && li_favorite->get_name( ) && '</u><br>' ).

      lcl_helper=>list_changes_per_branch( lo_online ).
    ENDLOOP.

* list open transports for current user
* todo

    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
