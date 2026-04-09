CLASS zcl_abapgit_gui_page_flowtsht DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_renderable .
    INTERFACES zif_abapgit_gui_menu_provider .

    CLASS-METHODS create
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abapgit_gui_page_flowtsht IMPLEMENTATION.

  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_flowtsht.

    CREATE OBJECT lo_component.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Flow Troubleshooting'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.

  METHOD zif_abapgit_gui_menu_provider~get_menu.

    ro_toolbar = zcl_abapgit_html_toolbar=>create( 'toolbar-flow-tsht' ).

    ro_toolbar->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.

  METHOD zif_abapgit_gui_renderable~render.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div class="repo-overview">' ).

    ri_html->add( |<h1>Flow Troubleshooting</h1>| ).
    ri_html->add( |<h2>Why is my transport not shown?</h2>| ).
    ri_html->add( |<ul>| ).
    ri_html->add( |<li>Transport must contain repository objects</li>| ).
    ri_html->add( |<li>These objects must be in a package that is part of a repository</li>| ).
    ri_html->add( |<li>The repository must be flow enabled</li>| ).
    ri_html->add( |<li>The user(you) must have the repository as a favorite</li>| ).
    ri_html->add( |</ul>| ).

    ri_html->add( '</div>' ).

  ENDMETHOD.

ENDCLASS.
