CLASS zcl_abapgit_gui_page_flowuns DEFINITION
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

    TYPES: BEGIN OF ty_unsupported,
             obj_type TYPE tadir-object,
             count    TYPE i,
           END OF ty_unsupported.
    TYPES ty_unsupported_tt TYPE STANDARD TABLE OF ty_unsupported WITH DEFAULT KEY.

ENDCLASS.



CLASS zcl_abapgit_gui_page_flowuns IMPLEMENTATION.

  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_flowuns.

    CREATE OBJECT lo_component.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Flow Unsupported Object Types'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.

  METHOD zif_abapgit_gui_menu_provider~get_menu.

    ro_toolbar = zcl_abapgit_html_toolbar=>create( 'toolbar-flow-uns' ).

    ro_toolbar->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.

  METHOD zif_abapgit_gui_renderable~render.

    DATA lt_repos           TYPE zcl_abapgit_flow_logic=>ty_repos_tt.
    DATA li_repo_online     LIKE LINE OF lt_repos.
    DATA li_repo            TYPE REF TO zif_abapgit_repo.
    DATA lt_tadir           TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lt_supported_types TYPE zif_abapgit_objects=>ty_types_tt.
    DATA lt_unsupported     TYPE ty_unsupported_tt.
    DATA ls_unsupported     TYPE ty_unsupported.

    FIELD-SYMBOLS <ls_tadir>       LIKE LINE OF lt_tadir.
    FIELD-SYMBOLS <ls_unsupported> LIKE LINE OF lt_unsupported.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div class="repo-overview">' ).
    ri_html->add( '<h1>Flow Unsupported Object Types</h1>' ).

    lt_repos = zcl_abapgit_flow_logic=>list_repos( abap_false ).
    IF lines( lt_repos ) = 0.
      ri_html->add( zcl_abapgit_gui_chunk_lib=>render_warning_banner( 'No flow-enabled repositories found' ) ).
      ri_html->add( '</div>' ).
      RETURN.
    ENDIF.

    lt_supported_types = zcl_abapgit_objects=>supported_list( ).

    LOOP AT lt_repos INTO li_repo_online.
      li_repo ?= li_repo_online.
      lt_tadir = li_repo->get_tadir_objects( ).
      CLEAR lt_unsupported.

      LOOP AT lt_tadir ASSIGNING <ls_tadir>.
        READ TABLE lt_supported_types WITH KEY table_line = <ls_tadir>-object TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          CONTINUE.
        ENDIF.

        READ TABLE lt_unsupported ASSIGNING <ls_unsupported>
          WITH KEY obj_type = <ls_tadir>-object.
        IF sy-subrc = 0.
          <ls_unsupported>-count = <ls_unsupported>-count + 1.
        ELSE.
          CLEAR ls_unsupported.
          ls_unsupported-obj_type = <ls_tadir>-object.
          ls_unsupported-count = 1.
          APPEND ls_unsupported TO lt_unsupported.
        ENDIF.
      ENDLOOP.

      SORT lt_unsupported BY count DESCENDING obj_type ASCENDING.

      ri_html->add( |<h2>{ li_repo->get_name( ) }</h2>| ).
      ri_html->add( |<table>| ).
      ri_html->add( |<tr><td><u>Object Type</u></td><td><u>Count</u></td></tr>| ).

      IF lt_unsupported IS INITIAL.
        ri_html->add( |<tr><td colspan="2">None</td></tr>| ).
      ELSE.
        LOOP AT lt_unsupported INTO ls_unsupported.
          ri_html->add( |<tr><td>{ ls_unsupported-obj_type }</td><td>{ ls_unsupported-count }</td></tr>| ).
        ENDLOOP.
      ENDIF.

      ri_html->add( |</table>| ).
    ENDLOOP.

    ri_html->add( '</div>' ).

  ENDMETHOD.

ENDCLASS.
