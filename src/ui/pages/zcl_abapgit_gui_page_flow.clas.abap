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

    CONSTANTS: BEGIN OF c_action,
                 refresh TYPE string VALUE 'refresh',
               END OF c_action.

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

    IF ii_event->mv_action = c_action-refresh.
      rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar EXPORTING iv_id = 'toolbar-main'.

    ro_toolbar->add(
      iv_txt = 'Refresh'
      iv_act = c_action-refresh ).

    ro_toolbar->add(
      iv_txt = zcl_abapgit_gui_buttons=>repo_list( )
      iv_act = zif_abapgit_definitions=>c_action-abapgit_home ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.
    DATA lt_favorites TYPE zif_abapgit_repo_srv=>ty_repo_list.
    DATA li_favorite  LIKE LINE OF lt_favorites.
    DATA lo_online    TYPE REF TO zcl_abapgit_repo_online.
    DATA lt_branches  TYPE lcl_helper=>ty_branches.
    DATA ls_branch    LIKE LINE OF lt_branches.
    DATA ls_path_name LIKE LINE OF ls_branch-changed_files.
    DATA ls_item      LIKE LINE OF ls_branch-changed_objects.
    DATA lv_status TYPE string.


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

      lt_branches = lcl_helper=>get_information( lo_online ).
      LOOP AT lt_branches INTO ls_branch.
        ri_html->add( '<b><font size="+2">' && li_favorite->get_name( ) && | - | ).
        ri_html->add_icon( 'code-branch' ).
        ri_html->add( ls_branch-branch-display_name && | - | ).
        ri_html->add_icon( 'truck-solid' ).
        ri_html->add( |<tt>{ ls_branch-transport-trkorr }</tt></font></b><br>| ).

        IF ls_branch-pr IS NOT INITIAL.
          ri_html->add_a(
            iv_txt   = ls_branch-pr-title
            iv_act   = |{ zif_abapgit_definitions=>c_action-url }?url={ ls_branch-pr-url }|
            iv_class = |url| ).

          IF ls_branch-pr-draft = abap_true.
            ri_html->add( 'DRAFT' ).
          ENDIF.
        ENDIF.

        ri_html->add( '<br>' ).
        IF lines( ls_branch-changed_files ) = 0.
          ri_html->add( 'NO CHANGES<br><br>' ).
          CONTINUE.
        ELSEIF ls_branch-branch-up_to_date = abap_false.
          ri_html->add( 'NONONONONO UPDATED<br><br>' ).
          CONTINUE.
        ENDIF.

        ri_html->add( |<table>| ).
        ri_html->add( |<tr><td><u>Filename</u></td><td><u>Remote SHA1</u></td>| &&
                      |<td><u>Local SHA1</u></td><td></td></tr>| ).
        LOOP AT ls_branch-changed_files INTO ls_path_name.
          lv_status = 'Diff'.
          IF ls_path_name-remote_sha1 = ls_path_name-local_sha1.
            lv_status = 'Match'.
          ENDIF.
          ri_html->add( |<tr><td><tt>{ ls_path_name-path }{ ls_path_name-name }</tt></td><td>{
            ls_path_name-remote_sha1(7) }</td><td>{
            ls_path_name-local_sha1(7) }</td><td>{ lv_status }</td></tr>| ).
        ENDLOOP.
        ri_html->add( |</table>| ).
        LOOP AT ls_branch-changed_objects INTO ls_item.
          ri_html->add( |<tt>{ ls_item-obj_type } { ls_item-obj_name }</tt><br>| ).
        ENDLOOP.
        ri_html->add( '<br>' ).
      ENDLOOP.
    ENDLOOP.

* list open transports for current user
* todo

    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
