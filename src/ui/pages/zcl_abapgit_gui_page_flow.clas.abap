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



CLASS ZCL_ABAPGIT_GUI_PAGE_FLOW IMPLEMENTATION.


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
    DATA lt_features  TYPE lcl_helper=>ty_features.
    DATA ls_feature   LIKE LINE OF lt_features.
    DATA ls_path_name LIKE LINE OF ls_feature-changed_files.
    DATA ls_item      LIKE LINE OF ls_feature-changed_objects.
    DATA lv_status    TYPE string.


    register_handlers( ).
    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( '<div class="repo-overview">' ).

    lt_features = lcl_helper=>get_information( ).
    LOOP AT lt_features INTO ls_feature.
      IF lines( ls_feature-changed_files ) = 0.
* no changes, eg. only files outside of starting folder changed
        CONTINUE.
      ENDIF.

      ri_html->add( '<b><font size="+2">' && ls_feature-repo_name && | - | ).
      ri_html->add_icon( 'code-branch' ).
      ri_html->add( ls_feature-branch-display_name && | - | ).
      ri_html->add_icon( 'truck-solid' ).
      ri_html->add( |<tt>{ ls_feature-transport-trkorr }</tt></font></b><br>| ).

      IF ls_feature-pr IS NOT INITIAL.
        ri_html->add_a(
          iv_txt   = ls_feature-pr-title
          iv_act   = |{ zif_abapgit_definitions=>c_action-url }?url={ ls_feature-pr-url }|
          iv_class = |url| ).

        IF ls_feature-pr-draft = abap_true.
          ri_html->add( 'DRAFT' ).
        ENDIF.
      ENDIF.

      ri_html->add( '<br>' ).
      IF ls_feature-branch-up_to_date = abap_false.
        ri_html->add( 'NONONONONO UPDATED<br><br>' ).
        CONTINUE.
      ENDIF.

      ri_html->add( |<table>| ).
      ri_html->add( |<tr><td><u>Filename</u></td><td><u>Remote SHA1</u></td>| &&
                    |<td><u>Local SHA1</u></td><td></td></tr>| ).
      LOOP AT ls_feature-changed_files INTO ls_path_name.
        lv_status = 'Diff'.
        IF ls_path_name-remote_sha1 = ls_path_name-local_sha1.
          lv_status = 'Match'.
        ENDIF.
        ri_html->add( |<tr><td><tt>{ ls_path_name-path }{ ls_path_name-name }</tt></td><td>{
          ls_path_name-remote_sha1(7) }</td><td>{
          ls_path_name-local_sha1(7) }</td><td>{ lv_status }</td></tr>| ).
      ENDLOOP.
      ri_html->add( |</table>| ).
      LOOP AT ls_feature-changed_objects INTO ls_item.
        ri_html->add( |<tt>{ ls_item-obj_type } { ls_item-obj_name }</tt><br>| ).
      ENDLOOP.
      ri_html->add( '<br>' ).
    ENDLOOP.

    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
