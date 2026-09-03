CLASS zcl_abapgit_gui_page_ref_sel DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.
    INTERFACES zif_abapgit_gui_page_title.

    CLASS-METHODS create
      IMPORTING
        !iv_key        TYPE zif_abapgit_persistence=>ty_repo-key
        !iv_action     TYPE string
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        !iv_key    TYPE zif_abapgit_persistence=>ty_repo-key
        !iv_action TYPE string
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_key TYPE zif_abapgit_persistence=>ty_repo-key.
    DATA mv_action TYPE string.
    DATA mo_picklist TYPE REF TO zcl_abapgit_gui_picklist.

    METHODS create_picklist
      RAISING
        zcx_abapgit_exception.

    METHODS execute_selection
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_gui_page_ref_sel IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    mv_key    = iv_key.
    mv_action = iv_action.

    create_picklist( ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_ref_sel.

    CREATE OBJECT lo_component
      EXPORTING
        iv_key    = iv_key
        iv_action = iv_action.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      ii_child_component = lo_component
      iv_show_as_modal   = abap_true ).

  ENDMETHOD.


  METHOD create_picklist.

    DATA li_repo_online TYPE REF TO zif_abapgit_repo_online.
    DATA lv_url TYPE string.

    li_repo_online ?= zcl_abapgit_repo_srv=>get_instance( )->get( mv_key ).
    lv_url = li_repo_online->get_url( ).

    CASE mv_action.
      WHEN zif_abapgit_definitions=>c_action-git_branch_switch.
        mo_picklist = zcl_abapgit_popup_branch_list=>create(
          iv_url             = lv_url
          iv_default_branch  = zcl_abapgit_git_branch_utils=>get_display_name(
                                 li_repo_online->get_selected_branch( ) )
          iv_show_new_option = abap_true )->create_picklist( ).
      WHEN zif_abapgit_definitions=>c_action-git_branch_delete.
        mo_picklist = zcl_abapgit_popup_branch_list=>create(
          iv_url         = lv_url
          iv_hide_branch = li_repo_online->get_selected_branch( )
          iv_hide_head   = abap_true )->create_picklist( ).
      WHEN zif_abapgit_definitions=>c_action-git_tag_switch
        OR zif_abapgit_definitions=>c_action-git_tag_delete.
        mo_picklist = zcl_abapgit_popup_tag_list=>create( lv_url )->create_picklist( ).
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( |Unexpected ref selection action { mv_action }| ).
    ENDCASE.

    mo_picklist->set_id( mv_action ).

  ENDMETHOD.


  METHOD execute_selection.

    DATA ls_selected TYPE zif_abapgit_git_definitions=>ty_git_branch.
    DATA ls_tag      TYPE zif_abapgit_git_definitions=>ty_git_tag.

    mo_picklist->get_result_item( CHANGING cs_selected = ls_selected ).
    IF ls_selected IS INITIAL.
      RETURN.
    ENDIF.

    CASE mv_action.
      WHEN zif_abapgit_definitions=>c_action-git_branch_switch.
        zcl_abapgit_services_git=>switch_branch(
          iv_key    = mv_key
          is_branch = ls_selected ).
      WHEN zif_abapgit_definitions=>c_action-git_branch_delete.
        zcl_abapgit_services_git=>delete_branch(
          iv_key    = mv_key
          is_branch = ls_selected ).
      WHEN zif_abapgit_definitions=>c_action-git_tag_switch.
        MOVE-CORRESPONDING ls_selected TO ls_tag.
        zcl_abapgit_services_git=>switch_tag(
          iv_key = mv_key
          is_tag = ls_tag ).
      WHEN zif_abapgit_definitions=>c_action-git_tag_delete.
        MOVE-CORRESPONDING ls_selected TO ls_tag.
        zcl_abapgit_services_git=>delete_tag(
          iv_key = mv_key
          is_tag = ls_tag ).
        zcl_abapgit_services_repo=>refresh( mv_key ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    " The picklist handles its own events (it is registered after this
    " component, see render). Delegate explicitly and evaluate the outcome
    rs_handled = mo_picklist->zif_abapgit_gui_event_handler~on_event( ii_event ).

    IF mo_picklist->is_fulfilled( ) = abap_true AND mo_picklist->was_cancelled( ) = abap_false.
      execute_selection( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_title~get_page_title.

    CASE mv_action.
      WHEN zif_abapgit_definitions=>c_action-git_branch_switch.
        rv_title = 'Switch Branch'.
      WHEN zif_abapgit_definitions=>c_action-git_branch_delete.
        rv_title = 'Delete Branch'.
      WHEN zif_abapgit_definitions=>c_action-git_tag_switch.
        rv_title = 'Switch Tag'.
      WHEN zif_abapgit_definitions=>c_action-git_tag_delete.
        rv_title = 'Delete Tag'.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    ri_html = mo_picklist->zif_abapgit_gui_renderable~render( ).

    " Register this component after the picklist so that on_event above
    " is called first and can post-process the picklist result
    register_handlers( ).

  ENDMETHOD.
ENDCLASS.
