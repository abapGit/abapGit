CLASS lcl_pr_popup DEFINITION FINAL.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_render_item.
    INTERFACES zif_abapgit_gui_page_title.
    INTERFACES zif_abapgit_gui_modal.

    CLASS-METHODS new
      IMPORTING
        iv_url TYPE string
      RETURNING
        VALUE(ro_instance) TYPE REF TO lcl_pr_popup.
    METHODS create_picklist_component
      RETURNING
        VALUE(ro_picklist) TYPE REF TO zcl_abapgit_gui_picklist
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.

    DATA mv_repo_url TYPE string.

    METHODS fetch_pull_request_list
      RETURNING
        VALUE(rt_pulls) TYPE zif_abapgit_pr_enum_provider=>ty_pull_requests
      RAISING
        zcx_abapgit_exception.

ENDCLASS.

CLASS lcl_pr_popup IMPLEMENTATION.

  METHOD new.
    CREATE OBJECT ro_instance.
    ro_instance->mv_repo_url = iv_url.
  ENDMETHOD.

  METHOD create_picklist_component.

    CREATE OBJECT ro_picklist
      EXPORTING
        ii_item_renderer = me
        it_list          = fetch_pull_request_list( ).

  ENDMETHOD.

  METHOD fetch_pull_request_list.

    rt_pulls = zcl_abapgit_pr_enumerator=>new( mv_repo_url )->get_pulls( ).

    IF lines( rt_pulls ) = 0.
      zcx_abapgit_exception=>raise( 'No pull requests found' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_render_item~render.

    FIELD-SYMBOLS <ls_pr> TYPE zif_abapgit_pr_enum_provider=>ty_pull_request.

    ASSIGN iv_item TO <ls_pr>.
    ASSERT sy-subrc = 0.

    ri_html = zcl_abapgit_html=>create( |<b>{ <ls_pr>-number
      }</b> - { <ls_pr>-title } @{ <ls_pr>-user }| ).

  ENDMETHOD.

  METHOD zif_abapgit_gui_page_title~get_page_title.
    rv_title = 'Choose Pull Request'.
  ENDMETHOD.

  METHOD zif_abapgit_gui_modal~is_modal.
    rv_yes = abap_true.
  ENDMETHOD.

ENDCLASS.

**********************************************************************

CLASS lcl_branch_popup DEFINITION FINAL.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_render_item.

    CLASS-METHODS new
      IMPORTING
        !iv_url TYPE string
        !iv_default_branch  TYPE string OPTIONAL
        !iv_show_new_option TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ro_instance) TYPE REF TO lcl_branch_popup.
    METHODS constructor
      IMPORTING
        !iv_url TYPE string
        !iv_default_branch  TYPE string OPTIONAL
        !iv_show_new_option TYPE abap_bool DEFAULT abap_false.

    METHODS create_picklist_component
      RETURNING
        VALUE(ro_picklist) TYPE REF TO zcl_abapgit_gui_picklist
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.

    DATA mv_repo_url TYPE string.
    DATA mv_default_branch TYPE string.
    DATA mv_show_new_option TYPE abap_bool.

    METHODS fetch_branch_list
      RETURNING
        VALUE(rt_branches) TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
      RAISING
        zcx_abapgit_exception.

ENDCLASS.

CLASS lcl_branch_popup IMPLEMENTATION.

  METHOD new.
    CREATE OBJECT ro_instance
      EXPORTING
        iv_url             = iv_url
        iv_default_branch  = iv_default_branch
        iv_show_new_option = iv_show_new_option.
  ENDMETHOD.

  METHOD constructor.
    mv_repo_url = iv_url.
    mv_default_branch = zif_abapgit_definitions=>c_git_branch-heads_prefix && iv_default_branch.
    mv_show_new_option = iv_show_new_option.
  ENDMETHOD.

  METHOD create_picklist_component.
  ENDMETHOD.

  METHOD fetch_branch_list.

    DATA lo_branches    TYPE REF TO zcl_abapgit_git_branch_list.
    DATA lv_head_symref TYPE string.

    FIELD-SYMBOLS <ls_branch> LIKE LINE OF rt_branches.

    lo_branches    = zcl_abapgit_git_transport=>branches( mv_repo_url ).
    rt_branches    = lo_branches->get_branches_only( ).
    lv_head_symref = lo_branches->get_head_symref( ).

    IF rt_branches IS INITIAL.
      zcx_abapgit_exception=>raise( 'No branches are available to select' ).
    ENDIF.

    " Clean up branches: HEAD duplicates, empty names
    LOOP AT rt_branches ASSIGNING <ls_branch>.
      IF <ls_branch>-name IS INITIAL.
        DELETE rt_branches INDEX sy-tabix.
      ELSEIF <ls_branch>-is_head = abap_true AND lv_head_symref IS NOT INITIAL AND <ls_branch>-name <> lv_head_symref.
        DELETE rt_branches INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

    SORT rt_branches BY is_head DESCENDING display_name ASCENDING.

    IF mv_show_new_option = abap_true.
      APPEND INITIAL LINE TO rt_branches ASSIGNING <ls_branch>.
      <ls_branch>-name = zif_abapgit_popups=>c_new_branch_label.
      <ls_branch>-display_name = zif_abapgit_popups=>c_new_branch_label.
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_gui_render_item~render.

    DATA lv_head_marker TYPE string.
    FIELD-SYMBOLS <ls_b> TYPE zif_abapgit_git_definitions=>ty_git_branch.

    ASSIGN iv_item TO <ls_b>.
    ASSERT sy-subrc = 0.

    " TODO render mv_default_branch properly

    IF <ls_b>-is_head = abap_true.
      lv_head_marker = | (<b>{ zif_abapgit_definitions=>c_head_name }</b>)|.
    ENDIF.

    ri_html = zcl_abapgit_html=>create( |{ <ls_b>-display_name }{ lv_head_marker }| ).

  ENDMETHOD.

ENDCLASS.
