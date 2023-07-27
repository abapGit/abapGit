CLASS zcl_abapgit_popup_branch_list DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_render_item.
    INTERFACES zif_abapgit_html_popup.

    CLASS-METHODS create
      IMPORTING
        !iv_url             TYPE string
        !iv_default_branch  TYPE string OPTIONAL
        !iv_show_new_option TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_popup)     TYPE REF TO zif_abapgit_html_popup.

    METHODS constructor
      IMPORTING
        !iv_url             TYPE string
        !iv_default_branch  TYPE string OPTIONAL
        !iv_show_new_option TYPE abap_bool DEFAULT abap_false.

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



CLASS zcl_abapgit_popup_branch_list IMPLEMENTATION.

  METHOD create.
    CREATE OBJECT ri_popup TYPE zcl_abapgit_popup_branch_list
      EXPORTING
        iv_url             = iv_url
        iv_default_branch  = iv_default_branch
        iv_show_new_option = iv_show_new_option.
  ENDMETHOD.

  METHOD constructor.
    mv_repo_url        = iv_url.
    mv_default_branch  = zif_abapgit_git_definitions=>c_git_branch-heads_prefix && iv_default_branch.
    mv_show_new_option = iv_show_new_option.
  ENDMETHOD.

  METHOD zif_abapgit_html_popup~create_picklist.

    CREATE OBJECT ro_picklist
      EXPORTING
        iv_title         = 'Choose Branch'
        it_list          = fetch_branch_list( )
        ii_item_renderer = me.

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

    " TODO render mv_default_branch properly, needs respecting support from the picklist components

    IF <ls_b>-is_head = abap_true.
      lv_head_marker = | (<b>{ zif_abapgit_git_definitions=>c_head_name }</b>)|.
    ENDIF.

    ri_html = zcl_abapgit_html=>create( |{ <ls_b>-display_name }{ lv_head_marker }| ).

  ENDMETHOD.

ENDCLASS.
