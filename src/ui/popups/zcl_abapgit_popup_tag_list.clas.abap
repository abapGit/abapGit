CLASS zcl_abapgit_popup_tag_list DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_render_item.
    INTERFACES zif_abapgit_html_popup.

    CLASS-METHODS create
      IMPORTING
        iv_url          TYPE string
      RETURNING
        VALUE(ri_popup) TYPE REF TO zif_abapgit_html_popup.

    METHODS constructor
      IMPORTING
        iv_url TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_repo_url TYPE string.

    METHODS fetch_tag_list
      RETURNING
        VALUE(rt_tags) TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_popup_tag_list IMPLEMENTATION.


  METHOD constructor.
    mv_repo_url = iv_url.
  ENDMETHOD.


  METHOD create.
    CREATE OBJECT ri_popup TYPE zcl_abapgit_popup_tag_list
      EXPORTING
        iv_url = iv_url.
  ENDMETHOD.


  METHOD fetch_tag_list.

    DATA lo_branches  TYPE REF TO zcl_abapgit_git_branch_list.

    lo_branches = zcl_abapgit_git_factory=>get_git_transport( )->branches( mv_repo_url ).
    rt_tags     = lo_branches->get_tags_only( ).

    DELETE rt_tags WHERE name CP '*' && zif_abapgit_git_definitions=>c_git_branch-peel.

    IF lines( rt_tags ) = 0.
      zcx_abapgit_exception=>raise( 'No tags are available to select' ).
    ENDIF.

    SORT rt_tags BY display_name ASCENDING.

  ENDMETHOD.


  METHOD zif_abapgit_gui_render_item~render.

    FIELD-SYMBOLS <ls_tag> TYPE zif_abapgit_git_definitions=>ty_git_branch.

    ASSIGN iv_item TO <ls_tag>.

    ri_html = zcl_abapgit_html=>create( |{ <ls_tag>-display_name }| ).

  ENDMETHOD.


  METHOD zif_abapgit_html_popup~create_picklist.

    CREATE OBJECT ro_picklist
      EXPORTING
        iv_title         = 'Choose Tag'
        it_list          = fetch_tag_list( )
        ii_item_renderer = me.

  ENDMETHOD.
ENDCLASS.
