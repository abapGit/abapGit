CLASS lcl_pr_popup DEFINITION FINAL.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_render_item.

    CLASS-METHODS new
      IMPORTING
        iv_url TYPE string
      RETURNING
        value(ro_instance) TYPE REF TO lcl_pr_popup.
    METHODS create_picklist_component
      RETURNING
        VALUE(ro_picklist) TYPE REF TO zcl_abapgit_gui_page_picklist
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.

    DATA mv_repo_url TYPE string.

    METHODS get_pull_request_list
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
        it_list          = get_pull_request_list( ).

  ENDMETHOD.

  METHOD get_pull_request_list.

    rt_pulls = zcl_abapgit_pr_enumerator=>new( mv_repo_url )->get_pulls( ).

    IF lines( rt_pulls ) = 0.
      zcx_abapgit_exception=>raise( 'No pull requests found' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_render_item~render.
    FIELD-SYMBOLS <ls_pr> TYPE zif_abapgit_pr_enum_provider=>ty_pull_request.
    ASSIGN iv_item TO <ls_pr>.
    ASSERT sy-subrc = 0.
    ri_html = zcl_abapgit_html=>create( )->add( |<b>{ <ls_pr>-number
      }</b> - { <ls_pr>-title } @{ <ls_pr>-user }| ).
  ENDMETHOD.
ENDCLASS.
