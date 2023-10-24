CLASS zcl_abapgit_popup_pull_request DEFINITION
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

  PRIVATE SECTION.

    DATA mv_repo_url TYPE string.

    METHODS fetch_pull_request_list
      RETURNING
        VALUE(rt_pulls) TYPE zif_abapgit_pr_enum_provider=>ty_pull_requests
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_popup_pull_request IMPLEMENTATION.


  METHOD create.

    CREATE OBJECT ri_popup TYPE zcl_abapgit_popup_pull_request
      EXPORTING
        iv_url = iv_url.

  ENDMETHOD.


  METHOD constructor.
    mv_repo_url = iv_url.
  ENDMETHOD.


  METHOD zif_abapgit_html_popup~create_picklist.

    CREATE OBJECT ro_picklist
      EXPORTING
        iv_title         = 'Choose Pull Request'
        it_list          = fetch_pull_request_list( )
        ii_item_renderer = me.

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

    ri_html = zcl_abapgit_html=>create( |<b>{ <ls_pr>-number }</b> - { <ls_pr>-title } @{ <ls_pr>-user }| ).

  ENDMETHOD.
ENDCLASS.
