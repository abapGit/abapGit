CLASS lcl_pr_renderer DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_gui_render_item.
    CLASS-METHODS new RETURNING value(ro_instance) TYPE REF TO lcl_pr_renderer.
ENDCLASS.

CLASS lcl_pr_renderer IMPLEMENTATION.
  METHOD new.
    CREATE OBJECT ro_instance.
  ENDMETHOD.

  METHOD zif_abapgit_gui_render_item~render.
    FIELD-SYMBOLS <ls_pr> TYPE zif_abapgit_pr_enum_provider=>ty_pull_request.
    ASSIGN iv_item TO <ls_pr>.
    ASSERT sy-subrc = 0.
    ri_html = zcl_abapgit_html=>create( )->add( |<b>{ <ls_pr>-number
      }</b> - { <ls_pr>-title } @{ <ls_pr>-user }| ).
  ENDMETHOD.
ENDCLASS.
