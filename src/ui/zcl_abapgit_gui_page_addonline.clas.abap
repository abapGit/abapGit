CLASS zcl_abapgit_gui_page_addonline DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS create
      RETURNING
        VALUE(ro_page) TYPE REF TO zcl_abapgit_gui_page_addonline
      RAISING
        zcx_abapgit_exception.

    METHODS zif_abapgit_gui_event_handler~on_event REDEFINITION .


  PROTECTED SECTION.

    METHODS render_content REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_ADDONLINE IMPLEMENTATION.


  METHOD create.
    CREATE OBJECT ro_page.
  ENDMETHOD.


  METHOD render_content.

    DATA lo_form TYPE REF TO zcl_abapgit_html_form.

    CREATE OBJECT ro_html.

    lo_form = zcl_abapgit_html_form=>create( ).
    lo_form->text(
      iv_name = 'abc'
      iv_label = 'abcd' ).

    ro_html->add( lo_form->render(
      iv_form_class = 'dialog'
      iv_action = 'dododo' ) ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.
  ENDMETHOD.
ENDCLASS.
