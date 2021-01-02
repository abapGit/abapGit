CLASS zcl_abapgit_gui_page_data DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
  PROTECTED SECTION.

    METHODS render_content
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_DATA IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    ms_control-page_title = 'Data'.

  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( 'hello world' ).



*    DATA lo_form TYPE REF TO zcl_abapgit_html_form.
*
*    lo_form = zcl_abapgit_html_form=>create( iv_form_id = 'add-repo-online-form' ).
*    lo_form->text(
*      iv_label       = 'Table'
*      iv_name        = 'table' ).
*
*
*    ri_html->add( lo_form->render(
*        iv_form_class     = 'dialog w600px m-em5-sides margin-v1'
*        io_values         = mo_form_data
*        io_validation_log = mo_validation_log ) ).


  ENDMETHOD.
ENDCLASS.
