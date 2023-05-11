CLASS zcl_abapgit_gui_popup_picklist DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_popup
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS c_selected_row TYPE string VALUE 'selected_row'.

    CONSTANTS:
      BEGIN OF c_event,
        ok     TYPE string VALUE 'ok',
        cancel TYPE string VALUE 'cancel',
      END OF c_event.

    CLASS-METHODS create
      IMPORTING
        !iv_form_id     TYPE string
        !it_list        TYPE STANDARD TABLE
      RETURNING
        VALUE(ro_popup) TYPE REF TO zcl_abapgit_gui_popup_picklist
      RAISING
        zcx_abapgit_exception.

    METHODS validate
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS get_form_schema
      IMPORTING
        !iv_form_id    TYPE string
        it_list        TYPE STANDARD TABLE
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_gui_popup_picklist IMPLEMENTATION.


  METHOD create.

    CREATE OBJECT ro_popup
      EXPORTING
        iv_form_id = iv_form_id
        io_form    = get_form_schema(
                       iv_form_id = iv_form_id
                       it_list    = it_list ).

    ro_popup->set_dimensions( iv_height = 120 + lines( it_list ) * 25 ).

  ENDMETHOD.


  METHOD get_form_schema.

    FIELD-SYMBOLS <ls_row> TYPE any.

    ro_form = zcl_abapgit_html_form=>create( iv_form_id = iv_form_id ).

    " Always prefix names with form id to distinguish them from the calling page and other popups
    ro_form->radio(
      iv_name     = |{ iv_form_id }-{ c_selected_row }|
      iv_condense = abap_true
      iv_label    = 'Select a Value from the List' ).

    LOOP AT it_list ASSIGNING <ls_row>.
      ro_form->option(
        iv_label = <ls_row>
        iv_value = |{ sy-tabix }| ).
    ENDLOOP.

    ro_form->command(
      iv_label    = 'OK'
      iv_cmd_type = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action   = |{ iv_form_id }-{ c_event-ok }|
    )->command(
      iv_label    = 'Cancel'
      iv_action   = |{ iv_form_id }-{ c_event-cancel }| ).

  ENDMETHOD.


  METHOD validate.

    super->validate( ).

    IF get_value( c_selected_row ) IS INITIAL.
      add_validation_error(
        iv_key  = c_selected_row
        iv_text = |You have to select one item| ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
