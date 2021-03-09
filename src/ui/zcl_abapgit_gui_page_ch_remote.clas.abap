CLASS zcl_abapgit_gui_page_ch_remote DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.

    METHODS render_content
        REDEFINITION .
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_event,
        go_back TYPE string VALUE 'go_back',
        save    TYPE string VALUE 'save',
      END OF c_event .
    DATA mv_key TYPE zif_abapgit_persistence=>ty_repo-key .
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_CH_REMOTE IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    mv_key = iv_key.
    ms_control-page_title = 'Change Remote'.

  ENDMETHOD.


  METHOD render_content.

    DATA lo_form TYPE REF TO zcl_abapgit_html_form.
    DATA lo_map TYPE REF TO zcl_abapgit_string_map.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    CREATE OBJECT lo_map.


    lo_form = zcl_abapgit_html_form=>create( ).

    lo_form->text(
      iv_name     = 'REMOTE'
      iv_required = abap_true
      iv_label    = 'Remote' ).
    lo_map->set(
      iv_key = 'REMOTE'
      iv_val = 'sdf' ).

    lo_form->command(
      iv_label    = 'Save Settings'
      iv_cmd_type = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action   = c_event-save ).

    lo_form->command(
      iv_label  = 'Back'
      iv_action = c_event-go_back ).

    ri_html->add( lo_form->render( lo_map ) ).

  ENDMETHOD.
ENDCLASS.
