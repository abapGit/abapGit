CLASS zcl_abapgit_gui_page_bkg_run DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      RAISING zcx_abapgit_exception.

    METHODS zif_abapgit_gui_event_handler~on_event
        REDEFINITION .
  PROTECTED SECTION.
    METHODS render_content        REDEFINITION.

  PRIVATE SECTION.
    DATA: mt_text TYPE TABLE OF string.

    METHODS: run.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_BKG_RUN IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'BACKGROUND_RUN'.
  ENDMETHOD.


  METHOD render_content.

    DATA: lv_text LIKE LINE OF mt_text.

    run( ).

    CREATE OBJECT ro_html.

    ro_html->add( '<div id="toc">' ).
    LOOP AT mt_text INTO lv_text.
      ro_html->add( '<pre>' && lv_text && '</pre><br>' ).
    ENDLOOP.
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD run.

    DATA: lx_error TYPE REF TO zcx_abapgit_exception,
          lv_text  TYPE string,
          lv_line  TYPE i VALUE 1.


    TRY.
        zcl_abapgit_background=>run( ).

        DO.
          READ LINE lv_line LINE VALUE INTO lv_text.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
          APPEND lv_text TO mt_text.
          lv_line = lv_line + 1.
        ENDDO.
      CATCH zcx_abapgit_exception INTO lx_error.
        APPEND lx_error->get_text( ) TO mt_text.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.
    RETURN.
  ENDMETHOD.
ENDCLASS.
