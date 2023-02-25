CLASS zcl_abapgit_gui_page_run_bckg DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler .
    INTERFACES zif_abapgit_gui_renderable .

    CLASS-METHODS create
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mt_text TYPE TABLE OF string.

    METHODS run.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_RUN_BCKG IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_run_bckg.

    CREATE OBJECT lo_component.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Background Run'
      io_page_menu       = zcl_abapgit_gui_chunk_lib=>back_toolbar( )
      ii_child_component = lo_component ).

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
    rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA: lv_text LIKE LINE OF mt_text.

    register_handlers( ).

    run( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div id="toc">' ).
    LOOP AT mt_text INTO lv_text.
      ri_html->add( '<pre>' && lv_text && '</pre><br>' ).
    ENDLOOP.
    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
