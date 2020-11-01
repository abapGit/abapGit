CLASS zcl_abapgit_html_viewer_gui DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_html_viewer .

    METHODS constructor .
  PROTECTED SECTION.

    DATA mo_html_viewer TYPE REF TO cl_gui_html_viewer .

    METHODS on_event
          FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING
          !action
          !frame
          !getdata
          !postdata
          !query_table .

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_html_viewer_gui IMPLEMENTATION.


  METHOD constructor.

    DATA: lt_events TYPE cntl_simple_events,
          ls_event  LIKE LINE OF lt_events.

    CREATE OBJECT mo_html_viewer
      EXPORTING
        query_table_disabled = abap_true
        parent               = cl_gui_container=>screen0.

    ls_event-eventid    = zif_abapgit_html_viewer=>m_id_sapevent.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_events.

    mo_html_viewer->set_registered_events( lt_events ).
    SET HANDLER on_event FOR mo_html_viewer.

  ENDMETHOD.


  METHOD on_event.

    RAISE EVENT zif_abapgit_html_viewer~sapevent
      EXPORTING
        action      = action
        frame       = frame
        getdata     = getdata
        postdata    = postdata
        query_table = query_table.

  ENDMETHOD.


  METHOD zif_abapgit_html_viewer~close_document.

    mo_html_viewer->close_document( ).

  ENDMETHOD.


  METHOD zif_abapgit_html_viewer~free.

    mo_html_viewer->free( ).

  ENDMETHOD.


  METHOD zif_abapgit_html_viewer~load_data.

    mo_html_viewer->load_data(
      EXPORTING
        url           = iv_url
        type          = iv_type
        subtype       = iv_subtype
        size          = iv_size
      IMPORTING
        assigned_url  = ev_assigned_url
      CHANGING
        data_table    = ct_data_table ).

  ENDMETHOD.


  METHOD zif_abapgit_html_viewer~set_registered_events.

    mo_html_viewer->set_registered_events( it_events ).

  ENDMETHOD.


  METHOD zif_abapgit_html_viewer~show_url.

    mo_html_viewer->show_url( iv_url ).

  ENDMETHOD.
ENDCLASS.
