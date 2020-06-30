INTERFACE zif_abapgit_gui_event_handler
  PUBLIC .

  METHODS on_event
    IMPORTING
      iv_action    TYPE clike
      iv_getdata   TYPE clike OPTIONAL
      it_postdata  TYPE cnht_post_data_tab OPTIONAL
    EXPORTING
      ei_page      TYPE REF TO zif_abapgit_gui_renderable
      ev_state     TYPE i
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
