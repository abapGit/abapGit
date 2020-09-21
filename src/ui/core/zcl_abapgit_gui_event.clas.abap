CLASS zcl_abapgit_gui_event DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event.

    METHODS constructor
      IMPORTING
        ii_gui_services TYPE REF TO zif_abapgit_gui_services
        iv_action       TYPE clike
        iv_getdata      TYPE clike OPTIONAL
        it_postdata     TYPE cnht_post_data_tab OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_EVENT IMPLEMENTATION.


  METHOD constructor.

    zif_abapgit_gui_event~mi_gui_services = ii_gui_services.
    zif_abapgit_gui_event~mv_action       = iv_action.
    zif_abapgit_gui_event~mv_getdata      = iv_getdata.
    zif_abapgit_gui_event~mt_postdata     = it_postdata.

  ENDMETHOD.
ENDCLASS.
