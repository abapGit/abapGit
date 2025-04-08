CLASS zcl_abapgit_ui_core_factory DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_ui_core_injector .

  PUBLIC SECTION.

    CLASS-METHODS get_html_viewer
      IMPORTING
        !io_container           TYPE REF TO cl_gui_container DEFAULT cl_gui_container=>screen0
        !iv_disable_query_table TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(ri_viewer)        TYPE REF TO zif_abapgit_html_viewer .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gi_html_viewer TYPE REF TO zif_abapgit_html_viewer .
ENDCLASS.



CLASS zcl_abapgit_ui_core_factory IMPLEMENTATION.


  METHOD get_html_viewer.

    IF gi_html_viewer IS NOT BOUND.
      CREATE OBJECT gi_html_viewer TYPE zcl_abapgit_html_viewer_gui
        EXPORTING
          io_container           = io_container
          iv_disable_query_table = iv_disable_query_table.
    ENDIF.

    ri_viewer = gi_html_viewer.

  ENDMETHOD.

ENDCLASS.
