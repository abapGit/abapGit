INTERFACE zif_abapgit_flow_exit
  PUBLIC .


  METHODS toolbar_extras
    IMPORTING
      !io_toolbar TYPE REF TO zcl_abapgit_html_toolbar
      !is_feature TYPE zif_abapgit_gui_page_flow=>ty_feature
      !iv_index   TYPE i .
ENDINTERFACE.
