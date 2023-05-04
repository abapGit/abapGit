INTERFACE zif_abapgit_gui_page_callback
  PUBLIC .


  METHODS row_selected
    IMPORTING
      !it_table TYPE STANDARD TABLE
      iv_index  TYPE i
    RAISING
      zcx_abapgit_exception.
ENDINTERFACE.
