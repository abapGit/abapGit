INTERFACE zif_abapgit_gui_diff_extra PUBLIC.
* only diff rendering, not page related stuff

  METHODS insert_nav
    RETURNING
      VALUE(rv_insert_nav) TYPE abap_bool .

ENDINTERFACE.
