CLASS lcl_table_scheme DEFINITION FINAL.
  " TODO: move to a global class, when table is separated as a component
  PUBLIC SECTION.
    DATA mt_col_spec TYPE zif_abapgit_definitions=>ty_col_spec_tt READ-ONLY.

    METHODS add_column
      IMPORTING
        iv_tech_name      TYPE string OPTIONAL
        iv_display_name   TYPE string OPTIONAL
        iv_css_class      TYPE string OPTIONAL
        iv_add_tz         TYPE abap_bool OPTIONAL
        iv_title          TYPE string OPTIONAL
        iv_allow_order_by TYPE any OPTIONAL
      RETURNING
        VALUE(ro_me) TYPE REF TO lcl_table_scheme.

ENDCLASS.

CLASS lcl_table_scheme IMPLEMENTATION.

  METHOD add_column.

    FIELD-SYMBOLS <ls_col> LIKE LINE OF mt_col_spec.
    APPEND INITIAL LINE TO mt_col_spec ASSIGNING <ls_col>.
    <ls_col>-display_name   = iv_display_name.
    <ls_col>-tech_name      = iv_tech_name.
    <ls_col>-title          = iv_title.
    <ls_col>-css_class      = iv_css_class.
    <ls_col>-add_tz         = iv_add_tz.
    <ls_col>-allow_order_by = iv_allow_order_by.

    ro_me = me.

  ENDMETHOD.

ENDCLASS.
