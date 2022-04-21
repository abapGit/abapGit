INTERFACE zif_abapgit_field_rules
  PUBLIC .
  TYPES ty_fill_rule TYPE c LENGTH 2.
  CONSTANTS:
    BEGIN OF c_fill_rule,
      date      TYPE ty_fill_rule VALUE 'DT',
      time      TYPE ty_fill_rule VALUE 'TM',
      timestamp TYPE ty_fill_rule VALUE 'TS',
      user      TYPE ty_fill_rule VALUE 'UR',
      client    TYPE ty_fill_rule VALUE 'CT',
    END OF c_fill_rule.
  TYPES:
    BEGIN OF ty_item,
      tabname     TYPE tabname,
      fieldname   TYPE fieldname,
      clear_field TYPE abap_bool,
      fill_rule   TYPE ty_fill_rule,
    END OF ty_item,
    ty_items TYPE SORTED TABLE OF ty_item WITH UNIQUE KEY tabname fieldname.

  METHODS add_item
    IMPORTING
      is_item TYPE ty_item.
  METHODS add_items
    IMPORTING
      it_item TYPE ty_items.
  METHODS apply_clear_logic
    IMPORTING
      iv_table TYPE tabname
    CHANGING
      ct_data  TYPE STANDARD TABLE.
  METHODS apply_fill_logic
    IMPORTING
      iv_table TYPE tabname
    CHANGING
      ct_data  TYPE STANDARD TABLE.
ENDINTERFACE.
