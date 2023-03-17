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
      package   TYPE ty_fill_rule VALUE 'PK',
    END OF c_fill_rule.

  METHODS add
    IMPORTING
      iv_table       TYPE tabname
      iv_field       TYPE fieldname
      iv_fill_rule   TYPE ty_fill_rule
    RETURNING
      VALUE(ro_self) TYPE REF TO zif_abapgit_field_rules.
  METHODS apply_clear_logic
    IMPORTING
      iv_table TYPE tabname
    CHANGING
      ct_data  TYPE STANDARD TABLE.
  METHODS apply_fill_logic
    IMPORTING
      iv_table   TYPE tabname
      iv_package TYPE devclass
    CHANGING
      ct_data    TYPE STANDARD TABLE.
ENDINTERFACE.
