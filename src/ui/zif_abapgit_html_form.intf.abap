INTERFACE zif_abapgit_html_form
  PUBLIC .

  TYPES:
    BEGIN OF ty_subitem,
      label    TYPE string,
      value    TYPE string,
      readonly TYPE abap_bool,
    END OF ty_subitem .
  TYPES:
    ty_subitems TYPE STANDARD TABLE OF ty_subitem WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_field,
      type          TYPE i,
      name          TYPE string,
      label         TYPE string,
      hint          TYPE string,
      dblclick      TYPE string,
      click         TYPE string,
      placeholder   TYPE string,
      required      TYPE string,
      upper_case    TYPE abap_bool,
      item_class    TYPE string,
      error         TYPE string,
      default_value TYPE string,
      side_action   TYPE string,
      subitems      TYPE ty_subitems,
      readonly      TYPE abap_bool,
      password      TYPE abap_bool,
      condense      TYPE abap_bool,
      min           TYPE i,
      max           TYPE i,
      rows          TYPE i,
      cols          TYPE i,
    END OF ty_field .
  TYPES:
    ty_fields TYPE STANDARD TABLE OF ty_field
          WITH DEFAULT KEY
          WITH UNIQUE SORTED KEY by_name COMPONENTS name .
  TYPES:
    BEGIN OF ty_command,
      label    TYPE string,
      action   TYPE string,
      cmd_type TYPE i,
    END OF ty_command .

  CONSTANTS c_rows TYPE string VALUE 'rows'.
  CONSTANTS:
    BEGIN OF c_cmd_type,
      input      TYPE i VALUE 1,
      input_main TYPE i VALUE 2,
      link       TYPE i VALUE 3,
      button     TYPE i VALUE 4,
    END OF c_cmd_type .
  CONSTANTS:
    BEGIN OF c_field_type,
      text        TYPE i VALUE 1,
      radio       TYPE i VALUE 2,
      checkbox    TYPE i VALUE 3,
      field_group TYPE i VALUE 4,
      number      TYPE i VALUE 5,
      textarea    TYPE i VALUE 6,
      table       TYPE i VALUE 7,
      hidden      TYPE i VALUE 8,
    END OF c_field_type .

ENDINTERFACE.
