INTERFACE zif_abapgit_html PUBLIC.

  CONSTANTS:
    BEGIN OF c_action_type,
      sapevent  TYPE c VALUE 'E',
      url       TYPE c VALUE 'U',
      onclick   TYPE c VALUE 'C',
      separator TYPE c VALUE 'S',
      dummy     TYPE c VALUE '_',
    END OF c_action_type .
  CONSTANTS:
    BEGIN OF c_html_opt,
      strong   TYPE c VALUE 'E',
      cancel   TYPE c VALUE 'C',
      crossout TYPE c VALUE 'X',
    END OF c_html_opt .

  METHODS add
    IMPORTING
      !ig_chunk TYPE any .
  METHODS render
    IMPORTING
      !iv_no_indent_jscss TYPE abap_bool OPTIONAL
    RETURNING
      VALUE(rv_html)      TYPE string .
  METHODS is_empty
    RETURNING
      VALUE(rv_yes) TYPE abap_bool .
  METHODS add_a
    IMPORTING
      !iv_txt   TYPE string
      !iv_act   TYPE string
      !iv_typ   TYPE char1 DEFAULT c_action_type-sapevent
      !iv_opt   TYPE clike OPTIONAL
      !iv_class TYPE string OPTIONAL
      !iv_id    TYPE string OPTIONAL
      !iv_style TYPE string OPTIONAL
      !iv_title TYPE string OPTIONAL.
  METHODS add_checkbox
    IMPORTING
      iv_id TYPE string.
  CLASS-METHODS a
    IMPORTING
      !iv_txt       TYPE string
      !iv_act       TYPE string
      !iv_typ       TYPE char1 DEFAULT zif_abapgit_html=>c_action_type-sapevent
      !iv_opt       TYPE clike OPTIONAL
      !iv_class     TYPE string OPTIONAL
      !iv_id        TYPE string OPTIONAL
      !iv_style     TYPE string OPTIONAL
      !iv_title     TYPE string OPTIONAL
    RETURNING
      VALUE(rv_str) TYPE string .
  CLASS-METHODS icon
    IMPORTING
      !iv_name      TYPE string
      !iv_hint      TYPE string OPTIONAL
      !iv_class     TYPE string OPTIONAL
    RETURNING
      VALUE(rv_str) TYPE string .

ENDINTERFACE.
