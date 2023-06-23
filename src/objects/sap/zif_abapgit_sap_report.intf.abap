INTERFACE zif_abapgit_sap_report
  PUBLIC.

  TYPES:
    ty_abap_language_version TYPE c LENGTH 1.

  METHODS read_report
    IMPORTING
      iv_name          TYPE syrepid
      iv_state         TYPE r3state OPTIONAL
      is_item          TYPE zif_abapgit_definitions=>ty_item OPTIONAL
    RETURNING
      VALUE(rt_source) TYPE abaptxt255_tab
    RAISING
      zcx_abapgit_exception.

  METHODS insert_report
    IMPORTING
      iv_name           TYPE syrepid
      it_source         TYPE STANDARD TABLE
      iv_state          TYPE r3state OPTIONAL
      iv_program_type   TYPE c OPTIONAL
      iv_extension_type TYPE c OPTIONAL
      iv_package        TYPE devclass
      iv_version        TYPE ty_abap_language_version OPTIONAL
      is_item           TYPE zif_abapgit_definitions=>ty_item OPTIONAL
    RAISING
      zcx_abapgit_exception.

  METHODS update_report
    IMPORTING
      iv_name           TYPE syrepid
      it_source         TYPE STANDARD TABLE
      iv_state          TYPE r3state OPTIONAL
      iv_program_type   TYPE c OPTIONAL
      iv_extension_type TYPE c OPTIONAL
      iv_package        TYPE devclass
      iv_version        TYPE ty_abap_language_version OPTIONAL
      is_item           TYPE zif_abapgit_definitions=>ty_item OPTIONAL
    RETURNING
      VALUE(rv_updated) TYPE abap_bool
    RAISING
      zcx_abapgit_exception.

  METHODS delete_report
    IMPORTING
      iv_name        TYPE syrepid
      iv_raise_error TYPE abap_bool DEFAULT abap_false
      iv_version     TYPE ty_abap_language_version OPTIONAL
      is_item        TYPE zif_abapgit_definitions=>ty_item OPTIONAL
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
