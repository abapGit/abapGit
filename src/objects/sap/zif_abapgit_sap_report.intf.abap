INTERFACE zif_abapgit_sap_report
  PUBLIC.

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
      is_item           TYPE zif_abapgit_definitions=>ty_item OPTIONAL
    RETURNING
      VALUE(rv_updated) TYPE abap_bool
    RAISING
      zcx_abapgit_exception.

  METHODS delete_report
    IMPORTING
      iv_name        TYPE syrepid
      iv_raise_error TYPE abap_bool DEFAULT abap_false
      is_item        TYPE zif_abapgit_definitions=>ty_item OPTIONAL
    RAISING
      zcx_abapgit_exception.

  METHODS read_progdir
    IMPORTING
      iv_name           TYPE syrepid
      iv_state          TYPE r3state DEFAULT 'A'
    RETURNING
      VALUE(rs_progdir) TYPE zcl_abapgit_objects_program=>ty_progdir
    RAISING
      zcx_abapgit_exception.

  METHODS update_progdir
    IMPORTING
      is_progdir TYPE zcl_abapgit_objects_program=>ty_progdir
      iv_package TYPE devclass
      iv_state   TYPE r3state DEFAULT 'I'
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
