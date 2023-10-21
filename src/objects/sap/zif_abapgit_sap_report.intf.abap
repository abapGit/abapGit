INTERFACE zif_abapgit_sap_report
  PUBLIC.

  TYPES:
    BEGIN OF ty_progdir,
      name    TYPE progdir-name,
      state   TYPE progdir-state,
      sqlx    TYPE progdir-sqlx,
      edtx    TYPE progdir-edtx,
      varcl   TYPE progdir-varcl,
      dbapl   TYPE progdir-dbapl,
      dbna    TYPE progdir-dbna,
      clas    TYPE progdir-clas,
      type    TYPE progdir-type,
      occurs  TYPE progdir-occurs,
      subc    TYPE progdir-subc,
      appl    TYPE progdir-appl,
      secu    TYPE progdir-secu,
      cnam    TYPE progdir-cnam,
      cdat    TYPE progdir-cdat,
      unam    TYPE progdir-unam,
      udat    TYPE progdir-udat,
      vern    TYPE progdir-vern,
      levl    TYPE progdir-levl,
      rstat   TYPE progdir-rstat,
      rmand   TYPE progdir-rmand,
      rload   TYPE progdir-rload,
      fixpt   TYPE progdir-fixpt,
      sset    TYPE progdir-sset,
      sdate   TYPE progdir-sdate,
      stime   TYPE progdir-stime,
      idate   TYPE progdir-idate,
      itime   TYPE progdir-itime,
      ldbname TYPE progdir-ldbname,
      uccheck TYPE progdir-uccheck,
    END OF ty_progdir.

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
      iv_version        TYPE uccheck
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
      iv_version        TYPE uccheck
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
      VALUE(rs_progdir) TYPE ty_progdir
    RAISING
      zcx_abapgit_exception.

  METHODS update_progdir
    IMPORTING
      is_progdir TYPE ty_progdir
      iv_package TYPE devclass
      iv_state   TYPE r3state DEFAULT 'I'
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
