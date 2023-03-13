INTERFACE zif_abapgit_xml_input
  PUBLIC .
  METHODS read
    IMPORTING
      !iv_name TYPE clike
    CHANGING
      !cg_data TYPE any
    RAISING
      zcx_abapgit_exception .
  METHODS get_raw
    RETURNING
      VALUE(ri_raw) TYPE REF TO if_ixml_document .
  METHODS i18n_params
    IMPORTING
      !is_i18n_params       TYPE zif_abapgit_definitions=>ty_i18n_params OPTIONAL
    RETURNING
      VALUE(rs_i18n_params) TYPE zif_abapgit_definitions=>ty_i18n_params .

* todo, add read_xml to match add_xml in lcl_xml_output
  METHODS get_metadata
    RETURNING
      VALUE(rs_metadata) TYPE zif_abapgit_definitions=>ty_metadata .
ENDINTERFACE.
