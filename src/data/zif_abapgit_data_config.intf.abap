INTERFACE zif_abapgit_data_config
  PUBLIC .


  TYPES:
    ty_data_type TYPE c LENGTH 4 .
  TYPES:
    BEGIN OF ty_config,
      type         TYPE ty_data_type,
      name         TYPE tadir-obj_name,
      skip_initial TYPE abap_bool,
      where        TYPE string_table,
    END OF ty_config .
  TYPES:
    ty_config_tt TYPE SORTED TABLE OF ty_config WITH UNIQUE KEY type name .

  CONSTANTS c_default_path TYPE string VALUE '/data/' ##NO_TEXT.
  CONSTANTS c_default_format TYPE string VALUE 'json' ##NO_TEXT.
  CONSTANTS c_config TYPE string VALUE 'conf' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF c_data_type,
      tabu TYPE ty_data_type VALUE 'TABU',
      vdat TYPE ty_data_type VALUE 'VDAT',
      cdat TYPE ty_data_type VALUE 'CDAT',
      tdat TYPE ty_data_type VALUE 'TDAT',
    END OF c_data_type .

  METHODS add_config
    IMPORTING
      !is_config TYPE ty_config
    RAISING
      zcx_abapgit_exception .
  METHODS from_json
    IMPORTING
      !it_files TYPE zif_abapgit_git_definitions=>ty_files_tt
    RAISING
      zcx_abapgit_exception .
  METHODS get_configs
    RETURNING
      VALUE(rt_configs) TYPE ty_config_tt .
  METHODS remove_config
    IMPORTING
      !is_config TYPE ty_config
    RAISING
      zcx_abapgit_exception .
  METHODS to_json
    RETURNING
      VALUE(rt_files) TYPE zif_abapgit_git_definitions=>ty_files_tt
    RAISING
      zcx_abapgit_exception .
  METHODS update_config
    IMPORTING
      !is_config TYPE ty_config
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
