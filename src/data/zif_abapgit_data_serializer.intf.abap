INTERFACE zif_abapgit_data_serializer
  PUBLIC .

  TYPES: ty_data_type TYPE c LENGTH 4.

  CONSTANTS: BEGIN OF c_data_type,
               tabu TYPE ty_data_type VALUE 'TABU',
               vdat TYPE ty_data_type VALUE 'VDAT',
               cdat TYPE ty_data_type VALUE 'CDAT',
               tdat TYPE ty_data_type VALUE 'TDAT',
             END OF c_data_type.

  TYPES: BEGIN OF ty_config,
           type  TYPE ty_data_type,
           name  TYPE tadir-obj_name,
           where TYPE string_table,
         END OF ty_config.

  TYPES: ty_config_tt TYPE STANDARD TABLE OF ty_config WITH DEFAULT KEY.

  METHODS serialize
    IMPORTING
      !it_config      TYPE ty_config_tt
    RETURNING
      VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_tt .

ENDINTERFACE.
