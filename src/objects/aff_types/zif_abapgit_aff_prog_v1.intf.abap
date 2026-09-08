INTERFACE zif_abapgit_aff_prog_v1 PUBLIC.

  TYPES ty_program_type TYPE c LENGTH 1.

  CONSTANTS:
    BEGIN OF co_program_type,
      executable_program TYPE ty_program_type VALUE '1',
      module_pool        TYPE ty_program_type VALUE 'M',
      subroutine_pool    TYPE ty_program_type VALUE 'S',
      include            TYPE ty_program_type VALUE 'I',
    END OF co_program_type.

  TYPES ty_program_status TYPE c LENGTH 1.

  CONSTANTS:
    BEGIN OF co_program_status,
      sap_production_program      TYPE ty_program_status VALUE 'P',
      customer_production_program TYPE ty_program_status VALUE 'K',
      system_program              TYPE ty_program_status VALUE 'S',
      test_program                TYPE ty_program_status VALUE 'T',
      unknown                     TYPE ty_program_status VALUE '',
    END OF co_program_status.

  TYPES:
    BEGIN OF ty_logical_database,
      name             TYPE c LENGTH 20,
      selection_screen TYPE c LENGTH 3,
    END OF ty_logical_database.

  TYPES:
    BEGIN OF ty_general_information,
      program_type         TYPE ty_program_type,
      program_status       TYPE ty_program_status,
      fix_point_arithmetic TYPE abap_bool,
      edit_locked          TYPE abap_bool,
      starts_using_variant TYPE abap_bool,
      authorization_group  TYPE c LENGTH 8,
      application          TYPE c LENGTH 1,
    END OF ty_general_information.

  TYPES:
    BEGIN OF ty_main,
      format_version      TYPE zif_abapgit_aff_types_v1=>ty_format_version,
      header              TYPE zif_abapgit_aff_types_v1=>ty_header_70_no_abap_lv,
      general_information TYPE ty_general_information,
      logical_database    TYPE ty_logical_database,
    END OF ty_main.

ENDINTERFACE.
