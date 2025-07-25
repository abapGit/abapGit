INTERFACE zif_abapgit_dot_abapgit PUBLIC.

  TYPES:
    BEGIN OF ty_requirement,
      component   TYPE tdevc-dlvunit,
      min_release TYPE saprelease,
      min_patch   TYPE sappatchlv,
    END OF ty_requirement .
  TYPES:
    ty_requirement_tt TYPE STANDARD TABLE OF ty_requirement WITH DEFAULT KEY .

  TYPES:
    BEGIN OF ty_dot_abapgit,
      name                  TYPE string,
      master_language       TYPE spras,
      i18n_languages        TYPE zif_abapgit_definitions=>ty_languages,
      use_lxe               TYPE abap_bool,
      without_translation   TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      starting_folder       TYPE string,
      folder_logic          TYPE string,
      ignore                TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      requirements          TYPE ty_requirement_tt,
      version_constant      TYPE string,
      abap_language_version TYPE string,
      original_system       TYPE tadir-srcsystem,
    END OF ty_dot_abapgit .

  CONSTANTS:
    BEGIN OF c_folder_logic,
      prefix TYPE string VALUE 'PREFIX',
      full   TYPE string VALUE 'FULL',
      mixed  TYPE string VALUE 'MIXED',
    END OF c_folder_logic,

    BEGIN OF c_abap_language_version,
      standard          TYPE string VALUE 'standard',
      key_user          TYPE string VALUE 'keyUser',
      cloud_development TYPE string VALUE 'cloudDevelopment',
      ignore            TYPE string VALUE 'ignore',
      undefined         TYPE string VALUE 'undefined', " any
    END OF c_abap_language_version.

ENDINTERFACE.
