INTERFACE zif_abapgit_dot_abapgit PUBLIC.

  TYPES:
    BEGIN OF ty_requirement,
      component   TYPE dlvunit,
      min_release TYPE saprelease,
      min_patch   TYPE sappatchlv,
    END OF ty_requirement .
  TYPES:
    ty_requirement_tt TYPE STANDARD TABLE OF ty_requirement WITH DEFAULT KEY .

  " Former APACK
  TYPES:
    BEGIN OF ty_dependency,
      name           TYPE string,
      version        TYPE string,
      sem_version    TYPE zif_abapgit_definitions=>ty_version,
      git_url        TYPE string,
      target_package TYPE devclass,
    END OF ty_dependency,

    ty_dependencies TYPE STANDARD TABLE OF ty_dependency WITH NON-UNIQUE DEFAULT KEY,

    BEGIN OF ty_descriptor,
      name              TYPE string,
      version           TYPE string,
      sem_version       TYPE zif_abapgit_definitions=>ty_version,
      description       TYPE string,
      git_url           TYPE string,
      target_package    TYPE devclass,
    END OF ty_descriptor,

    BEGIN OF ty_packaging.
      INCLUDE TYPE ty_descriptor.
    TYPES:
      dependencies TYPE ty_dependencies,
    END OF ty_packaging.

  TYPES:
    BEGIN OF ty_dot_abapgit,
      master_language TYPE spras,
      starting_folder TYPE string,
      folder_logic    TYPE string,
      ignore          TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      requirements    TYPE ty_requirement_tt,
      packaging       TYPE ty_packaging,
    END OF ty_dot_abapgit .

  CONSTANTS:
    BEGIN OF c_folder_logic,
      prefix TYPE string VALUE 'PREFIX',
      full   TYPE string VALUE 'FULL',
    END OF c_folder_logic .

ENDINTERFACE.
