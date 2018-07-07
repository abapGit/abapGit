INTERFACE zif_abapgit_dot_abapgit PUBLIC.

  TYPES:
    BEGIN OF ty_requirement,
      component   TYPE dlvunit,
      min_release TYPE saprelease,
      min_patch   TYPE sappatchlv,
    END OF ty_requirement .
  TYPES:
    ty_requirement_tt TYPE STANDARD TABLE OF ty_requirement WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_dot_abapgit,
      master_language              TYPE spras,
      starting_folder              TYPE string,
      folder_logic                 TYPE string,
      ignore                       TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      requirements                 TYPE ty_requirement_tt,
    END OF ty_dot_abapgit .

  CONSTANTS:
    BEGIN OF c_folder_logic,
      prefix TYPE string VALUE 'PREFIX',
      full   TYPE string VALUE 'FULL',
    END OF c_folder_logic .

ENDINTERFACE.
