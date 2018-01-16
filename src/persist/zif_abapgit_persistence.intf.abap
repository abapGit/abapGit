INTERFACE zif_abapgit_persistence PUBLIC.

  TYPES:
    ty_type  TYPE c LENGTH 12 .
  TYPES:
    ty_value TYPE c LENGTH 12 .
  TYPES:
    BEGIN OF ty_content,
      type     TYPE ty_type,
      value    TYPE ty_value,
      data_str TYPE string,
    END OF ty_content .
  TYPES:
    tt_content TYPE SORTED TABLE OF ty_content WITH UNIQUE KEY type value .

ENDINTERFACE.
